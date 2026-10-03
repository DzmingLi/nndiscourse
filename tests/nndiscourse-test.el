;;; nndiscourse-test.el --- Native backend tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'nndiscourse)

(ert-deftest nndiscourse-test-gnus-search-finds-cached-replies ()
  (let* ((record '(:name "latest" :posts
                   ((:number 2 :title "Org guide" :author "Ada"
                     :body "<p>Outline basics</p>")
                    (:number 7 :title "Org guide" :author "Bob"
                     :body "<p>Nested reply</p>"))))
         (db (make-nndiscourse--db :groups (list record)))
         (engine (make-instance 'gnus-search-nndiscourse)))
    (cl-letf (((symbol-function 'gnus-server-to-method)
               (lambda (_) '(nndiscourse "example.org")))
              ((symbol-function 'nndiscourse--select) (lambda (_) db)))
      (should (equal (gnus-search-run-search
                      engine "nndiscourse:example.org" '((query . "nested reply"))
                      '("nndiscourse+example.org:latest"))
                     [["nndiscourse+example.org:latest" 7 100]]))
      (should (equal (gnus-search-run-search
                      engine "nndiscourse:example.org" '((query . "missing"))
                      '("nndiscourse+example.org:latest"))
                     [])))))
(defmacro nndiscourse-test-with-db (&rest body)
  (declare (indent 0))
  `(let* ((directory (make-temp-file "nndiscourse-test-" t))
          (db (nndiscourse--load "https://example.org" (expand-file-name "state.json" directory)))
          (nndiscourse--state db)
          (auth-sources nil)
          (discourse-auth--credentials (make-hash-table :test #'equal)))
     (unwind-protect (progn ,@body) (delete-directory directory t))))
(defun nndiscourse-test-post (id floor &optional parent)
  (list :id id :post_number floor :topic_id 42 :reply_to_post_number parent
        :username "someone" :created_at "2026-10-01T12:00:00Z" :cooked "<p>正文</p>"))
(defun nndiscourse-test-topic ()
  (list :id 42 :title "Example" :post_stream
        (list :stream '(100 110 140) :posts (list (nndiscourse-test-post 100 1)))))
(defun nndiscourse-test-latest (&optional reverse)
  (list :users '((:id 7 :username "original") (:id 8 :username "another"))
        :topic_list
        (list :topics
              (if reverse
                  '((:id 99 :title "Other" :created_at "2026-09-30T00:00:00Z"
                     :last_posted_at "2026-10-02T00:00:00Z"
                     :posters ((:user_id 8)))
                    (:id 42 :title "Example" :created_at "2026-10-01T00:00:00Z"
                     :last_posted_at "2026-10-03T00:00:00Z"
                     :posters ((:user_id 7))))
                '((:id 42 :title "Example" :created_at "2026-10-01T00:00:00Z"
                   :last_posted_at "2026-10-01T12:00:00Z"
                   :posters ((:user_id 7)))
                  (:id 99 :title "Other" :created_at "2026-09-30T00:00:00Z"
                   :last_posted_at "2026-10-02T00:00:00Z"
                   :posters ((:user_id 8))))))))

(ert-deftest nndiscourse-test-latest-roots-and-lazy-replies ()
  (nndiscourse-test-with-db
    (let* ((group (nndiscourse--group db "latest" t))
           (first (nndiscourse--latest-entries group (nndiscourse-test-latest)))
           (root (nndiscourse--post db first 1))
           (root-id (nndiscourse--post-message-id db first root)))
      (should (= 2 (plist-get first :high)))
      (should (equal "original" (plist-get root :author)))
      (should-not (plist-get root :body))
      (should (equal "<topic.42.1." (substring root-id 0 12)))
      (nndiscourse--replace-group db group first)
      (cl-letf (((symbol-function 'nndiscourse--http)
                 (lambda (_db method path _fields cb)
                   (should (eq method 'get))
                   (should (equal path "/t/42.json"))
                   (funcall cb (nndiscourse-test-topic) nil nil))))
        (with-temp-buffer
          (should (equal '("latest" . 1)
                         (nndiscourse-request-article 1 "latest" nil (current-buffer))))
          (should (string-match-p "Archived-at: <https://example.org/t/42/1>"
                                  (buffer-string)))))
      (should (= 2 (length (plist-get (nndiscourse--group db "latest") :posts))))
      (should (equal "<p>正文</p>"
                     (plist-get (nndiscourse--post db (nndiscourse--group db "latest") 1)
                                :body)))
      (cl-letf (((symbol-function 'nndiscourse--fetch-topic)
                 (lambda (_db topic cb)
                   (should (= topic 42))
                   (funcall cb "Example"
                            (list (nndiscourse-test-post 100 1)
                                  (nndiscourse-test-post 110 3 1)
                                  (nndiscourse-test-post 140 7 3)) nil))))
        (let* ((record (nndiscourse--group db "latest"))
               (headers (nndiscourse-request-thread
                         (nndiscourse--header db record (nndiscourse--post db record 1))
                         "latest")))
          (should (= 3 (length headers)))
          (should (equal root-id (mail-header-id (car headers))))
          (should (equal (mail-header-id (nth 1 headers))
                         (mail-header-references (nth 2 headers))))))
      (let* ((record (nndiscourse--group db "latest"))
             (posts (plist-get record :posts))
             (other (cl-find 99 posts :key (lambda (p) (plist-get p :topic-id)))))
        (should (= 4 (plist-get record :high)))
        (should (= 2 (plist-get other :number)))
        (should (= 3 (length (cl-remove-if-not
                              (lambda (p) (equal 42 (plist-get p :topic-id))) posts))))
        (let* ((refreshed (nndiscourse--latest-entries record (nndiscourse-test-latest t)))
               (same-root (nndiscourse--post db refreshed 1)))
          (should (= 4 (plist-get refreshed :high)))
          (should (equal root-id (nndiscourse--post-message-id db refreshed same-root)))
          (should (equal "2026-10-03T00:00:00Z" (plist-get same-root :time))))))))

(ert-deftest nndiscourse-test-latest-rejects-bad-list-without-changing-cache ()
  (nndiscourse-test-with-db
    (let* ((record (nndiscourse--group db "latest" t))
           (snapshot (nndiscourse--latest-entries record (nndiscourse-test-latest)))
           failure)
      (nndiscourse--replace-group db record snapshot)
      (cl-letf (((symbol-function 'nndiscourse--http)
                 (lambda (_db _method path _fields cb)
                   (should (equal path "/latest.json"))
                   (funcall cb '(:topic_list (:topics ((:id 0 :title "Bad")))) nil nil))))
        (nndiscourse-update "latest" nil (lambda (error) (setq failure error))))
      (should failure)
      (should (equal snapshot (nndiscourse--group db "latest")))
      (should-not (gethash "latest" (nndiscourse--db-busy db))))))

(ert-deftest nndiscourse-test-category-list-uses-category-endpoint ()
  (nndiscourse-test-with-db
    (let* ((name "category.8.org-mode")
           (record (nndiscourse--group db name t)))
      (should (nndiscourse--list-group-p name))
      (should (equal "/c/org-mode/8.json" (nndiscourse--list-path name)))
      (cl-letf (((symbol-function 'nndiscourse--http)
                 (lambda (_db method path _fields cb)
                   (should (eq method 'get))
                   (should (equal path "/c/org-mode/8.json"))
                   (funcall cb (nndiscourse-test-latest) nil nil))))
        (let (failure)
          (nndiscourse-update name nil (lambda (error) (setq failure error)))
          (should-not failure)))
      (setq record (nndiscourse--group db name))
      (should (= 2 (plist-get record :high)))
      (should (equal "<topic.42.1."
                     (substring (nndiscourse--post-message-id
                                 db record (nndiscourse--post db record 1)) 0 12))))))

(ert-deftest nndiscourse-test-notifications-collapse-per-topic-and-renew-unread-number ()
  (nndiscourse-test-with-db
    (let* ((record (nndiscourse--group db "notifications" t))
           (reply '(:id 10 :notification_type 2 :topic_id 42 :post_number 3
                    :created_at "2026-10-01T12:00:00Z"
                    :data (:topic_title "Example" :username "alice")))
           (mention '(:id 11 :notification_type 1 :topic_id 42 :post_number 7
                      :created_at "2026-10-02T12:00:00Z"
                      :data (:topic_title "Example" :username "bob")))
           (first (nndiscourse--notification-entries
                   db record (list :notifications (list reply mention))))
           (root (car (plist-get first :posts))))
      (should (= 1 (length (plist-get first :posts))))
      (should (equal '(3) (plist-get root :alert-floors)))
      (should (equal "Example" (plist-get root :title)))
      (should (string-match-p "Reply #3" (plist-get root :body)))
      (should-not (string-match-p "#7" (plist-get root :body)))
      (should (= 1 (plist-get root :number)))
      (let* ((same (nndiscourse--notification-entries
                    db first (list :notifications (list mention reply))))
             (new (nndiscourse--notification-entries
                   db first
                   (list :notifications
                         (cons '(:id 12 :notification_type 2 :topic_id 42
                                 :post_number 9 :created_at "2026-10-03T12:00:00Z"
                                 :data (:topic_title "Example" :username "charlie"))
                               (list mention reply))))))
        (should (= 1 (plist-get (car (plist-get same :posts)) :number)))
        (should (= 2 (plist-get (car (plist-get new :posts)) :number)))
        (should (= 1 (length (plist-get new :posts))))))))
(ert-deftest nndiscourse-test-url-identity ()
  (dolist (url '("https://example.org/t/42" "https://example.org/t/42/7"
                 "https://example.org/t/slug/42/7?x=1" "https://example.org/t/slug/42.json"))
    (should (equal '("https://example.org" 42) (nndiscourse--location url))))
  (should (equal '("https://example.org:8443/forum" 42)
                 (nndiscourse--location "https://EXAMPLE.org:8443/forum/t/slug/42")))
  (dolist (url '("http://example.org/t/42" "https://user@example.org/t/42" "https://example.org/t/0"
                 "https://example.org/t/42/7/9" "https://example.org/t/slug/42\r\nInjected"))
    (should-error (nndiscourse--location url))))
(ert-deftest nndiscourse-test-pagination-and-native-headers ()
  (nndiscourse-test-with-db
    (let ((calls 0) failure)
      (cl-letf (((symbol-function 'nndiscourse--http)
                 (lambda (_db _method path fields callback)
                   (cl-incf calls)
                   (funcall callback
                            (if (equal path "/t/42.json") (nndiscourse-test-topic)
                              (should (equal path "/t/42/posts.json"))
                              (should (equal '(("post_ids[]" "110") ("post_ids[]" "140")) fields))
                              (list :post_stream (list :posts (list (nndiscourse-test-post 110 3 1)
                                                                  (nndiscourse-test-post 140 7 3))))) nil nil))))
        (nndiscourse-update "topic.42" nil (lambda (err) (setq failure err))))
      (should-not failure) (should (= calls 2))
      (let* ((group (nndiscourse--group db "topic.42"))
             (posts (plist-get group :posts))
             (header (nndiscourse--header db group (car (last posts)))))
        (should (equal '(1 3 7) (mapcar (lambda (p) (plist-get p :number)) posts)))
        (should (equal (nndiscourse--message-id db group 3) (mail-header-references header)))
        (should (equal (nndiscourse--db-groups db)
                       (nndiscourse--db-groups (nndiscourse--load "https://example.org" (nndiscourse--db-file db)))))
        (should (= #o600 (logand #o777 (file-modes (nndiscourse--db-file db)))))))))
(ert-deftest nndiscourse-test-partial-fetch-does-not-replace-cache ()
  (nndiscourse-test-with-db
    (let* ((group (nndiscourse--group db "topic.42" t)) failure)
      (setf (plist-get group :title) "Old snapshot")
      (cl-letf (((symbol-function 'nndiscourse--http)
                 (lambda (_db _method path _fields cb)
                   (funcall cb (if (equal path "/t/42.json") (nndiscourse-test-topic)
                                 '(:post_stream (:posts nil))) nil nil))))
        (nndiscourse-update "topic.42" nil (lambda (err) (setq failure err))))
      (should failure)
      (should (equal "Old snapshot" (plist-get (nndiscourse--group db "topic.42") :title)))
      (should-not (gethash "topic.42" (nndiscourse--db-busy db))))))
(ert-deftest nndiscourse-test-deleted-parent-and-high-water ()
  (nndiscourse-test-with-db
    (let* ((group (nndiscourse--group db "topic.42" t))
           (_ (setf (plist-get group :high) 99))
           (snapshot (nndiscourse--normalize db group "Title" (list (nndiscourse-test-post 140 7 3))))
           (parent (nndiscourse--post db snapshot 3)))
      (should (plist-get parent :missing))
      (should (= 99 (plist-get snapshot :high)))
      (should-not (equal (nndiscourse--message-id db snapshot 3)
                         (nndiscourse--message-id (make-nndiscourse--db :base "https://elsewhere.org") snapshot 3)))
      (should-error (nndiscourse--normalize db group "Title" (list (nndiscourse-test-post 140 7 7)))))))
(ert-deftest nndiscourse-test-scan-only-subscribed-topics ()
  (nndiscourse-test-with-db
    (nndiscourse--group db "topic.42" t) (nndiscourse--group db "topic.99" t)
    (let (scanned)
      (cl-letf (((symbol-function 'nndiscourse--select) (lambda (&rest _) db))
                ((symbol-function 'nndiscourse--subscribed-p) (lambda (group _) (equal group "topic.42")))
                ((symbol-function 'nndiscourse-update)
                 (lambda (group _server cb) (push group scanned) (funcall cb nil))))
        (should (nndiscourse-request-scan nil "example.org")))
      (should (equal '("topic.42") scanned)))))
(ert-deftest nndiscourse-test-http-credentials-and-destinations ()
  (nndiscourse-test-with-db
    (let ((calls 0) failure uncertain)
      (cl-letf (((symbol-function 'plz)
                 (lambda (_method _url &rest args)
                   (cl-incf calls)
                   (should-not (assoc "Cookie" (plist-get args :headers)))
                   (should-not (member "--location" plz-curl-default-args))
                   (funcall (plist-get args :then) (make-plz-response :status 200 :body "{}")))))
        (nndiscourse--http db 'get "/t/42.json" nil #'ignore)
        (nndiscourse--http db 'post "/posts.json" nil (lambda (_ e u) (setq failure e uncertain u)))
        (should failure) (should-not uncertain)
        (nndiscourse--http db 'get "https://evil.example" nil #'ignore)
        (should (= calls 1))))))
(ert-deftest nndiscourse-test-native-article-mime ()
  (nndiscourse-test-with-db
    (let* ((group (nndiscourse--normalize db (nndiscourse--group db "topic.42" t) "Title\nInjected: no"
                                        (list (nndiscourse-test-post 100 1)))))
      (setf (nndiscourse--db-groups db) (list group))
      (with-temp-buffer
        (should (equal '("topic.42" . 1) (nndiscourse-request-article 1 "topic.42" nil (current-buffer))))
        (should (string-match-p "Content-Transfer-Encoding: base64" (buffer-string)))
        (should-not (string-match-p "\nInjected:" (buffer-string)))
        (goto-char (point-min)) (search-forward "\n\n")
        (should (equal "<p>正文</p>" (decode-coding-string (base64-decode-string (buffer-substring (point) (point-max))) 'utf-8)))))))

(ert-deftest nndiscourse-test-reply-post-and-uncertain-lock ()
  (nndiscourse-test-with-db
    (let* ((group (nndiscourse--normalize db (nndiscourse--group db "topic.42" t) "Topic"
                                        (list (nndiscourse-test-post 100 1)
                                              (nndiscourse-test-post 110 3 1))))
           (calls 0))
      (setf (nndiscourse--db-groups db) (list group))
      (cl-letf (((symbol-function 'nndiscourse--http)
                 (lambda (_db method path fields callback)
                   (cl-incf calls)
                   (should (eq method 'post)) (should (equal path "/posts.json"))
                   (should (equal "42" (cadr (assoc "topic_id" fields))))
                   (should (equal "3" (cadr (assoc "reply_to_post_number" fields))))
                   (should (equal "中文 & text" (cadr (assoc "raw" fields))))
                   ;; Verify the durable pending lock exists before dispatch.
                   (should (equal "pending" (plist-get (car (nndiscourse--db-attempts
                                                            (nndiscourse--load "https://example.org" (nndiscourse--db-file db)))) :state)))
                   (funcall callback nil "timeout" t))))
        (should-error (nndiscourse--submit db group 3 nil "中文 & text"))
        (should-error (nndiscourse--submit db group 3 nil "中文 & text"))
        (should (= calls 1)))
      (should (equal "pending" (plist-get (car (nndiscourse--db-attempts db)) :state))))))

(ert-deftest nndiscourse-test-confirmed-reply-and-rejected-post ()
  (nndiscourse-test-with-db
    (let* ((group (nndiscourse--normalize db (nndiscourse--group db "topic.42" t) "Topic"
                                        (list (nndiscourse-test-post 100 1))))
           reject)
      (setf (nndiscourse--db-groups db) (list group))
      (cl-letf (((symbol-function 'nndiscourse--http)
                 (lambda (_db _method _path _fields callback)
                   (if reject (funcall callback nil "HTTP 422" nil)
                     (funcall callback (nndiscourse-test-post 200 2 1) nil nil)))))
        (should (equal 2 (plist-get (nndiscourse--submit db group 1 nil "first") :post_number)))
        (should (equal "sent" (plist-get (car (nndiscourse--db-attempts db)) :state)))
        (should-error (nndiscourse--submit db group 1 nil "first"))
        (setq reject t)
        (should-error (nndiscourse--submit db group 1 nil "second"))
        (should (equal "failed" (plist-get (car (nndiscourse--db-attempts db)) :state)))))))

(ert-deftest nndiscourse-test-new-topic-category-and-draft ()
  (nndiscourse-test-with-db
    (let ((posted nil))
      (cl-letf (((symbol-function 'nndiscourse--http)
                 (lambda (_db method path fields cb)
                   (pcase (list method path)
                     (`(get "/categories.json")
                      (funcall cb '(:category_list (:categories ((:id 5 :name "General")
                                                                (:id 8 :name "Help" :parent_category_id 5)))) nil nil))
                     (`(post "/posts.json")
                      (setq posted fields)
                      (funcall cb '(:id 201 :topic_id 99 :post_number 1) nil nil))))))
        (should (equal '(("General [5]" . 5) ("General / Help [8]" . 8))
                       (nndiscourse--categories db)))
        (should (equal 99 (plist-get (nndiscourse--submit db nil nil "Hello" "Body" 8) :topic_id)))
        (should (equal "8" (cadr (assoc "category" posted))))
        (should (equal "Hello" (cadr (assoc "title" posted))))))))

(ert-deftest nndiscourse-test-composer-rejects-attachments ()
  (with-temp-buffer
    (nndiscourse-compose-topic-mode)
    (insert "Subject: Example\n" mail-header-separator "\nBody text")
    (should (equal "Body text" (nndiscourse--body)))
    (goto-char (point-max)) (insert "\n<#part type=application/octet-stream>")
    (should-error (nndiscourse--body))))
