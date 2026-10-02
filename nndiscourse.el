;;; nndiscourse.el --- Discourse topics and latest lists in Gnus -*- lexical-binding: t; -*-
;; Copyright (C) 2019 The Authors of nndiscourse.el
;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.3.0
;; Keywords: news, comm
;; URL: https://github.com/DzmingLi/nndiscourse
;; Package-Requires: ((emacs "29.1") (plz "0.9.1"))
;;; Commentary:
;; Fork of dickmao/nndiscourse.  A topic group contains native floor articles;
;; a site's latest group imports roots and retrieves replies on demand.
;; HTTP and User API authorization run in Emacs.  Gnus owns article marks.
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'discourse-auth)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-sum)
(require 'gnus-start)
(require 'gnus-msg)
(require 'nnoo)
(require 'nnheader)
(require 'rfc2047)
(require 'mail-parse)
(require 'message)
(require 'mm-decode)

(defgroup nndiscourse nil "Discourse topics and latest lists." :group 'gnus)
(nnoo-declare nndiscourse)
(defvoo nndiscourse-address nil "HTTPS site base, including an optional mount path.")
(defvoo nndiscourse-directory (expand-file-name "nndiscourse-topics/" gnus-directory)
  "Directory for private local topic snapshots.")
(defvoo nndiscourse-status-string "")
(defvoo nndiscourse--state nil)
(nnoo-define-basics nndiscourse)
(defcustom nndiscourse-timeout 30 "HTTP request timeout in seconds." :type 'number)
(defcustom nndiscourse-scan-timeout 300 "Maximum synchronous Gnus scan wait in seconds." :type 'number)
(cl-defstruct nndiscourse--db base file groups attempts busy)
(defvar nndiscourse--databases (make-hash-table :test #'equal))

(defun nndiscourse--base (server)
  "Normalize SERVER or its configured HTTPS address."
  (discourse-auth-base (or nndiscourse-address
                           (if (string-prefix-p "https://" server) server (concat "https://" server)))))

(defun nndiscourse--load (base file)
  "Load BASE's JSON snapshot at FILE, without evaluating code."
  (let ((db (make-nndiscourse--db :base base :file file :busy (make-hash-table :test #'equal))))
    (when (file-exists-p file)
      (let ((data (with-temp-buffer (insert-file-contents file)
                                   (json-parse-buffer :object-type 'plist :array-type 'list
                                                      :null-object nil :false-object nil))))
        (unless (and (equal (plist-get data :version) 1) (equal (plist-get data :base) base))
          (error "Unrecognized Discourse snapshot"))
        (setf (nndiscourse--db-groups db) (plist-get data :groups)
              (nndiscourse--db-attempts db) (plist-get data :attempts))))
    db))

(defun nndiscourse--save (db)
  "Atomically persist DB without credentials or Gnus reading marks."
  (let* ((directory (file-name-directory (nndiscourse--db-file db))) temp)
    (make-directory directory t)
    (set-file-modes directory #o700)
    (unwind-protect
        (progn
          (setq temp (make-temp-file (expand-file-name ".snapshot-" directory)))
          (set-file-modes temp #o600)
          (with-temp-file temp
            (let ((json-encoding-pretty-print nil))
              (insert (json-encode
                       (list :version 1 :base (nndiscourse--db-base db)
                             :groups (vconcat
                                      (mapcar (lambda (group)
                                                (let ((copy (copy-sequence group)))
                                                  (setf (plist-get copy :posts) (vconcat (plist-get copy :posts)))
                                                  copy))
                                              (nndiscourse--db-groups db)))
                             :attempts (vconcat (nndiscourse--db-attempts db)))))))
          (rename-file temp (nndiscourse--db-file db) t))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(deffoo nndiscourse-open-server (server &optional defs _connectionless)
  (condition-case problem
      (progn
        (nnoo-change-server 'nndiscourse server defs)
        (let* ((base (nndiscourse--base server))
               (file (expand-file-name (concat (secure-hash 'sha256 base) ".json") nndiscourse-directory)))
          (setq nndiscourse--state
                (or (gethash file nndiscourse--databases)
                    (puthash file (nndiscourse--load base file) nndiscourse--databases))))
        t)
    (error (nnheader-report 'nndiscourse "%s" (error-message-string problem)))))

(defun nndiscourse--select (&optional server)
  "Select SERVER, returning its database."
  (when (and server (not (nndiscourse-open-server server))) (error "%s" nndiscourse-status-string))
  (or nndiscourse--state (error "No Discourse server selected")))

(defun nndiscourse--topic-id (group)
  "Extract a positive topic ID from GROUP."
  (unless (and (stringp group) (string-match "\\`topic\\.\\([1-9][0-9]*\\)\\'" group))
    (error "Expected topic.ID; subscribe using nndiscourse-subscribe-topic"))
  (string-to-number (match-string 1 group)))

(defun nndiscourse--list-group-p (group)
  "Return non-nil for a latest or category topic list GROUP."
  (and (stringp group)
       (or (member group '("latest" "notifications"))
           (string-match-p "\\`category\\.[1-9][0-9]*\\.[[:alnum:]-]+\\'" group))))

(defun nndiscourse--list-path (group)
  "Return the same-site JSON endpoint for list GROUP."
  (cond ((equal group "latest") "/latest.json")
        ((string-match "\\`category\\.\\([1-9][0-9]*\\)\\.\\([[:alnum:]-]+\\)\\'" group)
         (format "/c/%s/%s.json" (match-string 2 group) (match-string 1 group)))
        (t (error "Expected a Discourse topic list group"))))

(defun nndiscourse--group (db name &optional create)
  "Find NAME in DB, optionally CREATE a local subscription record."
  (or (cl-find name (nndiscourse--db-groups db) :key (lambda (g) (plist-get g :name)) :test #'equal)
      (when create
        (let ((record (if (nndiscourse--list-group-p name)
                          (list :name name :title
                                (if (equal name "notifications") "Replies to me" name)
                                :high 0 :posts nil)
                        (list :name name :id (nndiscourse--topic-id name)
                              :title name :high 0 :posts nil))))
          (push record (nndiscourse--db-groups db))
          record))))

(defun nndiscourse--positive-p (value)
  "Whether VALUE is a positive integer."
  (and (integerp value) (> value 0)))

(defun nndiscourse--http (db method path fields callback)
  "Request a same-site JSON endpoint; CALLBACK receives (DATA ERROR UNCERTAIN).
No browser cookies, redirects or curlrc.  POST failures may be uncertain."
  (let ((done nil) dispatched
        (plz-curl-default-args '("--disable" "--silent" "--show-error" "--compressed")))
    (cl-labels ((finish (data error &optional uncertain)
                  (unless done (setq done t) (funcall callback data error uncertain))))
      (condition-case nil
          (let ((headers (discourse-auth-headers (nndiscourse--db-base db) (eq method 'post))))
            (unless (or (and (eq method 'get)
                             (string-match-p "\\`/\\(?:t/[1-9][0-9]*\\(?:/posts\\)?\\|categories\\|latest\\|notifications\\|c/[[:alnum:]-]+/[1-9][0-9]*\\)\\.json\\'" path))
                        (and (eq method 'post) (equal path "/posts.json")))
              (error "Unsupported endpoint"))
            (let ((query (url-build-query-string fields)))
              (setq dispatched t)
              (plz method (concat (nndiscourse--db-base db) path
                                  (when (and (eq method 'get) fields) (concat "?" query)))
                :headers (append headers '(("Accept" . "application/json")
                                           ("Content-Type" . "application/x-www-form-urlencoded; charset=utf-8")))
                :body-type 'binary :body (when (eq method 'post) (encode-coding-string query 'utf-8))
                :as 'response :timeout nndiscourse-timeout :connect-timeout 10
                :then (lambda (response)
                        (let (data failure)
                          (condition-case nil
                              (if (<= 200 (plz-response-status response) 299)
                                  (progn
                                    (setq data (json-parse-string (plz-response-body response) :object-type 'plist
                                                                  :array-type 'list :null-object nil :false-object nil))
                                    (when (plist-get data :errors) (setq failure "Discourse rejected the request")))
                                (setq failure "Discourse redirected the request"))
                            (error (setq failure "Invalid Discourse response")))
                          (finish data failure (and failure (eq method 'post)))))
                :else (lambda (failure)
                        (let* ((err (if (plz-error-p failure) failure
                                      (and (listp failure) (cl-find-if #'plz-error-p failure))))
                               (response (and err (plz-error-response err)))
                               (status (and response (plz-response-status response))))
                          (finish nil (if status (format "Discourse HTTP %s; check authorization and permissions" status)
                                        "Discourse network failure or timeout")
                                  (and (eq method 'post)
                                       (not (and status (<= 400 status 499) (/= status 408))))))))))
        (error (finish nil "Discourse request setup failed; authorize with discourse-auth-login before posting"
                       (and dispatched (eq method 'post))))))))

(defun nndiscourse--normalize (db group title posts)
  "Validate POSTS and return a complete replacement snapshot for GROUP."
  (unless (stringp title) (error "Missing topic title"))
  (let ((seen (make-hash-table :test #'eql)) records)
    (dolist (post posts)
      (let ((floor (plist-get post :post_number)) (parent (plist-get post :reply_to_post_number)))
        (unless (and (nndiscourse--positive-p floor) (nndiscourse--positive-p (plist-get post :id))
                     (equal (plist-get post :topic_id) (plist-get group :id))
                     (not (gethash floor seen)) (stringp (plist-get post :cooked))
                     (or (null parent) (and (nndiscourse--positive-p parent) (< parent floor))))
          (error "Invalid topic post or reply relation"))
        (puthash floor t seen)
        (push (list :number floor :id (plist-get post :id)
                    :parent (unless (= floor 1) (or parent 1))
                    :author (or (plist-get post :display_username) (plist-get post :username) "Discourse")
                    :time (plist-get post :created_at) :body (plist-get post :cooked) :missing nil) records)))
    (dolist (parent (delete-dups (delq nil (mapcar (lambda (p) (plist-get p :parent)) records))))
      (unless (gethash parent seen)
        (push (list :number parent :id nil :parent (unless (= parent 1) 1)
                    :author "Unavailable post" :time nil :body "<p>This parent post was deleted or is inaccessible.</p>"
                    :missing t) records)))
    (unless (cl-find 1 records :key (lambda (p) (plist-get p :number)))
      (push (list :number 1 :id nil :parent nil :author "Unavailable first post" :time nil
                  :body "<p>The first post is inaccessible.</p>" :missing t) records))
    (ignore db)
    (list :name (plist-get group :name) :id (plist-get group :id) :title title
          :high (apply #'max (plist-get group :high) (mapcar (lambda (p) (plist-get p :number)) records))
          :posts (sort records (lambda (a b) (< (plist-get a :number) (plist-get b :number)))))))

(defun nndiscourse--fetch-topic (db topic callback)
  "Fetch all posts of TOPIC; call CALLBACK with (TITLE POSTS FAILURE)."
  (let (title posts remaining finished)
    (cl-labels
        ((finish (failure)
           (unless finished
             (setq finished t)
             (funcall callback title posts failure)))
         (next-page ()
           (if (null remaining) (finish nil)
             (let ((batch (seq-take remaining 20)))
               (nndiscourse--http
                db 'get (format "/t/%d/posts.json" topic)
                (mapcar (lambda (id) (list "post_ids[]" (number-to-string id))) batch)
                (lambda (data failure _uncertain)
                  (if failure (finish failure)
                    (condition-case problem
                        (let* ((page (plist-get (plist-get data :post_stream) :posts))
                               (ids (mapcar (lambda (p) (plist-get p :id)) page)))
                          (unless (and (= (length ids) (length batch))
                                       (null (cl-set-exclusive-or ids batch :test #'equal)))
                            (error "Incomplete topic page; refresh to reconcile deletions"))
                          (setq posts (append posts page) remaining (nthcdr (length batch) remaining))
                          (next-page))
                      (error (finish (error-message-string problem)))))))))))
      (nndiscourse--http
       db 'get (format "/t/%d.json" topic) nil
       (lambda (data failure _uncertain)
         (if failure (finish failure)
           (condition-case problem
               (let* ((stream (plist-get data :post_stream)) (ids (plist-get stream :stream)))
                 (setq title (plist-get data :title) posts (plist-get stream :posts))
                 (unless (and (equal topic (plist-get data :id)) (consp ids) (consp posts)
                              (cl-every #'nndiscourse--positive-p ids)
                              (= (length ids) (length (delete-dups (copy-sequence ids))))
                              (cl-every (lambda (post) (member (plist-get post :id) ids)) posts))
                   (error "Invalid topic stream"))
                 (setq remaining (cl-set-difference ids (mapcar (lambda (p) (plist-get p :id)) posts)))
                 (next-page))
             (error (finish (error-message-string problem))))))))))

(defun nndiscourse--replace-group (db old replacement)
  "Persist REPLACEMENT for OLD in DB, restoring the old value on failure."
  (let ((groups (nndiscourse--db-groups db)))
    (setf (nndiscourse--db-groups db) (cons replacement (delq old (copy-sequence groups))))
    (condition-case problem
        (nndiscourse--save db)
      (error (setf (nndiscourse--db-groups db) groups)
             (signal (car problem) (cdr problem))))))

(defun nndiscourse--latest-entries (record data)
  "Return a new RECORD with topic roots from a list DATA response."
  (let* ((topics (plist-get (plist-get data :topic_list) :topics))
         (users (plist-get data :users))
         (seen (make-hash-table :test #'eql))
         (copy (copy-sequence record))
         (entries (mapcar #'copy-sequence (plist-get record :posts)))
         (high (plist-get record :high)))
    (unless (and (consp topics) (listp users))
      (error "Invalid Discourse latest list"))
    (dolist (topic topics)
      (let* ((id (plist-get topic :id))
             (title (plist-get topic :title))
             (time (or (plist-get topic :last_posted_at)
                       (plist-get topic :created_at)))
             (poster (car (plist-get topic :posters)))
             (user (cl-find (plist-get poster :user_id) users
                            :key (lambda (item) (plist-get item :id))))
             (author (or (plist-get user :name) (plist-get user :username)
                         (plist-get topic :last_poster_username) "Discourse"))
             (root (cl-find-if (lambda (p) (and (equal id (plist-get p :topic-id))
                                                (equal 1 (plist-get p :floor)))) entries)))
        (unless (and (nndiscourse--positive-p id) (not (gethash id seen))
                     (stringp title) (not (string-empty-p title))
                     (stringp time) (date-to-time time) (stringp author))
          (error "Invalid Discourse latest topic"))
        (puthash id t seen)
        (unless root
          (setq root (list :number (cl-incf high) :topic-id id :floor 1
                           :parent nil :body nil)))
        (setf (plist-get root :title) title
              (plist-get root :author) author
              (plist-get root :time) time)
        (setq entries
              (cons root (cl-remove-if
                          (lambda (p) (and (equal id (plist-get p :topic-id))
                                           (equal 1 (plist-get p :floor)))) entries)))))
    (setf (plist-get copy :high) high
          (plist-get copy :posts) (sort entries (lambda (a b) (< (plist-get a :number)
                                                             (plist-get b :number)))))
    copy))

(defun nndiscourse--notification-entries (db record data)
  "Group reply notifications into one article per topic."
  (unless (plist-member data :notifications)
    (error "Invalid Discourse notification response"))
  (let ((by-topic (make-hash-table :test #'eql))
        (copy (copy-sequence record))
        (entries (mapcar #'copy-sequence (plist-get record :posts)))
        (high (plist-get record :high)))
    (dolist (notification (plist-get data :notifications))
      (when (equal (plist-get notification :notification_type) 2)
        (let ((topic (plist-get notification :topic_id))
              (id (plist-get notification :id))
              (floor (plist-get notification :post_number))
              (created (plist-get notification :created_at)))
          (unless (and (nndiscourse--positive-p topic)
                       (nndiscourse--positive-p id)
                       (nndiscourse--positive-p floor)
                       (stringp created) (date-to-time created))
            (error "Invalid Discourse reply notification"))
          (push notification (gethash topic by-topic)))))
    (maphash
     (lambda (topic notifications)
       (setq notifications
             (sort notifications
                   (lambda (a b)
                     (time-less-p (date-to-time (plist-get b :created_at))
                                  (date-to-time (plist-get a :created_at))))))
       (let* ((ids (sort (mapcar (lambda (n) (plist-get n :id)) notifications) #'<))
              (previous (cl-find-if (lambda (p) (equal topic (plist-get p :topic-id)))
                                    entries))
              (latest (car notifications))
              (meta (plist-get latest :data))
              (meta (if (stringp meta)
                        (json-parse-string meta :object-type 'plist :array-type 'list)
                      meta))
              (floors (delete-dups (mapcar (lambda (n) (plist-get n :post_number))
                                                notifications)))
              (body (concat "<p>Replies to this topic:</p><ul>"
                            (mapconcat
                             (lambda (notification)
                               (let ((floor (plist-get notification :post_number)))
                                 (format "<li><a href=\"%s/t/%d/%d\">Reply #%d</a></li>"
                                         (nndiscourse--db-base db) topic floor floor)))
                             notifications "")
                            "</ul>"))
              (entry (if (and previous
                              (equal ids (plist-get previous :notification-ids)))
                         previous
                       (list :number (cl-incf high) :topic-id topic :floor 1
                             :parent nil :notification-ids ids))))
         (setf (plist-get entry :title)
               (or (plist-get meta :topic_title) (format "Topic %d" topic))
               (plist-get entry :author)
               (or (plist-get meta :display_username)
                   (plist-get meta :username) "Discourse")
               (plist-get entry :time) (plist-get latest :created_at)
               (plist-get entry :body) body
               (plist-get entry :alert-floors) floors)
         (setq entries (cons entry
                             (cl-remove topic entries :key (lambda (p) (plist-get p :topic-id))
                                        :test #'equal)))))
     by-topic)
    (setf (plist-get copy :high) high
          (plist-get copy :posts)
          (sort entries (lambda (a b) (< (plist-get a :number) (plist-get b :number)))))
    copy))

(defun nndiscourse--update-notifications (db record callback)
  "Refresh replies in RECORD, preserving Gnus reading marks."
  (nndiscourse--http
   db 'get "/notifications.json" nil
   (lambda (data failure _uncertain)
     (unless failure
       (condition-case problem
           (nndiscourse--replace-group
            db record (nndiscourse--notification-entries db record data))
         (error (setq failure (error-message-string problem)))))
     (when callback (funcall callback failure)))))

(defun nndiscourse--update-latest (db record callback)
  "Refresh only topic roots in list RECORD."
  (nndiscourse--http
   db 'get (nndiscourse--list-path (plist-get record :name)) nil
   (lambda (data failure _uncertain)
     (unless failure
       (condition-case problem
           (nndiscourse--replace-group db record (nndiscourse--latest-entries record data))
         (error (setq failure (error-message-string problem)))))
     (when callback (funcall callback failure)))))

(defun nndiscourse-update (group &optional server callback)
  "Refresh GROUP on SERVER asynchronously; CALLBACK receives an error or nil."
  (let* ((db (nndiscourse--select server))
         (record (nndiscourse--group db group t))
         (busy (nndiscourse--db-busy db)))
    (when (gethash group busy) (user-error "This group is already updating"))
    (puthash group t busy)
    (cl-labels ((finish (failure)
                  (remhash group busy)
                  (when callback (funcall callback failure))))
      (condition-case problem
          (cond
           ((equal group "notifications")
            (nndiscourse--update-notifications db record #'finish))
           ((nndiscourse--list-group-p group)
            (nndiscourse--update-latest db record #'finish))
           (t
            (nndiscourse--fetch-topic
             db (plist-get record :id)
             (lambda (title posts failure)
               (unless failure
                 (condition-case problem
                     (nndiscourse--replace-group
                      db record (nndiscourse--normalize db record title posts))
                   (error (setq failure (error-message-string problem)))))
               (finish failure)))))
        (error (finish (error-message-string problem)))))))

(defun nndiscourse--await (start)
  "Invoke START with a callback and wait for its result list."
  (let ((deadline (+ (float-time) nndiscourse-scan-timeout)) done result)
    (funcall start (lambda (&rest values) (setq result values done t)))
    (while (and (not done) (< (float-time) deadline)) (accept-process-output nil 0.05))
    (unless done (error "Discourse operation is still pending; do not resend"))
    result))

(defun nndiscourse--subscribed-p (group server)
  "Whether GROUP on SERVER is subscribed in Gnus."
  (let ((info (gnus-get-info (gnus-group-prefixed-name group (list 'nndiscourse server)))))
    (and info (<= (gnus-info-level info) gnus-level-subscribed))))

(deffoo nndiscourse-request-scan (&optional group server)
  (condition-case problem
      (let* ((db (nndiscourse--select server))
             (names (if group (list group) (mapcar (lambda (g) (plist-get g :name)) (nndiscourse--db-groups db)))))
        (dolist (name names)
          (when (nndiscourse--subscribed-p name (or server (nnoo-current-server 'nndiscourse)))
            (when-let* ((failure (car (nndiscourse--await (lambda (cb) (nndiscourse-update name server cb))))))
              (error "%s" failure)))) t)
    (error (nnheader-report 'nndiscourse "%s" (error-message-string problem)))))

(defun nndiscourse--message-id (db group number)
  "Return a stable, site-scoped Message-ID for NUMBER in GROUP."
  (format "<%s.%d.%s@discourse.invalid>" (plist-get group :name) number
          (secure-hash 'sha256 (nndiscourse--db-base db))))

(defun nndiscourse--post-message-id (db group post)
  "Return POST's identity, shared by latest and topic groups."
  (nndiscourse--message-id
   db (if (nndiscourse--list-group-p (plist-get group :name))
          (list :name (format "topic.%d" (plist-get post :topic-id))) group)
   (or (plist-get post :floor) (plist-get post :number))))

(defun nndiscourse--line (value)
  "Sanitize VALUE for an RFC header."
  (replace-regexp-in-string "[\r\n\t\x00-\x1f]+" " " (if (stringp value) value "")))

(defun nndiscourse--header (db group post)
  "Build the native mail header for POST."
  (let* ((number (plist-get post :number))
         (parent (plist-get post :parent))
         (parent-post (and parent (cl-find parent (plist-get group :posts)
                                            :key (lambda (p) (plist-get p :number)))))
         (title (or (plist-get post :title) (plist-get group :title))))
    (make-full-mail-header
     number (concat (when parent "Re: ") (nndiscourse--line title))
     (nndiscourse--line (plist-get post :author))
     (condition-case nil
         (let ((system-time-locale "C"))
           (format-time-string "%a, %d %b %Y %T %z" (date-to-time (plist-get post :time)) t))
       (error "Thu, 01 Jan 1970 00:00:00 +0000"))
     (nndiscourse--post-message-id db group post)
     (if parent-post (nndiscourse--post-message-id db group parent-post) "") 0 0 "" nil)))

(defun nndiscourse--post (db group article)
  "Find an ARTICLE number or Message-ID in GROUP."
  (cl-find-if (lambda (post) (if (integerp article) (= article (plist-get post :number))
                              (equal article (nndiscourse--post-message-id db group post))))
              (plist-get group :posts)))

(defun nndiscourse--merge-latest-thread (db record topic title posts)
  "Return a new latest RECORD with TOPIC's complete reply tree."
  (let* ((topic-group (list :name (format "topic.%d" topic) :id topic :high 0))
         (normalized (nndiscourse--normalize db topic-group title posts))
         (old (plist-get record :posts))
         (numbers (make-hash-table :test #'eql))
         (high (plist-get record :high))
         (copy (copy-sequence record))
         imported)
    (dolist (post (plist-get normalized :posts))
      (let* ((floor (plist-get post :number))
             (previous (cl-find-if
                        (lambda (item) (and (equal topic (plist-get item :topic-id))
                                             (equal floor (plist-get item :floor)))) old)))
        (puthash floor (if previous (plist-get previous :number) (cl-incf high)) numbers)))
    (dolist (post (plist-get normalized :posts))
      (let* ((floor (plist-get post :number))
             (previous (cl-find (gethash floor numbers) old
                                :key (lambda (item) (plist-get item :number))))
             (entry (copy-sequence post)))
        (setf (plist-get entry :number) (gethash floor numbers)
              (plist-get entry :topic-id) topic
              (plist-get entry :floor) floor
              (plist-get entry :title) title
              (plist-get entry :parent) (and (plist-get post :parent)
                                              (gethash (plist-get post :parent) numbers)))
        ;; The list's root date represents the last activity, for summary order.
        (when (and (= floor 1) previous)
          (setf (plist-get entry :time) (plist-get previous :time))
          (when (plist-get previous :notification-ids)
            (setf (plist-get entry :notification-ids)
                  (plist-get previous :notification-ids)
                  (plist-get entry :alert-floors)
                  (plist-get previous :alert-floors))))
        (push entry imported)))
    (setf (plist-get copy :high) high
          (plist-get copy :posts)
          (sort (append (cl-remove topic old :key (lambda (item) (plist-get item :topic-id))
                                       :test #'equal)
                        imported)
                (lambda (a b) (< (plist-get a :number) (plist-get b :number)))))
    copy))

(defun nndiscourse--latest-root-body (db record post)
  "Fetch POST's body without importing its replies into latest RECORD."
  (let* ((topic (plist-get post :topic-id))
         (response (nndiscourse--await
                    (lambda (cb)
                      (nndiscourse--http db 'get (format "/t/%d.json" topic) nil cb))))
         (data (car response))
         (failure (cadr response))
         (root (cl-find 1 (plist-get (plist-get data :post_stream) :posts)
                        :key (lambda (item) (plist-get item :post_number)))))
    (when failure (error "%s" failure))
    (unless (and (equal topic (plist-get data :id))
                 (nndiscourse--positive-p (plist-get root :id))
                 (stringp (plist-get root :cooked)))
      (error "Discourse did not return this topic's first post"))
    (let* ((copy (copy-sequence record))
           (entries (mapcar #'copy-sequence (plist-get record :posts)))
           (entry (cl-find (plist-get post :number) entries
                           :key (lambda (item) (plist-get item :number)))))
      (setf (plist-get entry :id) (plist-get root :id)
            (plist-get entry :body) (plist-get root :cooked)
            (plist-get copy :posts)
            (cons entry (cl-remove (plist-get post :number) entries
                                   :key (lambda (item) (plist-get item :number)))))
      (nndiscourse--replace-group db record copy)
      entry)))

(deffoo nndiscourse-request-create-group (group &optional server _args)
  (let ((db (nndiscourse--select server))) (nndiscourse--group db group t) (nndiscourse--save db)) t)
(deffoo nndiscourse-request-type (_group &optional _article) 'post)
(deffoo nndiscourse-asynchronous-p () nil)
(deffoo nndiscourse-close-group (_group &optional _server) t)
(defun nndiscourse--body ()
  "Read a plain Markdown body from the current Message buffer."
  (save-excursion
    (message-goto-body)
    (let ((body (buffer-substring-no-properties (point) (point-max)))
          (type (message-fetch-field "Content-Type")))
      (when (or (and type (not (string-match-p "\\`text/plain\\(?:;\\|\\'\\)" (downcase type))))
                (string-match-p "<#\\(?:part\\|multipart\\|secure\\|external\\)" body))
        (error "Discourse composer supports plain Markdown only; remove attachments"))
      (when (string-empty-p (string-trim body)) (error "Empty Discourse post"))
      body)))

(defun nndiscourse--submit (db group parent title body &optional category)
  "Post BODY to GROUP, with PARENT floor or a new TITLE and CATEGORY.
Persist an uncertain-send lock before network dispatch."
  (let* ((kind (if category "topic" "reply"))
         (fingerprint (secure-hash 'sha256 (prin1-to-string
                                           (list (nndiscourse--db-base db) kind
                                                 (and group (plist-get group :id)) parent title category body))))
         (previous (cl-find fingerprint (nndiscourse--db-attempts db)
                            :key (lambda (a) (plist-get a :fingerprint)) :test #'equal))
         (attempt (or previous (list :fingerprint fingerprint :state "pending")))
         (fields (if category `(("title" ,title) ("raw" ,body) ("category" ,(number-to-string category)))
                   `(("topic_id" ,(number-to-string (plist-get group :id)))
                     ("reply_to_post_number" ,(number-to-string parent)) ("raw" ,body))))
         result failure uncertain)
    (when (and previous (equal (plist-get previous :state) "pending"))
      (error "Previous send is uncertain; inspect the site before clearing the lock"))
    (when (and previous (equal (plist-get previous :state) "sent"))
      (error "This exact post was already sent; refresh the topic"))
    (unless previous (push attempt (nndiscourse--db-attempts db)))
    (setf (plist-get attempt :state) "pending")
    (nndiscourse--save db)
    (condition-case problem
        (pcase-let ((`(,data ,err ,maybe)
                     (nndiscourse--await
                      (lambda (cb)
                        (nndiscourse--http db 'post "/posts.json" fields
                                          (lambda (payload reason unsure) (funcall cb payload reason unsure)))))))
          (setq result data failure err uncertain maybe))
      (error (setq failure (error-message-string problem) uncertain t)))
    (unless failure
      (unless (and (nndiscourse--positive-p (plist-get result :id))
                   (nndiscourse--positive-p (plist-get result :topic_id))
                   (nndiscourse--positive-p (plist-get result :post_number))
                   (if category
                       (= (plist-get result :post_number) 1)
                     (and (equal (plist-get result :topic_id) (plist-get group :id))
                          (equal (plist-get result :reply_to_post_number) parent))))
        (setq failure "Discourse did not confirm this post; check the website before retrying"
              uncertain t)))
    (unless uncertain (setf (plist-get attempt :state) (if failure "failed" "sent")))
    (nndiscourse--save db)
    (when failure (error "%s" failure))
    result))

(deffoo nndiscourse-request-post (&optional server)
  "Publish a native Gnus followup to a known Discourse floor."
  (condition-case problem
      (let* ((db (nndiscourse--select server))
             (raw-group (message-fetch-field "Newsgroups"))
             (name (and raw-group (gnus-group-real-name raw-group)))
             (record (and name (nndiscourse--group db name)))
             (refs (split-string (or (message-fetch-field "References") "")))
             (target (and record (nndiscourse--post db record (car (last refs))))))
        (unless (and record target (not (plist-get target :missing))
                     (not (string-match-p "[,\r\n]" raw-group)))
          (error "Reply to one known Discourse floor; crossposting is unsupported"))
        (nndiscourse--submit
         db (if (nndiscourse--list-group-p name)
                (list :id (plist-get target :topic-id)) record)
         (or (plist-get target :floor) (plist-get target :number))
         nil (nndiscourse--body))
        t)
    (error (nnheader-report 'nndiscourse "%s" (error-message-string problem)))))

(deffoo nndiscourse-request-group (group &optional server _fast _info)
  (let ((record (nndiscourse--group (nndiscourse--select server) group)))
    (if (not record) (nnheader-report 'nndiscourse "Unknown Discourse group")
      (nnheader-insert "211 %d 1 %d %s\n" (length (plist-get record :posts)) (plist-get record :high) group t))))

(deffoo nndiscourse-request-list (&optional server)
  (let ((db (nndiscourse--select server)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (g (nndiscourse--db-groups db))
        (insert (format "%s %d 1 y\n" (plist-get g :name) (plist-get g :high)))))) t)
(deffoo nndiscourse-retrieve-groups (_groups &optional server)
  (nndiscourse-request-list server) 'active)
(deffoo nndiscourse-request-list-newsgroups (&optional server)
  (let ((db (nndiscourse--select server)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (g (nndiscourse--db-groups db))
        (insert (plist-get g :name) "\t" (nndiscourse--line (plist-get g :title)) "\n")))) t)

(deffoo nndiscourse-retrieve-headers (articles &optional group server _fetch-old)
  (let* ((db (nndiscourse--select server)) (record (nndiscourse--group db group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (number articles)
        (when-let* ((post (nndiscourse--post db record number)))
          (nnheader-insert-nov (nndiscourse--header db record post)))))) 'nov)

(deffoo nndiscourse-request-thread (header group)
  (condition-case problem
      (let* ((db (nndiscourse--select))
             (record (nndiscourse--group db group))
             (anchor (and record (nndiscourse--post db record (mail-header-id header)))))
        (unless anchor (error "Discourse article is absent from this group"))
        (when (nndiscourse--list-group-p group)
          (let* ((topic (plist-get anchor :topic-id))
                 (result (nndiscourse--await
                          (lambda (cb) (nndiscourse--fetch-topic db topic cb))))
                 (failure (nth 2 result)))
            (when failure (error "%s" failure))
            (nndiscourse--replace-group
             db record (nndiscourse--merge-latest-thread
                        db record topic (nth 0 result) (nth 1 result)))
            (setq record (nndiscourse--group db group))))
        (mapcar (lambda (p) (nndiscourse--header db record p))
                (if (nndiscourse--list-group-p group)
                    (cl-remove-if-not
                     (lambda (p) (equal (plist-get p :topic-id)
                                         (plist-get anchor :topic-id)))
                     (plist-get record :posts))
                  (plist-get record :posts))))
    (error (nnheader-report 'nndiscourse "%s" (error-message-string problem)))))

(deffoo nndiscourse-request-article (article &optional group server buffer)
  (let* ((db (nndiscourse--select server)) (record (nndiscourse--group db group))
         (post (nndiscourse--post db record article)))
    (if (not post) (nnheader-report 'nndiscourse "Article is absent from this topic snapshot")
      (when (and (nndiscourse--list-group-p group) (null (plist-get post :body)))
        (setq post (nndiscourse--latest-root-body db record post)
              record (nndiscourse--group db group)))
      (let ((header (nndiscourse--header db record post)))
        (with-current-buffer (or buffer nntp-server-buffer)
          (erase-buffer)
          (insert "From: " (rfc2047-encode-string (mail-header-from header)) "\n"
                  "Subject: " (rfc2047-encode-string (mail-header-subject header)) "\n"
                  "Date: " (mail-header-date header) "\nMessage-ID: " (mail-header-id header) "\n"
                  "References: " (mail-header-references header) "\nNewsgroups: " group "\n"
                  "Archived-at: <" (nndiscourse--db-base db) "/t/"
                  (number-to-string (or (plist-get post :topic-id) (plist-get record :id)))
                  "/" (number-to-string (or (plist-get post :floor) (plist-get post :number))) ">\n"
                  "MIME-Version: 1.0\nContent-Type: text/html; charset=utf-8\nContent-Transfer-Encoding: base64\n\n"
                  (base64-encode-string (encode-coding-string (plist-get post :body) 'utf-8)) "\n")))
      (cons group (plist-get post :number)))))

(defun nndiscourse--location (url)
  "Parse a topic URL into (BASE ID), ignoring a floor suffix and query."
  (unless (and (stringp url)
               (string-match "\\`\\(https://[^?#]+?\\)/t/\\([^?#\r\n]+\\)\\(?:[?#][^\r\n]*\\)?\\'" url))
    (user-error "Enter an HTTPS Discourse topic URL"))
  (let* ((base (match-string 1 url))
         (path (string-remove-suffix ".json" (string-remove-suffix "/" (match-string 2 url))))
         (parts (split-string path "/"))
         (tail (if (string-match-p "\\`[1-9][0-9]*\\'" (car parts)) parts (cdr parts))))
    (unless (and (<= 1 (length tail) 2)
                 (cl-every (lambda (part) (string-match-p "\\`[1-9][0-9]*\\'" part)) tail))
      (user-error "Invalid topic ID or floor suffix"))
    (list (discourse-auth-base base) (string-to-number (car tail)))))

(defun nndiscourse--server-for (base)
  "Use BASE itself as the server identity, preserving mount and port isolation."
  ;; A colon in server names interferes with Gnus foreign-group name parsing.
  (concat (url-host (url-generic-parse-url base)) "-" (substring (secure-hash 'sha256 base) 0 12)))

;;;###autoload
(defun nndiscourse-subscribe-topic (url)
  "Subscribe to URL and read its full topic."
  (interactive "sDiscourse topic URL: ")
  (pcase-let* ((`(,base ,id) (nndiscourse--location url))
              (server (nndiscourse--server-for base))
              (method `(nndiscourse ,server (nndiscourse-address ,base)))
              (group (format "topic.%d" id))
              (full (gnus-group-prefixed-name group method)))
    (unless (gnus-alive-p) (gnus-no-server))
    (unless (nndiscourse-open-server server (cddr method)) (error "%s" nndiscourse-status-string))
    (with-current-buffer gnus-group-buffer
      (unless (gnus-group-entry full) (gnus-group-make-group group method))
      (gnus-group-change-level (gnus-group-entry full)
                               gnus-level-default-subscribed))
    (nndiscourse-update
     group server
     (lambda (failure)
       (if failure (message "Topic update failed: %s" failure)
         (with-current-buffer gnus-group-buffer (gnus-group-read-group t t full)))))))

;;;###autoload
(defun nndiscourse-subscribe-latest (site)
  "Subscribe to SITE's latest topic list; retrieve replies on demand."
  (interactive "sDiscourse site URL: ")
  (let* ((base (discourse-auth-base site))
         (server (nndiscourse--server-for base))
         (method `(nndiscourse ,server (nndiscourse-address ,base)))
         (group "latest")
         (full (gnus-group-prefixed-name group method)))
    (unless (gnus-alive-p) (gnus-no-server))
    (unless (nndiscourse-open-server server (cddr method))
      (error "%s" nndiscourse-status-string))
    (with-current-buffer gnus-group-buffer
      (unless (gnus-group-entry full) (gnus-group-make-group group method))
      (gnus-group-change-level (gnus-group-entry full)
                               gnus-level-default-subscribed))
    (nndiscourse-update
     group server
     (lambda (failure)
       (if failure (message "Latest topics update failed: %s" failure)
         (with-current-buffer gnus-group-buffer
           (gnus-group-read-group t t full)))))))

;;;###autoload
(defun nndiscourse-subscribe-topic-at-point ()
  "Subscribe to the Discourse topic under point in a Gnus Summary."
  (interactive)
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "Use this in a Gnus Summary"))
  (let* ((method (gnus-find-method-for-group gnus-newsgroup-name))
         (server (nth 1 method))
         (db (and (eq (car method) 'nndiscourse) (nndiscourse--select server)))
         (group (gnus-group-real-name gnus-newsgroup-name))
         (record (and db (nndiscourse--group db group)))
         (post (and record
                    (nndiscourse--post db record
                                      (mail-header-id (gnus-summary-article-header))))))
    (unless post (user-error "No Discourse topic at point"))
    (nndiscourse-subscribe-topic
     (format "%s/t/%d" (nndiscourse--db-base db)
             (or (plist-get post :topic-id) (plist-get record :id))))))

;;;###autoload
(defun nndiscourse-open-notification ()
  "Expand the notification topic and visit its newest replied floor."
  (interactive)
  (unless (and (derived-mode-p 'gnus-summary-mode)
               (equal (gnus-group-real-name gnus-newsgroup-name) "notifications"))
    (user-error "Use this in a Discourse notifications Summary"))
  (let* ((method (gnus-find-method-for-group gnus-newsgroup-name))
         (db (nndiscourse--select (nth 1 method)))
         (record (nndiscourse--group db "notifications"))
         (post (nndiscourse--post db record
                                 (mail-header-id (gnus-summary-article-header))))
         (topic (and post (plist-get post :topic-id)))
         (floor (car (plist-get post :alert-floors))))
    (unless (and topic floor)
      (user-error "No notification target at point"))
    (gnus-summary-refer-thread)
    (setq record (nndiscourse--group db "notifications"))
    (let ((target (cl-find-if
                   (lambda (item) (and (equal topic (plist-get item :topic-id))
                                       (equal floor (plist-get item :floor))))
                   (plist-get record :posts))))
      (unless target (user-error "Notified floor is not available in this topic"))
      (gnus-summary-goto-article (plist-get target :number)))))

(defvar-local nndiscourse--compose-base nil)
(defvar-local nndiscourse--compose-category nil)
(defvar-local nndiscourse--compose-sending nil)
(defvar-local nndiscourse--compose-sent nil)
(defvar nndiscourse-compose-topic-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map message-mode-map)
    (define-key map [remap message-send] #'nndiscourse-compose-send)
    (define-key map [remap message-send-and-exit] #'nndiscourse-compose-send-and-exit)
    (define-key map (kbd "C-c C-c") #'nndiscourse-compose-send-and-exit)
    (define-key map (kbd "C-c C-s") #'nndiscourse-compose-send)
    map))
(define-derived-mode nndiscourse-compose-topic-mode message-mode "Discourse-Topic"
  "Compose a new Discourse topic with Message's editor."
  (setq-local message-send-method-alist
              '((nndiscourse nndiscourse--message-p nndiscourse--message-guard))))
(defun nndiscourse--message-p ()
  "Recognize a Discourse topic draft in Message's transport selection."
  (derived-mode-p 'nndiscourse-compose-topic-mode))
(defun nndiscourse--message-guard (&rest _)
  "Prevent direct Message transport from delivering a topic as mail or news."
  (user-error "Use nndiscourse-compose-send or C-c C-c"))

(defun nndiscourse--categories (db)
  "Read accessible categories from DB's site as (LABEL . ID) pairs."
  (pcase-let ((`(,data ,failure ,_)
               (nndiscourse--await (lambda (cb)
                                     (nndiscourse--http db 'get "/categories.json" nil cb)))))
    (when failure (error "%s" failure))
    (let* ((items (plist-get (plist-get data :category_list) :categories))
           (valid (cl-remove-if-not
                   (lambda (item) (and (nndiscourse--positive-p (plist-get item :id))
                                       (stringp (plist-get item :name)))) items)))
      (unless valid (error "No accessible categories returned"))
      (mapcar
       (lambda (item)
         (let* ((parent (cl-find (plist-get item :parent_category_id) valid
                                 :key (lambda (candidate) (plist-get candidate :id))))
                (label (concat (if parent (concat (plist-get parent :name) " / ") "")
                               (plist-get item :name)
                               (format " [%d]" (plist-get item :id)))))
           (cons label (plist-get item :id)))) valid))))

;;;###autoload
(defun nndiscourse-compose-topic (site)
  "Select a category on SITE and compose a new topic in message-mode."
  (interactive "sDiscourse site URL: ")
  (let* ((base (discourse-auth-base site))
         (server (nndiscourse--server-for base))
         (db (progn (unless (nndiscourse-open-server server `((nndiscourse-address ,base)))
                      (error "%s" nndiscourse-status-string))
                    (nndiscourse--select server)))
         (categories (nndiscourse--categories db))
         (choice (completing-read "Post in category: " categories nil t))
         (category (cdr (assoc choice categories)))
         (buffer (generate-new-buffer "*Discourse new topic*")))
    (with-current-buffer buffer
      (nndiscourse-compose-topic-mode)
      (setq nndiscourse--compose-base base nndiscourse--compose-category category)
      (insert "Subject: \n" mail-header-separator "\n")
      (setq-local header-line-format (format "Discourse · %s · %s" base choice))
      (goto-char (point-min)) (end-of-line))
    (pop-to-buffer buffer)
    buffer))

(defun nndiscourse-compose-send (&optional exit)
  "Publish this topic.  With EXIT, close the draft after confirmation."
  (interactive)
  (unless (derived-mode-p 'nndiscourse-compose-topic-mode) (user-error "Not a Discourse topic draft"))
  (when nndiscourse--compose-sending (user-error "Submission is already in progress"))
  (when nndiscourse--compose-sent (user-error "This topic was already sent"))
  (let* ((base nndiscourse--compose-base)
         (server (nndiscourse--server-for base))
         (category nndiscourse--compose-category)
         (title (string-trim (or (message-fetch-field "Subject") "")))
         (body (nndiscourse--body))
         result)
    (unless (and (not (string-empty-p title)) (not (string-match-p "[\r\n]" title)))
      (user-error "Enter a single-line topic title"))
    (setq nndiscourse--compose-sending t)
    (unwind-protect
        (progn
          (unless (nndiscourse-open-server server `((nndiscourse-address ,base)))
            (error "%s" nndiscourse-status-string))
          (setq result (nndiscourse--submit (nndiscourse--select server) nil nil title body category))
          (setq nndiscourse--compose-sent t buffer-read-only t)
          (set-buffer-modified-p nil)
          (message "Discourse topic published: %s/t/%d" base (plist-get result :topic_id))
          (run-at-time 0 nil #'nndiscourse-subscribe-topic
                       (format "%s/t/%d" base (plist-get result :topic_id)))
          (when exit
            (let ((draft (current-buffer)))
              (when (get-buffer-window draft) (quit-window nil (get-buffer-window draft)))
              (kill-buffer draft))))
      (when (buffer-live-p (current-buffer)) (setq nndiscourse--compose-sending nil)))))

(defun nndiscourse-compose-send-and-exit ()
  "Publish this topic and close the confirmed draft."
  (interactive)
  (nndiscourse-compose-send t))

;;;###autoload
(defun nndiscourse-clear-uncertain-sends ()
  "Clear pending send locks only after checking the website for duplicates."
  (interactive)
  (unless (yes-or-no-p "Checked the Discourse website and confirmed these posts were NOT published? ")
    (user-error "Pending submissions remain locked"))
  (let ((db (nndiscourse--select)))
    (setf (nndiscourse--db-attempts db)
          (cl-remove "pending" (nndiscourse--db-attempts db)
                     :test #'equal :key (lambda (item) (plist-get item :state))))
    (nndiscourse--save db)))

(gnus-declare-backend "nndiscourse" 'address)
(nnoo-define-skeleton nndiscourse)
(provide 'nndiscourse)
;;; nndiscourse.el ends here
