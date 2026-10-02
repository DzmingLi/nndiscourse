;;; nndiscourse.el --- Topic subscriptions for Discourse in Gnus -*- lexical-binding: t; -*-
;; Copyright (C) 2019 The Authors of nndiscourse.el
;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.2.0
;; Keywords: news, comm
;; URL: https://github.com/DzmingLi/nndiscourse
;; Package-Requires: ((emacs "29.1") (plz "0.9.1"))
;;; Commentary:
;; Fork of dickmao/nndiscourse.  One explicitly subscribed topic is one group;
;; each floor is a native article.  HTTP and User API authorization run in Emacs.
;; Gnus owns subscriptions, reading marks and caching.  No forum-wide crawl.
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

(defgroup nndiscourse nil "Discourse topic subscriptions." :group 'gnus)
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

(defun nndiscourse--group (db name &optional create)
  "Find NAME in DB, optionally CREATE a local subscription record."
  (or (cl-find name (nndiscourse--db-groups db) :key (lambda (g) (plist-get g :name)) :test #'equal)
      (when create
        (let ((record (list :name name :id (nndiscourse--topic-id name) :title name :high 0 :posts nil)))
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
                             (string-match-p "\\`/\\(?:t/[1-9][0-9]*\\(?:/posts\\)?\\|categories\\)\\.json\\'" path))
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

(defun nndiscourse-update (group &optional server callback)
  "Fetch only GROUP's complete topic on SERVER asynchronously.
Commit only a fully validated snapshot.  CALLBACK receives an error or nil."
  (let* ((db (nndiscourse--select server)) (record (nndiscourse--group db group t))
         (busy (nndiscourse--db-busy db)) (topic (plist-get record :id))
         title posts remaining finished)
    (when (gethash group busy) (user-error "This topic is already updating"))
    (puthash group t busy)
    (cl-labels
        ((finish (failure)
           (unless finished
             (setq finished t)
             (remhash group busy)
             (unless failure
               (condition-case problem
                   (let* ((replacement (nndiscourse--normalize db record title posts))
                          (old (nndiscourse--db-groups db)))
                     (setf (nndiscourse--db-groups db) (cons replacement (remove record old)))
                     (condition-case problem (nndiscourse--save db)
                       (error (setf (nndiscourse--db-groups db) old) (signal (car problem) (cdr problem)))))
                 (error (setq failure (error-message-string problem)))))
             (when callback (funcall callback failure))))
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

(defun nndiscourse--line (value)
  "Sanitize VALUE for an RFC header."
  (replace-regexp-in-string "[\r\n\t\x00-\x1f]+" " " (if (stringp value) value "")))

(defun nndiscourse--header (db group post)
  "Build the native mail header for POST."
  (let ((number (plist-get post :number)) (parent (plist-get post :parent)))
    (make-full-mail-header
     number (concat (unless (= number 1) "Re: ") (nndiscourse--line (plist-get group :title)))
     (nndiscourse--line (plist-get post :author))
     (condition-case nil
         (let ((system-time-locale "C"))
           (format-time-string "%a, %d %b %Y %T %z" (date-to-time (plist-get post :time)) t))
       (error "Thu, 01 Jan 1970 00:00:00 +0000"))
     (nndiscourse--message-id db group number)
     (if parent (nndiscourse--message-id db group parent) "") 0 0 "" nil)))

(defun nndiscourse--post (db group article)
  "Find an ARTICLE number or Message-ID in GROUP."
  (cl-find-if (lambda (post) (if (integerp article) (= article (plist-get post :number))
                              (equal article (nndiscourse--message-id db group (plist-get post :number)))))
              (plist-get group :posts)))

(deffoo nndiscourse-request-create-group (group &optional server _args)
  (let ((db (nndiscourse--select server))) (nndiscourse--group db group t) (nndiscourse--save db)) t)
(deffoo nndiscourse-request-type (_group &optional _article) 'post)
(deffoo nndiscourse-asynchronous-p () nil)
(deffoo nndiscourse-close-group (_group &optional _server) t)
(deffoo nndiscourse-request-post (&optional _server)
  (nnheader-report 'nndiscourse "Posting is not available in this revision"))

(deffoo nndiscourse-request-group (group &optional server _fast _info)
  (let ((record (nndiscourse--group (nndiscourse--select server) group)))
    (if (not record) (nnheader-report 'nndiscourse "Unknown topic subscription")
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

(deffoo nndiscourse-request-thread (_header group)
  (let* ((db (nndiscourse--select)) (record (nndiscourse--group db group)))
    (mapcar (lambda (p) (nndiscourse--header db record p)) (plist-get record :posts))))

(deffoo nndiscourse-request-article (article &optional group server buffer)
  (let* ((db (nndiscourse--select server)) (record (nndiscourse--group db group))
         (post (nndiscourse--post db record article)))
    (if (not post) (nnheader-report 'nndiscourse "Article is absent from this topic snapshot")
      (let ((header (nndiscourse--header db record post)))
        (with-current-buffer (or buffer nntp-server-buffer)
          (erase-buffer)
          (insert "From: " (rfc2047-encode-string (mail-header-from header)) "\n"
                  "Subject: " (rfc2047-encode-string (mail-header-subject header)) "\n"
                  "Date: " (mail-header-date header) "\nMessage-ID: " (mail-header-id header) "\n"
                  "References: " (mail-header-references header) "\nNewsgroups: " group "\n"
                  "Archived-at: <" (nndiscourse--db-base db) "/t/" (number-to-string (plist-get record :id))
                  "/" (number-to-string (plist-get post :number)) ">\n"
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
  "Subscribe to URL and read that topic; never enumerate the whole forum."
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
      (gnus-group-change-level full gnus-level-default-subscribed))
    (nndiscourse-update
     group server
     (lambda (failure)
       (if failure (message "Topic update failed: %s" failure)
         (with-current-buffer gnus-group-buffer (gnus-group-read-group t t full)))))))

(add-to-list 'gnus-valid-select-methods '(nndiscourse "discourse"))
(nnoo-define-skeleton nndiscourse)
(provide 'nndiscourse)
;;; nndiscourse.el ends here
