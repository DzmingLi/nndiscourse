;;; discourse-auth.el --- User API authorization for Discourse -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (plz "0.9.1"))
;;; Commentary:
;; Authorize in a browser using Discourse device codes.  API requests then
;; use a per-site User API Key, independently of browser cookies.
;;; Code:
(require 'cl-lib)
(require 'plz)
(require 'json)
(require 'auth-source)
(require 'url-util)
(require 'subr-x)
(require 'browse-url)

(defgroup discourse-auth nil "Discourse User API authorization." :group 'applications)
(defcustom discourse-auth-openssl-program "openssl"
  "OpenSSL executable used for ephemeral RSA keys and payload decryption."
  :type 'file)
(defcustom discourse-auth-scopes '("read" "write")
  "Scopes requested at authorization.  The forum must permit them."
  :type '(repeat string))
(defcustom discourse-auth-save-function nil
  "Optional function persisting a newly authorized credential.
Called with normalized site URL and a plist with :client and :key strings.
The function must not log credentials.  It should signal on failure and may
return a human-readable status string.  With nil, login is session-only.
Credentials are always read through the user's existing `auth-sources'."
  :type '(choice (const :tag "Session only" nil) function))
(defvar discourse-auth--credentials (make-hash-table :test #'equal))
(defvar discourse-auth--pending (make-hash-table :test #'equal))
(cl-defstruct discourse-auth--job base directory nonce client public device
              deadline interval timer buffer)

(defun discourse-auth-base (url)
  "Validate and normalize a Discourse site URL, including its mount path."
  (unless (and (stringp url)
               (string-match-p "\\`https://[[:alnum:].-]+\\(?::[0-9]+\\)?\\(?:/[[:alnum:]_-]+\\)*/?\\'" url))
    (user-error "Enter an HTTPS forum base URL, without a topic, query or fragment"))
  (let* ((parsed (url-generic-parse-url url))
         (port (url-portspec parsed)))
    (concat "https://" (downcase (url-host parsed))
            (if (and port (/= port 443)) (format ":%d" port) "")
            (string-remove-suffix "/" (url-filename parsed)))))

(defun discourse-auth--value (value)
  "Resolve an auth-source VALUE without exposing it in messages."
  (if (functionp value) (funcall value) value))

(defun discourse-auth-credential (base)
  "Return BASE's credential plist, or nil when not authorized."
  (setq base (discourse-auth-base base))
  (or (gethash base discourse-auth--credentials)
      (let ((token (car (auth-source-search :host base :port "discourse-user-api"
                                             :require '(:user :secret) :max 1))))
        (when token
          (list :client (discourse-auth--value (plist-get token :user))
                :key (discourse-auth--value (plist-get token :secret)))))))

(defun discourse-auth-headers (base &optional required)
  "Return User API headers for BASE; signal if REQUIRED but not authorized."
  (let ((credential (discourse-auth-credential base)))
    (when (and required (null credential))
      (user-error "Authorize this forum first with M-x discourse-auth-login"))
    (when credential
      (let ((key (plist-get credential :key)) (client (plist-get credential :client)))
        (unless (and (stringp key) (string-match-p "\\`[[:alnum:]_-]+\\'" key)
                     (stringp client) (string-match-p "\\`[[:alnum:]_-]+\\'" client))
          (error "Invalid saved Discourse credential"))
        `(("User-Api-Key" . ,key) ("User-Api-Client-Id" . ,client))))))

(defun discourse-auth-save (base)
  "Persist BASE's session credential using `discourse-auth-save-function'."
  (interactive (list (completing-read "Save forum authorization: "
                                      (hash-table-keys discourse-auth--credentials) nil t)))
  (setq base (discourse-auth-base base))
  (let ((credential (gethash base discourse-auth--credentials)))
    (unless credential (user-error "No current authorization for this site"))
    (unless discourse-auth-save-function
      (user-error "Configure discourse-auth-save-function to persist credentials"))
    (discourse-auth-headers base t)
    (let ((status (funcall discourse-auth-save-function base (copy-sequence credential))))
      (message "%s" (if (stringp status) status "Discourse credential saved"))
      status)))

(defun discourse-auth--openssl (input &rest args)
  "Run OpenSSL with INPUT on stdin and ARGS; return its output."
  (unless (executable-find discourse-auth-openssl-program)
    (user-error "Set discourse-auth-openssl-program to an OpenSSL executable"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when input (insert input))
    (unless (zerop (apply #'call-process-region (point-min) (point-max)
                         discourse-auth-openssl-program t '(t nil) nil args))
      (error "Discourse authorization cryptography failed"))
    (buffer-string)))

(defun discourse-auth--new-job (base)
  "Generate an ephemeral RSA key pair and unpredictable nonce for BASE."
  (let* ((directory (make-temp-file "discourse-auth-" t))
         (private (expand-file-name "private.pem" directory))
         (job (make-discourse-auth--job :base base :directory directory)))
    (condition-case problem
        (progn
          (set-file-modes directory #o700)
          (with-temp-file private
            (insert (discourse-auth--openssl nil "genpkey" "-algorithm" "RSA"
                                             "-pkeyopt" "rsa_keygen_bits:2048")))
          (set-file-modes private #o600)
          (setf (discourse-auth--job-public job)
                (discourse-auth--openssl nil "pkey" "-in" private "-pubout")
                (discourse-auth--job-nonce job)
                (string-trim (discourse-auth--openssl nil "rand" "-hex" "16"))
                (discourse-auth--job-client job)
                (concat "emacs-" (string-trim (discourse-auth--openssl nil "rand" "-hex" "16"))))
          job)
      (error (delete-directory directory t) (signal (car problem) (cdr problem))))))

(defun discourse-auth--decrypt (job payload)
  "Decrypt and validate an authorization PAYLOAD for JOB."
  (unless (and (stringp payload) (< (length payload) 16384))
    (error "Invalid authorization payload"))
  (let* ((plain (discourse-auth--openssl
                 (base64-decode-string payload) "pkeyutl" "-decrypt"
                 "-inkey" (expand-file-name "private.pem" (discourse-auth--job-directory job))
                 "-pkeyopt" "rsa_padding_mode:oaep" "-pkeyopt" "rsa_oaep_md:sha1"))
         (data (json-parse-string (decode-coding-string plain 'utf-8) :object-type 'plist)))
    (unless (and (equal (plist-get data :nonce) (discourse-auth--job-nonce job))
                 (stringp (plist-get data :key))
                 (string-match-p "\\`[[:alnum:]_-]+\\'" (plist-get data :key)))
      (error "Authorization nonce or key validation failed"))
    (list :key (plist-get data :key) :client (discourse-auth--job-client job))))

(defun discourse-auth--request (base method path fields callback)
  "Send a cookie-free authorization request; CALLBACK receives (VALUE ERROR)."
  (unless (and (member path '("/user-api-key/new" "/user-api-key/device.json" "/user-api-key/device/poll.json"))
               (or (and (eq method 'head) (equal path "/user-api-key/new"))
                   (and (eq method 'post) (not (equal path "/user-api-key/new")))))
    (error "Invalid Discourse authorization endpoint"))
  (let ((plz-curl-default-args '("--disable" "--silent" "--show-error" "--compressed")) done)
    (cl-labels ((finish (value error)
                 (unless done (setq done t) (funcall callback value error))))
      (condition-case nil
          (plz method (concat (discourse-auth-base base) path)
        :headers '(("Accept" . "application/json") ("Content-Type" . "application/json"))
        :body-type 'binary
        :body (when fields
                (encode-coding-string
                 (json-encode (mapcar (lambda (field) (cons (car field) (cadr field))) fields)) 'utf-8))
        :as 'response :timeout 30 :connect-timeout 10
        :then (lambda (response)
                (let (value failure)
                  (condition-case nil
                      (if (not (<= 200 (plz-response-status response) 299))
                          (setq failure "Forum redirected or rejected authorization")
                        (setq value (if (eq method 'head) (plz-response-headers response)
                                      (json-parse-string (plz-response-body response) :object-type 'plist))))
                    (error (setq failure "Invalid authorization response")))
                  (finish value failure)))
        :else (lambda (failure)
                (let* ((record (if (plz-error-p failure) failure
                                 (and (listp failure) (cl-find-if #'plz-error-p failure))))
                       (response (and record (plz-error-response record))))
                  (finish nil (if response
                                  (format "Authorization HTTP %s; check site access and allowed scopes"
                                          (plz-response-status response))
                                "Authorization request failed; check network access")))))
        (error (finish nil "Could not start authorization request"))))))

(defun discourse-auth--active-p (job)
  "Whether JOB is still the active authorization for its site."
  (eq job (gethash (discourse-auth--job-base job) discourse-auth--pending)))

(defun discourse-auth--status (job text)
  "Display TEXT in JOB's authorization buffer."
  (when (buffer-live-p (discourse-auth--job-buffer job))
    (with-current-buffer (discourse-auth--job-buffer job)
      (let ((inhibit-read-only t)) (goto-char (point-max)) (insert "\n" text "\n")))))

(defun discourse-auth--finish (job message)
  "Stop JOB, remove ephemeral secrets and report MESSAGE."
  (when (timerp (discourse-auth--job-timer job)) (cancel-timer (discourse-auth--job-timer job)))
  (when (discourse-auth--active-p job) (remhash (discourse-auth--job-base job) discourse-auth--pending))
  (when (file-directory-p (discourse-auth--job-directory job))
    (delete-directory (discourse-auth--job-directory job) t))
  (discourse-auth--status job message)
  (message "%s" message))

(defun discourse-auth--poll (job)
  "Poll JOB once, scheduling further requests only while authorization is pending."
  (when (discourse-auth--active-p job)
    (if (>= (float-time) (discourse-auth--job-deadline job))
        (discourse-auth--finish job "Discourse authorization expired; run login again")
      (discourse-auth--request
       (discourse-auth--job-base job) 'post "/user-api-key/device/poll.json"
       `(("device_code" ,(discourse-auth--job-device job)))
       (lambda (data error)
         (when (discourse-auth--active-p job)
           (condition-case nil
               (cond
                (error (discourse-auth--finish job error))
                ((equal (plist-get data :status) "authorization_pending")
                 (setf (discourse-auth--job-timer job)
                       (run-at-time (discourse-auth--job-interval job) nil #'discourse-auth--poll job)))
                ((equal (plist-get data :status) "authorized")
                 (let ((credential (discourse-auth--decrypt job (plist-get data :payload)))
                       (base (discourse-auth--job-base job)))
                   (puthash base credential discourse-auth--credentials)
                   (discourse-auth--finish job "Discourse authorized for this Emacs session")
                   (when discourse-auth-save-function
                     (condition-case nil
                         (let ((status (discourse-auth-save base)))
                           (discourse-auth--status job (if (stringp status) status "Credential saved.")))
                       ((error quit)
                        (discourse-auth--status job "Credential persistence unfinished. Run M-x discourse-auth-save to retry; session authorization remains usable.")
                        (message "Authorized for this session; credential persistence unfinished"))))))
                (t (discourse-auth--finish job "Discourse authorization denied or expired")))
             (error (discourse-auth--finish job "Discourse authorization validation failed; run login again")))))))))

;;;###autoload
(defun discourse-auth-cancel (base)
  "Cancel a pending authorization for BASE."
  (interactive (list (completing-read "Cancel authorization: " (hash-table-keys discourse-auth--pending) nil t)))
  (when-let* ((job (gethash (discourse-auth-base base) discourse-auth--pending)))
    (discourse-auth--finish job "Discourse authorization cancelled")))

(defun discourse-auth--cancel-all ()
  "Clean up temporary key material when Emacs exits."
  (mapc #'discourse-auth-cancel (hash-table-keys discourse-auth--pending)))
(add-hook 'kill-emacs-hook #'discourse-auth--cancel-all)

;;;###autoload
(defun discourse-auth-login (base)
  "Authorize a Discourse BASE URL in the browser using a device code."
  (interactive "sDiscourse site URL: ")
  (setq base (discourse-auth-base base))
  (when (gethash base discourse-auth--pending) (user-error "Authorization is already pending for this site"))
  (let ((job (discourse-auth--new-job base)))
    (puthash base job discourse-auth--pending)
    (setf (discourse-auth--job-buffer job) (get-buffer-create (format "*Discourse authorization: %s*" base)))
    (with-current-buffer (discourse-auth--job-buffer job)
      (special-mode)
      (let ((inhibit-read-only t)) (erase-buffer) (insert "Discourse authorization\n" base "\nChecking device authorization support…\n")))
    (pop-to-buffer (discourse-auth--job-buffer job))
    (discourse-auth--request
     base 'head "/user-api-key/new" nil
     (lambda (headers error)
       (when (discourse-auth--active-p job)
         (if (or error (not (equal (cdr (assq 'auth-api-device-code headers)) "true")))
             (discourse-auth--finish job (or error "This forum does not advertise device authorization support"))
           (discourse-auth--request
            base 'post "/user-api-key/device.json"
            `(("application_name" "Emacs thread-reader") ("client_id" ,(discourse-auth--job-client job))
              ("nonce" ,(discourse-auth--job-nonce job)) ("scopes" ,(string-join discourse-auth-scopes ","))
              ("public_key" ,(discourse-auth--job-public job)) ("padding" "oaep"))
            (lambda (data error)
              (when (discourse-auth--active-p job)
                (condition-case nil
                    (if error (discourse-auth--finish job error)
                      (let ((url (plist-get data :verification_uri_with_request))
                            (code (plist-get data :user_code))
                            (device (plist-get data :device_code))
                            (expires (plist-get data :expires_in))
                            (interval (plist-get data :interval)))
                        (unless (and (stringp url)
                                     (string-match-p (concat "\\`" (regexp-quote base)
                                                             "/user-api-key/activate[?]request=[[:alnum:]_-]+\\'") url)
                                     (stringp code) (string-match-p "\\`[[:alnum:]-]+\\'" code)
                                     (stringp device) (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" device)
                                     (numberp expires) (> expires 0) (numberp interval) (< 0 interval 61))
                          (error "Invalid device response"))
                        (setf (discourse-auth--job-device job) device
                              (discourse-auth--job-deadline job) (+ (float-time) (min expires 1800))
                              (discourse-auth--job-interval job) (max 5 interval))
                        (discourse-auth--status job (format "Authorize Emacs thread-reader in your browser.\nDevice code: %s\n%s\n\nM-x discourse-auth-cancel cancels this request." code url))
                        (setf (discourse-auth--job-timer job) (run-at-time (max 5 interval) nil #'discourse-auth--poll job))
                        (browse-url url)))
                  (error (discourse-auth--finish job "Could not start device authorization; run login again"))))))))))))

(provide 'discourse-auth)
;;; discourse-auth.el ends here
