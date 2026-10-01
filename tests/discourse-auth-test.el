;;; discourse-auth-test.el --- Authorization tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'discourse-auth)

(ert-deftest discourse-auth-test-scope-and-header-validation ()
  (let ((discourse-auth--credentials (make-hash-table :test #'equal))
        (auth-sources nil))
    (unwind-protect
        (progn
          (puthash "https://example.org/forum" '(:key "key" :client "client") discourse-auth--credentials)
          (should (discourse-auth-headers "https://EXAMPLE.org:443/forum/" t))
          (should-not (discourse-auth-headers "https://other.example/forum"))
          (should-not (discourse-auth-headers "https://example.org"))
          (should-error (discourse-auth-headers "https://example.org" t) :type 'user-error)
          (puthash "https://example.org" '(:key "key\r\nInjected: yes" :client "client") discourse-auth--credentials)
          (should-error (discourse-auth-headers "https://example.org"))
          (should-error (discourse-auth-base "https://user@example.org")))
      nil)))

(ert-deftest discourse-auth-test-real-rsa-oaep-and-nonce ()
  (skip-unless (executable-find discourse-auth-openssl-program))
  (let* ((job (discourse-auth--new-job "https://example.org"))
         (public (expand-file-name "public.pem" (discourse-auth--job-directory job))))
    (unwind-protect
        (progn
          (with-temp-file public (insert (discourse-auth--job-public job)))
          (dolist (nonce (list (discourse-auth--job-nonce job) "wrong-nonce"))
            (let* ((cipher (discourse-auth--openssl
                            (json-serialize (list :key "synthetic-key" :nonce nonce))
                            "pkeyutl" "-encrypt" "-pubin" "-inkey" public
                            "-pkeyopt" "rsa_padding_mode:oaep" "-pkeyopt" "rsa_oaep_md:sha1"))
                   (payload (base64-encode-string cipher t)))
              (if (equal nonce "wrong-nonce") (should-error (discourse-auth--decrypt job payload))
                (should (equal "synthetic-key" (plist-get (discourse-auth--decrypt job payload) :key)))))))
      (discourse-auth--finish job "Test complete")
      (should-not (file-exists-p (discourse-auth--job-directory job))))))

(ert-deftest discourse-auth-test-transport-failure-callback-once ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'plz) (lambda (&rest _) (error "No curl"))))
      (discourse-auth--request "https://example.org" 'head "/user-api-key/new" nil
                               (lambda (value failure)
                                 (cl-incf calls) (should-not value) (should failure))))
    (should (= calls 1))))

(ert-deftest discourse-auth-test-poll-completion-and-stale-callback ()
  (let* ((discourse-auth--pending (make-hash-table :test #'equal))
         (discourse-auth--credentials (make-hash-table :test #'equal))
         (job (make-discourse-auth--job :base "https://example.org"
                                       :directory (make-temp-file "discourse-test-" t)
                                       :deadline (+ (float-time) 60) :device "code"))
         (discourse-auth-save-function #'ignore)
         callback saved)
    (unwind-protect
        (cl-letf (((symbol-function 'discourse-auth--request)
                   (lambda (_base _method _path _fields cb) (setq callback cb)))
                  ((symbol-function 'discourse-auth--decrypt)
                   (lambda (_job _payload) '(:key "test-key" :client "test-client")))
                  ((symbol-function 'discourse-auth-save) (lambda (_) (setq saved t))))
          (puthash "https://example.org" job discourse-auth--pending)
          (discourse-auth--poll job)
          (funcall callback '(:status "authorized" :payload "cipher") nil)
          (should saved)
          (should (gethash "https://example.org" discourse-auth--credentials))
          (should-not (discourse-auth--active-p job))
          (should-not (file-directory-p (discourse-auth--job-directory job)))
          (setq saved nil)
          (funcall callback '(:status "authorized" :payload "cipher") nil)
          (should-not saved))
      (when (file-directory-p (discourse-auth--job-directory job))
        (delete-directory (discourse-auth--job-directory job) t)))))

(ert-deftest discourse-auth-test-device-flow-cancel-and-origin ()
  (dolist (foreign '(nil t))
    (let* ((discourse-auth--pending (make-hash-table :test #'equal))
           (directory (make-temp-file "discourse-test-" t))
           (job (make-discourse-auth--job :base "https://example.org" :directory directory
                                         :nonce "test" :client "test" :public "test"))
           (calls 0) opened)
      (save-window-excursion
        (unwind-protect
            (cl-letf (((symbol-function 'discourse-auth--new-job) (lambda (_) job))
                      ((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url)))
                      ((symbol-function 'discourse-auth--request)
                       (lambda (_base method _path _fields callback)
                         (cl-incf calls)
                         (funcall callback
                                  (if (eq method 'head) '((auth-api-device-code . "true"))
                                    (list :verification_uri_with_request
                                          (if foreign "https://evil.example/user-api-key/activate?request=abc"
                                            "https://example.org/user-api-key/activate?request=abc")
                                          :user_code "TEST-CODE" :device_code (make-string 64 ?a)
                                          :expires_in 600 :interval 5)) nil))))
              (discourse-auth-login "https://example.org")
              (should (= calls 2))
              (if foreign
                  (progn (should-not opened) (should-not (discourse-auth--active-p job)))
                (should opened)
                (should (timerp (discourse-auth--job-timer job)))
                (discourse-auth-cancel "https://example.org"))
              (should-not (file-directory-p directory)))
          (when (timerp (discourse-auth--job-timer job)) (cancel-timer (discourse-auth--job-timer job)))
          (when (file-directory-p directory) (delete-directory directory t))
          (when (buffer-live-p (discourse-auth--job-buffer job)) (kill-buffer (discourse-auth--job-buffer job))))))))

(ert-deftest discourse-auth-test-device-post-requires-json ()
  (cl-letf (((symbol-function 'plz)
             (lambda (method url &rest args)
               (should (eq method 'post))
               (should (equal url "https://example.org/user-api-key/device.json"))
               (should (equal "application/json" (cdr (assoc "Content-Type" (plist-get args :headers)))))
               (should-not (assoc "Cookie" (plist-get args :headers)))
               (should-not (member "--location" plz-curl-default-args))
               (should (equal "read,write" (plist-get (json-parse-string (plist-get args :body) :object-type 'plist) :scopes)))
               (funcall (plist-get args :then) (make-plz-response :status 200 :body "{}")))))
    (discourse-auth--request "https://example.org" 'post "/user-api-key/device.json"
                             '(("scopes" "read,write")) #'ignore)))

(ert-deftest discourse-auth-test-native-auth-sources ()
  (let* ((file (make-temp-file "discourse-authinfo-"))
         (auth-sources (list file))
         (auth-source-do-cache nil)
         (discourse-auth--credentials (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "machine https://example.org port discourse-user-api login client password synthetic-key\n"))
          (should (equal "synthetic-key" (cdr (assoc "User-Api-Key" (discourse-auth-headers "https://example.org")))))
          (should-not (discourse-auth-headers "https://other.example"))
          (should (equal (list file) auth-sources)))
      (delete-file file))))

(ert-deftest discourse-auth-test-save-delegates-without-gpg ()
  (let ((discourse-auth--credentials (make-hash-table :test #'equal))
        (discourse-auth-save-function nil) saved)
    (puthash "https://example.org" '(:key "synthetic-key" :client "client") discourse-auth--credentials)
    (should-error (discourse-auth-save "https://example.org") :type 'user-error)
    (setq discourse-auth-save-function
          (lambda (base credential) (setq saved (list base credential)) "Delegated"))
    (should (equal "Delegated" (discourse-auth-save "https://example.org")))
    (should (equal '("https://example.org" (:key "synthetic-key" :client "client")) saved))))
