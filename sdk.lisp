(in-package :screenshotbot-sdk)

(defmacro defopt (var &key params
                        default
                        boolean
                        required
                        (help "undocumented"))
  (let ((params (or params
                    (unless boolean
                      `(list ,(str:replace-all "*" "" (string var)))))))
    `(list ',var ,default ,help :params ,params)))

(define-flag *directory*
  :default-value "./"
  :selector "directory"
  :type string
  :help "Directory of images")

(define-flag *org-defaults*
  :default-value nil
  :selector "defaults"
  :type (or null string))

(define-flag *api-key*
  :selector "api-key"
  :default-value nil
  :type (or null string)
  :help "Screenshotbot API Key")

(define-flag *create-github-issue*
  :selector "create-github-issue"
  :default-value t
  :type boolean
  :help "Create a Github issue if enabled on your account")

(define-flag *api-secret*
  :selector "api-secret"
  :default-value nil
  :type (or null string)
  :help "Screenshotbot API Secret")

(define-flag *hostname*
  :selector "api-hostname"
  :default-value "https://api.screenshotbot.io"
  :type string
  :help "Screenshotbot API Endpoing"
  :documentation "Only used for Enterprise User")

(define-flag *channel*
  :selector "channel"
  :default-value "unnamed-channel"
  :type string
  :help "Channel name for screenshot tracking")

(define-flag *branch*
  :selector "branch"
  :default-value "master"
  :type string
  :help "Git Branch name for screenshot tracking")


(define-flag *repo-url*
  :selector "repo-url"
  :default-value nil
  :type (or null string)
  :help "Repository URL (e.g. https://github.com/foo/bar)")

(define-flag *production*
  :selector "production"
  :default-value nil
  :type boolean)

(define-flag *help*
  :selector "help"
  :default-value nil
  :type boolean)

(define-flag *lang-regex*
  :selector "lang-regex"
  :default-value nil
  :type (or null string))

(define-flag *device-regex*
  :selector "device-regex"
  :default-value nil
  :type (or null string))

(define-flag *ios-multi-dir*
  :selector "ios-multi-dir"
  :default-value nil
  :type boolean)

(defclass model ()
  (type))

(defclass image (model)
  ((id :type string)
   (upload-url :type string)))

(define-condition api-error (error)
  ((message :initarg :message)))

(defmethod print-object ((e api-error) stream)
  (with-slots (message) e
   (format stream "#<API-ERROR ~a>" message)))

(defun request (api &key (method :post)
                      parameters)
  (with-open-stream (stream
                (drakma:http-request (format nil "~a~a" *hostname* api)
                       :want-stream t
                       :method method
                       :parameters (apply 'list
                                          (cons "api-key" *api-key*)
                                          (cons "api-secret-key" *api-secret*)
                                          parameters)))
    (let ((result (json:decode-json stream)))
      (awhen (assoc-value result :error)
        (error 'api-error :message it))
      (assoc-value result :response))))


(defun upload-image (key path)
  (log:debug "Checking to see if we need to re-upload ~a" path)
  (let* ((path (pathname path))
         (hash (ironclad:byte-array-to-hex-string (md5:md5sum-file path))))
    (let ((response (request "/api/screenshot"
                             :parameters `(("name" . ,key)
                                           ("hash" . ,hash)))))
      (log:debug "/api/screenshot response: ~s" response)
      (let ((upload-url (assoc-value response :upload-url)))
        (when upload-url
          (log:info "Uploading ~a" path)
          (with-open-file (s path :direction :input :element-type 'flexi-streams:octet)
           (multiple-value-bind (result code)
               (drakma:http-request upload-url
                                    :method :put
                                    :preserve-uri t
                                    :decode-content t
                                    :content-type "application/octet-stream"
                                    :force-binary t
                                    :content-length (file-length s)
                                    :content s)
             (log:info "Got image upload response: ~s" (flexi-streams:octets-to-string result))
             (unless (eql 200 code)
               (error "Failed to upload image: code ~a" code))))))
      (setf (assoc-value response :name) key)
      response)))

(defclass git-repo ()
  ((link :initarg :link
         :accessor repo-link)
   (dir :initarg :dir
        :accessor repo-dir
        :initform (git-root))))

(defmethod git-command ((repo git-repo))
  (list
   "git"
   "--git-dir"
   (namestring (path:catdir (repo-dir repo) ".git/"))))

(defmethod cleanp ((repo git-repo))
  (str:emptyp ($ (git-command repo) "status" "--porcelain")))


(defmethod current-commit ((repo git-repo))
  ($ (git-command repo) "rev-parse" "HEAD"))

(defun make-run (channel images &key repo
                                  branch
                                  create-github-issue
                                  metadata-provider
                                  is-trunk)
  (restart-case
   (flet ((bool (x) (if x "true" "false")))
     (let ((records (json:encode-json-to-string
                     (loop for im in images collect
                           (let ((name (assoc-value im :name)))
                            `(("name" . ,name)
                              ("imageId" . ,(assoc-value im :id))
                              ("lang" . ,(screenshot-lang metadata-provider name))
                              ("device" . ,(screenshot-device metadata-provider name))))))))
       (let ((response (request "/api/run"
                                :parameters `(("channel" . ,channel)
                                              ("screenshot-records" . ,records)
                                              ("branch" . ,branch)
                                              ("github-repo" . ,(repo-link repo))
                                              ("commit" . ,(current-commit repo))
                                              ("create-github-issue" . ,(bool create-github-issue))
                                              ("is-clean" . ,(bool (cleanp repo)))
                                              ("is-trunk" . ,(bool is-trunk))))))
         ;; what to do with response?
         response)))
    (retry-run ()
      (make-run channel images
                :metadata-provider metadata-provider
                :repo repo
                :branch branch
                :is-trunk is-trunk))))


(defun $! (&rest args)
  (multiple-value-bind (out error res)
      (uiop:run-program args
                        :error-output :interactive
                        :output :interactive
                        :ignore-error-status t)
    (declare (ignore out error))
    (eql 0 res)))

(defun $ (&rest args)
  (let ((out
         (uiop:run-program (flatten args)
                           :output 'string)))
    (str:trim out)))

(defun git-root ()
  (pathname (format nil "~a/"
                    ($ "git" "rev-parse" "--show-toplevel"))))

(defclass basic-directory-run ()
  ((directory :initarg :directory)))

(defclass metadata-provider ()
  ())

(defun read-first-match (regex name)
  (multiple-value-bind
        (full matches)
      (cl-ppcre:scan-to-strings regex name)
    (cond
      (full
       (elt matches 0))
      (t
       (log:debug "No regex match for ~a, ~a" regex name)))))

(defmethod screenshot-lang ((m metadata-provider) name)
  (when *lang-regex*
    (read-first-match *lang-regex* name)))

(defmethod screenshot-device ((m metadata-provider) name)
  (when *device-regex*
    (read-first-match *device-regex* name)))

(defun make-directory-run (dir &key channel
                                 branch
                                 create-github-issue
                                 (metadata-provider (make-instance 'metadata-provider))
                                 is-trunk
                                 repo)
  (log:debug "Reading images from ~a" dir)
  (let ((images
         (loop for im in (fad:list-directory dir)
               if (equal "png" (pathname-type im))
                 collect
                 (progn
                   (upload-image (pathname-name im) im)))))
    (log:info "Creating run")
    (make-run channel images :repo repo
              :create-github-issue create-github-issue
                             :metadata-provider metadata-provider
                             :branch branch
                             :is-trunk is-trunk)))

(defun test-upload ()
  (make-directory-run #P "/home/arnold/builds/ios-oss/Screenshots/_64/Kickstarter_Framework_iOSTests.ActivitiesViewControllerTests/"
                      :channel "test-channel"
                      :repo (make-instance 'git-repo
                                           :link "https://github.com/tdrhq/ios-oss"
                                           :dir #P "~/builds/ios-oss/")
                      :is-trunk t
                      :branch "master"))

(defun help ()
  (format t "Screenshotbot Recorder script~%"))

(defun prepare-directory (fn)
  (cond
    ((android-run-p)
     (log:info "Looks like an Android run")
     (prepare-android-directory fn))
    ((not (str:emptyp *directory*))
     (unless (path:-d *directory*)
       (error "Not a directory: ~a. Did you miss --metadata if you intended to use a bundle.zip?" *directory*))
     (funcall fn *directory*))
    (t
     (error "Unknown run type, maybe you missed --directory?"))))

(defun parse-org-defaults ()
  (when *org-defaults*
   (ecase (intern (str:upcase *org-defaults*) "KEYWORD")
     (nil
      nil)
     (:kickstarter-ios
      (setf *lang-regex* ".*_lang_(.*?)_.*")
      (setf *device-regex* ".*_device_(.*?)$")))))

(defun recursive-directories (directory)
  (or
   (loop for d in (fad:list-directory directory)
         if (and (not (str:starts-with-p "." (car (last (pathname-directory d)))))
                 (path:-d d))
           appending (recursive-directories d))
   (list directory)))


(defun main (&optional (argv #+lispworks system:*line-arguments-list*
                             #-lispworks nil))
  (Declare (ignore argv))
  (let ((unrecognized   (parse-command-line (cdr (command-line)))))
    (flet ((single-directory-run (directory &key channel)
             (log:info "Uploading images from: ~a" directory)
             (make-directory-run directory
                                 :channel channel
                                 :create-github-issue *create-github-issue*
                                 :repo (make-instance 'git-repo
                                                      :link *repo-url*)
                                 :is-trunk *production*
                                 :branch *branch*)))
      (cond
       (unrecognized
        (format t "Unrecognized arguments: ~a~%" (Str:join " " unrecognized))
        (help))
       (*help*
        (help))
       (*ios-multi-dir*
        (parse-org-defaults)
        (loop for directory in (recursive-directories *directory*)
              do
                 (progn
                   (log:info "Uploading run from ~a" directory)
                   (single-directory-run directory
                                         :channel (format nil "~a/~a"
                                                          *channel*
                                                          (car (last (pathname-directory directory))))))))
       (t
        (parse-org-defaults)
        (prepare-directory
         (lambda (directory)
           (single-directory-run directory :channel *channel*))))))))
