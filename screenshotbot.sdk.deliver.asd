(defpackage :screenshotbot.sdk.deliver
  (:use :cl :asdf))
(in-package :screenshotbot.sdk.deliver)

(defparameter *library-file-dir*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

(defclass deliver-script (source-file)
  ((type :initform "lisp")))

(defmethod output-files ((o compile-op) (s deliver-script))
  (list
   (make-pathname :name (funcall (find-symbol "REGEX-REPLACE-ALL" "CL-PPCRE")
                                 "^deliver-" (component-name s) "")
                  :type nil
                  :defaults *library-file-dir*)))

(defmethod perform ((o load-op) (s deliver-script))
  t)

(defun lw ()
  #+linux  "/opt/software/lispworks/lispworks-7-1-0-amd64-linux"
  #+darwin (car sys:*line-arguments-list*))

(defmethod perform ((o compile-op) (s deliver-script))
  (uiop:run-program (list (lw)
                          "-build"
                          (namestring
                           (merge-pathnames (format nil "~a.lisp" (component-name s))
                                            *library-file-dir*))
                          (namestring
                           (car (output-files o s))))
                    :output :interactive
                    :error-output :interactive))

(defsystem :screenshotbot.sdk.deliver
  :serial t
  :defsystem-depends-on (:cl-ppcre)
  :depends-on (:screenshotbot.sdk)
  :components ((deliver-script "deliver-sdk")))
