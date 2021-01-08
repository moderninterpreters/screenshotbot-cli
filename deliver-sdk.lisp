(in-package :cl-user)

(require :asdf)

(load "~/quicklisp/setup.lisp")

(defvar *root-dir* (make-pathname :name nil
                                  :type nil
                                  :defaults (merge-pathnames #P"../../"
                                                             *load-truename*)))

(pushnew *root-dir* ql:*local-project-directories*)


(set-default-character-element-type 'character)

(ql:quickload :screenshotbot.sdk)

(defun deliver-main ()
 (let ((output-file (cadddr system:*line-arguments-list*)))
   (delete-file output-file)
   (deliver 'screenshotbot-sdk:main output-file 5
            :keep-function-name t
            :keep-pretty-printer t
            :keep-lisp-reader t
            :keep-symbols `(system:pipe-exit-status)
            :packages-to-keep-symbol-names :all
            :multiprocessing t)))

(deliver-main)
(uiop:quit)
