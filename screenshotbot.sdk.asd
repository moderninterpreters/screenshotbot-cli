(defsystem :screenshotbot.sdk
    :serial t
    :depends-on (:drakma
                 :com.google.flag
                 :ironclad
                 :cl-json
                 :log4cl
                 :cl-fad
                 :cxml
                 :zip
                 :trivial-garbage
                 :tmpdir
                 :imago
                 :imago/pngload
                 :md5
                 :anaphora
                 :str)
  :components ((:file "package")
               (:file "android")
               (:file "sdk")))
