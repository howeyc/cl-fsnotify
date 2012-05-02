;;;; cl-fsnotify.asd

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel)
  (asdf:operate 'asdf:load-op 'cffi))

#+bsd
(asdf:defsystem #:cl-fsnotify
  :name "fsnotify"
  :version "0.2.0"
  :description "Filesystem notifications"
  :author "Chris Howey <chris@howey.me>"
  :license "ISC"
  :serial t
  :depends-on (#:cffi #:cffi-grovel)
  :components ((:file "package")
               (:file "package-kqueue")
               (cffi-grovel:grovel-file "grovel-kqueue")
               (:file "cl-fsnotify-kqueue")
               (:file "cl-fsnotify")
               (:file "cl-fsnotify-bsd")))


#+linux
(asdf:defsystem #:cl-fsnotify
  :name "fsnotify"
  :version "0.2.0"
  :description "Filesystem notifications"
  :author "Chris Howey <chris@howey.me>"
  :license "ISC"
  :serial t
  :depends-on (#:cl-inotify)
  :components ((:file "package")
               (:file "cl-fsnotify")
               (:file "cl-fsnotify-linux")))
