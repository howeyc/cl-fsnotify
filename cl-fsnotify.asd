;;;; cl-fsnotify.asd

#+bsd
(asdf:defsystem #:cl-fsnotify
  :name "fsnotify"
  :version "0.1.0"
  :description "Filesystem notifications"
  :author "Chris Howey <chris@howey.me>"
  :license "ISC"
  :serial t
  :depends-on (#:cl-kqueue)
  :components ((:file "package")
               (:file "cl-fsnotify")
               (:file "cl-fsnotify-bsd")))


#+linux
(asdf:defsystem #:cl-fsnotify
  :name "fsnotify"
  :version "0.1.0"
  :description "Filesystem notifications"
  :author "Chris Howey <chris@howey.me>"
  :license "ISC"
  :serial t
  :depends-on (#:cl-inotify)
  :components ((:file "package")
               (:file "cl-fsnotify")
               (:file "cl-fsnotify-linux")))
