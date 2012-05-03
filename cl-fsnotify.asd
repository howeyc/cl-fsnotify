;;;; cl-fsnotify.asd

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel)
  (asdf:operate 'asdf:load-op 'cffi))

(asdf:defsystem #:cl-fsnotify
  :name "fsnotify"
  :version "0.2.0"
  :description "Filesystem notifications"
  :author "Chris Howey <chris@howey.me>"
  :license "ISC"
  :serial t
  :depends-on (#:cffi #:cffi-grovel)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                            #+(or bsd freebsd)
                            (:module "bsd"
                             :serial t
                             :components ((:file "package-kqueue")
                                          (cffi-grovel:grovel-file "grovel-kqueue")
                                          (:file "cl-fsnotify-kqueue")
                                          (:file "cl-fsnotify-bsd")))
                            #+linux
                            (:module "linux"
                             :serial t
                             :components ((:file "package-inotify")
                                          (cffi-grovel:grovel-file "grovel-inotify")
                                          (:file "cl-fsnotify-inotify")
                                          (:file "cl-fsnotify-linux")))
                            (:file "cl-fsnotify")))))
