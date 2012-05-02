;;;; package.lisp

(defpackage #:cl-fsnotify-inotify
  (:use #:cl
        #:cffi
        #-linux (error "Package cl-fsnotify-inotify is supported on Linux only."))
  (:export
    #:with-inotify
    #:open-inotify
    #:close-inotify
    #:add-watch
    #:del-watch
    #:get-events))

