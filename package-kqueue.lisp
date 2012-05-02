;;;; package.lisp

(defpackage #:cl-fsnotify-kqueue
  (:use #:cl
        #:cffi
        #-bsd (error "Package cl-fsnotify-kqueue is supported on BSDs only."))
  (:export
    #:with-kqueue
    #:open-kqueue
    #:close-kqueue
    #:add-watch
    #:del-watch
    #:get-events))

