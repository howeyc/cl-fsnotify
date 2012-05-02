;;;; package.lisp

(defpackage #:cl-fsnotify-kqueue
  (:use #:cl
        #:cffi)
  (:export
    #:with-kqueue
    #:open-kqueue
    #:close-kqueue
    #:add-watch
    #:del-watch
    #:get-events))

