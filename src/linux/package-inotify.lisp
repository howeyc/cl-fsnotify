;;;; package.lisp

(defpackage #:cl-fsnotify-inotify
  (:use #:cl
        #:cffi)
  (:export
    #:with-inotify
    #:open-inotify
    #:close-inotify
    #:add-watch
    #:del-watch
    #:get-events))

