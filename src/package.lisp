;;;; package.lisp

(defpackage #:cl-fsnotify
  (:use #:cl)
  (:export
    #:open-fsnotify
    #:add-watch
    #:get-events
    #:del-watch
    #:close-fsnotify))

