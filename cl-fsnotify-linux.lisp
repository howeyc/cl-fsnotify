;;;; cl-fsnotify-linux.lisp

(in-package #:cl-fsnotify)

(defvar *inotify*)

(defun open-fsnotify ()
 "Create a file notification instance."
 (setf *inotify* (cl-fsnotify-inotify:open-inotify)))

(defun close-fsnotify ()
 "Close a file notification instance."
  (cl-fsnotify-inotify:close-inotify *inotify*)
  (setf *inotify* nil))

(defmethod add-watch ((path string))
  (let ((dir (first (directory (pathname path)))))
    (when (and dir (null (pathname-name dir))) ; Directories have no name in pathname structure
      (add-directory dir))
    (cl-fsnotify-inotify:add-watch *inotify* path)))
                        
(defmethod add-watch ((path pathname))
  (add-watch (namestring path)))

(defmethod del-watch ((path string))
  (let ((dir (first (directory (pathname path)))))
    (when (and dir (null (pathname-name dir))) ; Directories have no name in pathname structure
      (del-directory dir))
  (cl-fsnotify-inotify:del-watch *inotify* path)))

(defmethod del-watch ((path pathname))
  (del-watch (namestring path)))

(defun get-fsnotify-compatible-mask (masks)
  (cond ((intersection '(:in-delete :in-delete-self) masks) :DELETE)
        ((intersection '(:in-create) masks) :CREATE)
        (t :MODIFY)))

(defun get-events ()
 "Retrieve an alist of events that have occured for watched files.

 Each cons is of the form (EVENT-TYPE . PATHNAME).
 EVENT-TYPE is one of the following keywords :CREATE :MODIFY :DELETE
 PATHNAME is pathname of the file that triggered the event."
  (loop for (event-path . event-masks) in (cl-fsnotify-inotify:get-events *inotify*)
        collect (cons event-path (get-fsnotify-compatible-mask event-masks))))
