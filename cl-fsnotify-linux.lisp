;;;; cl-fsnotify-linux.lisp

(in-package #:cl-fsnotify)

(defvar *inotify*)

(defun open-fsnotify ()
 (setf *inotify* (cl-inotify:make-inotify)))

(defun close-fsnotify ()
  (cl-inotify:close-inotify *inotify*)
  (setf *inotify* nil))

(defmethod add-watch ((path string))
  (let ((dir (first (directory (pathname path)))))
    (when (and dir (null (pathname-name dir))) ; Directories have no name in pathname structure
      (add-directory dir))
    (cl-inotify:watch *inotify* path '(:create :modify :delete :delete-self))))
                        
(defmethod add-watch ((path pathname))
  (add-watch (namestring path)))

(defmethod del-watch ((path string))
  (let ((dir (first (directory (pathname path)))))
    (when (and dir (null (pathname-name dir))) ; Directories have no name in pathname structure
      (rem-directory dir))
  (cl-inotify:unwatch *inotify* path)))

(defmethod del-watch ((path pathname))
  (del-watch (namestring path)))

(defun get-fsnotify-compatible-mask (masks)
  (let ((delete-masks (intersection '(:DELETE :DELETE-SELF) masks)))
    (if delete-masks
      :DELETE
      (let ((compatible-masks (intersection '(:CREATE :DELETE :MODIFY) masks)))
        (if compatible-masks
          (first compatible-masks)
          :MODIFY)))))

(defun get-events ()
  (let ((results nil))
    (loop for event in (cl-inotify:next-events *inotify*)
          collect (cons 
                    (concatenate 'string (cl-inotify:event-pathname/flags *inotify* event) (cl-inotify:inotify-event-name event))
                    (get-fsnotify-compatible-mask (cl-inotify:inotify-event-mask event))))))
