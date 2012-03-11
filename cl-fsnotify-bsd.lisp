;;;; cl-fsnotify-bsd.lisp

(in-package #:cl-fsnotify)

(defvar *kq*)
(defvar *dir-watches*)

(defun open-fsnotify ()
 (setf *kq* (cl-kqueue:open-kqueue))
 (setf *dir-watches* (make-hash-table :test 'equal)))

(defun close-fsnotify ()
 (cl-kqueue:close-kqueue *kq*)
 (setf *kq* nil)
 (setf *dir-watches* nil))

(defmethod add-watch ((path pathname))
  (let ((dir (first (directory path))))
    (when (and dir (null (pathname-name dir))) ; Directories have no name in pathname structure
      (add-directory dir))
    (cl-kqueue:add-watch *kq* (namestring path))))
                        
(defmethod add-watch ((path string))
  (add-watch (pathname path)))

(defmethod del-watch ((path string))
  (let ((dir (first (directory (pathname path)))))
    (when (and dir (null (pathname-name dir))) ; Directories have no name in pathname structure
      (rem-directory dir)
      (remhash path *dir-watches*))
    (cl-kqueue:del-watch *kq* path)))

(defmethod del-watch ((path pathname))
  (del-watch (namestring path)))

(defmethod get-all-fs-events ((event-namestring string))
  (let ((dir (first (directory (pathname event-namestring)))))
    (if (and dir (null (pathname-name dir)))
      (let ((before-files (gethash event-namestring *dir-watches*))
            (after-files (get-files-in-directory dir)))
        (append
          (loop for new-file in (set-difference after-files before-files :test #'equal :key #'namestring)
                collect (cons new-file :CREATE)
                do (add-watch new-file))
          (loop for del-file in (set-difference before-files after-files :test #'equal :key #'namestring)
                collect (cons del-file :DELETE))))
      (when (probe-file event-namestring)     ; Event might be DELETE
        (list (cons (pathname event-namestring) :MODIFY))))))

(defun get-events ()
  (remove-if #'null
             (loop for event-namestring in (cl-kqueue:get-events *kq*)
                   append (get-all-fs-events event-namestring))))
