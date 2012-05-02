;;;; cl-fsnotify-bsd.lisp

(in-package #:cl-fsnotify)

(defvar *kq*)
(defvar *dir-watches* nil)

(defun open-fsnotify ()
 (setf *kq* (cl-fsnotify-kqueue:open-kqueue)))

(defun close-fsnotify ()
 (cl-fsnotify-kqueue:close-kqueue *kq*)
 (setf *kq* nil)
 (setf *dir-watches* nil))

(defmethod add-watch ((path pathname))
  (let ((dir (first (directory path))))
    (cond ((and dir (null (pathname-name dir))) ; Directories have no name in pathname structure
      (add-directory dir)
      (let ((dirlist (get-files-in-directory dir))
            (current-watch (assoc (namestring dir) *dir-watches* :test #'string=)))
       (if current-watch
        (rplacd current-watch dirlist)
        (setf *dir-watches* (nconc *dir-watches* (list (cons (namestring dir) dirlist))))))
      (cl-fsnotify-kqueue:add-watch *kq* (namestring dir)))
     (t (cl-fsnotify-kqueue:add-watch *kq* (namestring path))))))
                        
(defmethod add-watch ((path string))
  (add-watch (pathname path)))

(defmethod del-watch ((path string))
  (let ((dir (first (directory (pathname path)))))
    (when (and dir (null (pathname-name dir))) ; Directories have no name in pathname structure
      (rem-directory dir)
      (setf *dir-watches* (remove path *dir-watches* :key #'car)))
    (cl-fsnotify-kqueue:del-watch *kq* path)))

(defmethod del-watch ((path pathname))
  (del-watch (namestring path)))

(defmethod get-all-fs-events ((event-namestring string))
  (let ((dir (first (directory (pathname event-namestring)))))
    (if (and dir (null (pathname-name dir)))
      (let* ((before-files (cdr (assoc event-namestring *dir-watches* :test #'string=)))
            (after-files (get-files-in-directory dir))
            (return-files (append
             (loop for new-file in (set-difference after-files before-files :test #'string= :key #'namestring)
                   collect (cons new-file :CREATE)
                   do (add-watch new-file))
             (loop for del-file in (set-difference before-files after-files :test #'string= :key #'namestring)
                   collect (cons del-file :DELETE)
                   do (del-watch del-file)))))
       (let ((dir-watch (assoc event-namestring *dir-watches* :test #'string=)))
        (when dir-watch
         (rplacd dir-watch after-files)))
       return-files)
      (when (probe-file event-namestring)     ; Event might be DELETE
        (list (cons (pathname event-namestring) :MODIFY))))))

(defun get-events ()
  (remove-if #'null
             (loop for event-namestring in (cl-fsnotify-kqueue:get-events *kq*)
                   append (get-all-fs-events event-namestring))))
