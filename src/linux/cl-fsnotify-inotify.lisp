;;;; cl-inotify.lisp

(in-package #:cl-fsnotify-inotify)

(defclass inotify-tracker ()
 ((fd :accessor inotify-fd
   :initarg :fd
   :initform 0)
  (path-hash 
   :documentation "Map path to watch descriptor"
   :accessor inotify-path-hash
   :initarg :path-hash
   :initform nil)
  (wd-hash 
   :documentation "Map watch descriptor to path"
   :accessor inotify-wd-hash
   :initarg :wd-hash
   :initform nil)
  (buffer
   :documentation "Read buffer"
   :accessor inotify-buffer
   :initarg :buffer
   :initform nil)))

(defbitfield (inotify-flag :uint32)
 (:in-access        #.in-access)
 (:in-modify        #.in-modify)
 (:in-attrib        #.in-attrib)
 (:in-close-write   #.in-close-write)
 (:in-close-nowrite #.in-close-nowrite)
 (:in-close         #.in-close)
 (:in-open          #.in-open)
 (:in-moved-from    #.in-moved-from)
 (:in-moved-to      #.in-moved-to)
 (:in-move          #.in-move)
 (:in-create        #.in-create)
 (:in-delete        #.in-delete)
 (:in-delete-self   #.in-delete-self)
 (:in-move-self     #.in-move-self)
 (:in-unmount       #.in-unmount)
 (:in-q-overflow    #.in-q-overflow)
 (:in-ignored       #.in-ignored)
 (:in-onlydir       #.in-onlydir)
 (:in-dont-follow   #.in-dont-follow)
 (:in-mask-add      #.in-mask-add)
 (:in-isdir         #.in-isdir)
 (:in-oneshot       #.in-oneshot)
 (:in-all-events    #.in-all-events))

(defcfun ("inotify_init1" c-inotify-init) :int
         "Creates a new inotify instance."
         (flags         :int))

(defcfun ("inotify_add_watch" c-inotify-add-watch) :int
         "Add watch."
         (fd            :int)
         (path          :string)
         (mask          :uint32))

(defcfun ("inotify_rm_watch" c-inotify-rm-watch) :int
         "Remove watch."
         (fd            :int)
         (wd            :int))

(defcstruct struct-inotify-event
            "inotify event structure"
            (wd         :int)
            (mask       :uint32)
            (cookie     :uint32)
            (len        :uint32)
            (name       :char))

(defconstant +buffer-size+ 4096)

(defun open-inotify ()
 (make-instance 'inotify-tracker
  :fd (c-inotify-init o-nonblock)
  :path-hash (make-hash-table :test 'equal)
  :wd-hash (make-hash-table :test 'equal)
  :buffer (foreign-alloc :char :count +buffer-size+)))

(defun close-inotify (inotify-instance)
 (let ((hash (inotify-wd-hash inotify-instance)))
  (maphash (lambda (wd path)
           (declare (ignore path))
           (c-inotify-rm-watch (inotify-fd inotify-instance) wd)
           (remhash wd hash)) hash)
  (foreign-free (inotify-buffer inotify-instance))
  (foreign-funcall "close" :int (inotify-fd inotify-instance) :int)))

(defun add-watch (inotify-instance path)
 (unless (or (null (probe-file path)) (gethash path (inotify-path-hash inotify-instance)))
  (let ((wd (c-inotify-add-watch (inotify-fd inotify-instance) path (logior in-create in-attrib in-modify in-delete in-delete-self))))
   (setf (gethash path (inotify-path-hash inotify-instance)) wd
         (gethash wd (inotify-wd-hash inotify-instance)) path))))

(defun del-watch (inotify-instance path)
 (let ((wd (gethash path (inotify-path-hash inotify-instance))))
  (when wd
   (c-inotify-rm-watch (inotify-fd inotify-instance) wd)
   (remhash path (inotify-path-hash inotify-instance))
   (remhash wd (inotify-wd-hash inotify-instance)))))

(defun get-events (inotify-instance)
  (let ((buffer (inotify-buffer inotify-instance))
        events-queue)
    (loop
      for read = (foreign-funcall "read" :int (inotify-fd inotify-instance) :pointer buffer :int +buffer-size+ :int)
      while (plusp read)
      do (loop
           with offset = 0
           with offset-buffer = (inc-pointer buffer offset)
           while (< offset read)
           do (with-foreign-slots ((wd mask len) offset-buffer (:struct struct-inotify-event))
                (let ((name (if (plusp len)
                                (foreign-string-to-lisp (foreign-slot-pointer offset-buffer '(:struct struct-inotify-event) 'name) :max-chars len)
                                "")))
                  (let ((result (cons (concatenate 'string (gethash wd (inotify-wd-hash inotify-instance)) name) (foreign-bitfield-symbols 'inotify-flag mask))))
                    (if events-queue
                        (let ((cons (list result)))
                          (setf (cddr events-queue) cons
                                (cdr events-queue) cons))
                        (let ((queue (list (list result))))
                          (setf (cdr queue) (car queue)
                                events-queue queue)))))
                (incf offset (+ raw-event-size len))
                (setf offset-buffer (inc-pointer buffer offset))))
      finally (return (car events-queue)))))

(defmacro with-inotify ((name) &body body)
 `(let ((,name (open-inotify)))
     (unwind-protect (progn ,@body)
      (close-inotify ,name))))
