;;;; cl-kqueue.lisp

(in-package #:cl-fsnotify-kqueue)

(defclass kqueue-tracker ()
 ((fd :accessor kqueue-fd
   :initarg :fd
   :initform 0)
  (hash :accessor kqueue-hash
   :initarg :hash
   :initform nil)))

(defbitfield kevent-flags
 (:ev-add       #.kqueue-flag-add)
 (:ev-clear     #.kqueue-flag-clear)
 (:ev-delete    #.kqueue-flag-delete))

(defbitfield kevent-filter
 (:evfilt-vnode #.kqueue-fflag-vnode))

(defbitfield kevent-fflags
 (:note-delete  #.kqueue-note-delete)
 (:note-write   #.kqueue-note-write)
 (:note-extend  #.kqueue-note-extend)
 (:note-attrib  #.kqueue-note-attrib)
 (:note-link    #.kqueue-note-link)
 (:note-rename  #.kqueue-note-rename)
 (:note-revoke  #.kqueue-note-revoke))

(defcfun ("kqueue" c-kqueue) :int
         "Creates a new kernel event queue.")

(defcfun ("kevent" c-kevent) :int
         "Register and return events."
         (kq            :int)
         (changelist    :pointer)
         (nchanges      :int)
         (eventlist     :pointer)
         (nevents       :int)
         (timeout       :pointer))

(defcstruct struct-kevent
            "kevent structure"
            (ident      :unsigned-long)
            (filter     :short)
            (flags      :unsigned-short)
            (fflags     :unsigned-int)
            (data       :long)
            (udata      :pointer))

(defcstruct struct-timespec
 "timespec"
 (tv-sec :long)
 (tv-nsec :long))

(defun open-kqueue ()
 (make-instance 'kqueue-tracker :fd (c-kqueue) :hash (make-hash-table :test #'equal)))

(defun close-kqueue (kq)
 (let ((hash (kqueue-hash kq)))
  (maphash #'(lambda (path fd) 
             (cffi:foreign-funcall "close" :int fd :int)
             (remhash path hash)) hash)
  (cffi:foreign-funcall "close" :int (kqueue-fd kq) :int)))

(defun add-watch (kq path)
 (unless (null (probe-file path))
  (let ((fd (or (gethash path (kqueue-hash kq)) (cffi:with-foreign-string (c-path path) (cffi:foreign-funcall "open" :string c-path :int (logior open-read-only open-non-block) :int)))))
   (setf (gethash path (kqueue-hash kq)) fd)
   (with-foreign-object (kev 'struct-kevent)
     (psetf (foreign-slot-value kev 'struct-kevent 'ident) fd
            (foreign-slot-value kev 'struct-kevent 'filter) (foreign-bitfield-value 'kevent-filter '(:evfilt-vnode))
            (foreign-slot-value kev 'struct-kevent 'flags)  (foreign-bitfield-value 'kevent-flags  '(:ev-add :ev-clear))
            (foreign-slot-value kev 'struct-kevent 'fflags) (foreign-bitfield-value 'kevent-fflags '(:note-delete :note-write :note-extend :note-attrib :note-link :note-rename :note-revoke))
            (foreign-slot-value kev 'struct-kevent 'udata) (convert-to-foreign path :string))
     (c-kevent (kqueue-fd kq) kev 1 (null-pointer) 0 (null-pointer))))))

(defun del-watch (kq path)
 (let ((fd (gethash path (kqueue-hash kq))))
 (when fd
  (with-foreign-object (kev 'struct-kevent)
     (psetf (foreign-slot-value kev 'struct-kevent 'ident) (gethash path (kqueue-hash kq))
            (foreign-slot-value kev 'struct-kevent 'filter) (foreign-bitfield-value 'kevent-filter '(:evfilt-vnode))
            (foreign-slot-value kev 'struct-kevent 'flags)  (foreign-bitfield-value 'kevent-flags  '(:ev-delete))
            (foreign-slot-value kev 'struct-kevent 'fflags) (foreign-bitfield-value 'kevent-fflags '(:note-delete :note-write :note-extend :note-attrib :note-link :note-rename :note-revoke))
            (foreign-slot-value kev 'struct-kevent 'udata) (convert-to-foreign path :string))
     (c-kevent (kqueue-fd kq) kev 1 (null-pointer) 0 (null-pointer)))
  (remhash path (kqueue-hash kq))
  (cffi:foreign-funcall "close" :int fd :int))))

(defun get-event (kq-fd)
  (with-foreign-objects ((kev 'struct-kevent) (timespec 'struct-timespec))
     (psetf (foreign-slot-value timespec 'struct-timespec 'tv-sec) 0
            (foreign-slot-value timespec 'struct-timespec 'tv-nsec) 0)
     (when (= (c-kevent kq-fd (null-pointer) 0 kev 1 timespec) 1)
       (convert-from-foreign (foreign-slot-value kev 'struct-kevent 'udata) :string))))

(defun get-events (kq)
  (loop as event = (get-event (kqueue-fd kq))
        while event
        collect event))

(defmacro with-kqueue ((name) &body body)
 `(let ((,name (open-kqueue)))
     (unwind-protect (progn ,@body)
      (close-kqueue ,name))))
