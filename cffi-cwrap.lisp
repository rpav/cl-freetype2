(in-package :freetype2)

 ;; Memory

(defcfun (libc-calloc "calloc") :pointer
  (nmemb size_t)
  (size size_t))

(defcfun (libc-free "free") :void
  (ptr :pointer))

 ;; Wrapper Struct

(declaim (inline fw-ptr))
(defstruct (foreign-wrapper (:conc-name #:fw-))
  (ptr (cffi-sys:null-pointer) :type cffi-sys:foreign-pointer)
  (cffitype :pointer))

#+-(declaim (inline w* w[] p* &))
(defun w* (wrapper &optional (type-cast :pointer))
  (mem-ref (fw-ptr wrapper) type-cast))

(defun w[] (wrapper index &optional type-cast)
  (let* ((type (or type-cast (fw-cffitype wrapper)))
         (size (foreign-type-size type)))
    (inc-pointer (fw-ptr wrapper) (* size index))))

(defun p* (ptr &optional (type-cast :pointer))
  (mem-ref ptr type-cast))

(defun & (wrapper) (fw-ptr wrapper))

(defmethod print-object ((object foreign-wrapper) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "{#X~8,'0X}" (pointer-address (fw-ptr object)))))

 ;; POINTER-TO foreign type

(define-foreign-type pointer-to-type ()
  ((type :reader pointer-type :initarg :type)
   (array-size :reader pointer-array-size :initarg :array-size))
  (:actual-type :pointer))

(define-parse-method pointer-to (&key type array-size)
  (make-instance 'pointer-to-type :type type :array-size array-size))

(defmethod translate-from-foreign (ptr (type pointer-to-type))
  (unless (null-pointer-p ptr)
    (let* ((type (pointer-type type))
           (instance (make-instance type)))
      (setf (fw-ptr instance) ptr)
      (setf (fw-cffitype instance) type)
      instance)))

(defmethod translate-to-foreign (wrapper (type pointer-to-type))
  (declare (ignore type))
  (if wrapper
      (fw-ptr wrapper)
      (null-pointer)))

 ;; CFFI Wrapper Macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-accessor-defuns (accessor c-accessor instance-form)
    `((defun ,accessor (instance)
        (,c-accessor ,instance-form))
      (defun (setf ,accessor) (v instance)
        (setf (,c-accessor ,instance-form) v))
      (export ',accessor)))

  (defun make-array-accessors (accessor c-accessor instance-form num-accessor rec-fn rec-type)
    `((defun ,accessor (instance)
        (let* ((ptr0 (,c-accessor ,instance-form))
               (num (,num-accessor ,instance-form))
               (arr (make-array num)))
          (loop for i from 0 below num
                as ptr = (w[] ptr0 i)
                do (setf (elt arr i) (,rec-fn :ptr ptr :cffitype ',rec-type)))
          arr))))

  (defun make-c-conc-name (symbol-name)
    (concatenate 'string "%" symbol-name "-"))

  (defun make-s-conc-name (symbol-name)
    (concatenate 'string symbol-name "-"))

  (defun make-handle-conc-name (handle-type)
    (if handle-type (concatenate 'string (symbol-name handle-type) "-")))

  (defun make-type-name (symbol-name)
    (intern (concatenate 'string symbol-name "-CFFITYPE")))

  (defun make-foreign-name (symbol-name)
    (intern (concatenate 'string "FOREIGN-" symbol-name)))

  (defun make-make-name (symbol-name)
    (intern (concatenate 'string "%MAKE-" (string symbol-name))))

  (defun conc-name (name symbol)
    (intern (concatenate 'string name (string symbol)))))

(defmacro defcwrap (name slots &optional wrapper-slots)
  (let* ((handle-type (if (listp name) (cadr name) nil))
         (name (if (listp name) (car name) name))
         (symbol-name (symbol-name name)))
    (let ((c-conc-name (make-c-conc-name symbol-name))
          (s-conc-name (make-s-conc-name symbol-name))
          (handle-conc-name (make-handle-conc-name handle-type))
          (type-name (make-type-name symbol-name))
          (foreign-name (make-foreign-name symbol-name))
          (make-name (make-make-name symbol-name)))
      `(progn
         (define-foreign-type ,type-name () ()
           (:actual-type ,foreign-name))
         (define-parse-method ,name () (make-instance ',type-name))
         (defcstruct (,foreign-name :conc-name ,(make-symbol c-conc-name))
           ,@slots)
         (defstruct (,name (:include foreign-wrapper)
                           (:constructor ,make-name))
           ,@wrapper-slots)
         ,@(loop for slot in slots
                 as slot-name = (symbol-name (car slot))
                 as slot-type = (cffi::parse-type (cadr slot))
                 as is-array = (and (typep slot-type 'pointer-to-type)
                                    (not (null (pointer-array-size slot-type))))
                 as slot-accessor = (conc-name s-conc-name slot-name)
                 as c-accessor = (conc-name c-conc-name slot-name)
                 as handle-accessor = (when handle-type (conc-name handle-conc-name slot-name))
                 as num-accessor = (when is-array (conc-name c-conc-name (pointer-array-size slot-type)))
                 as rec-fn = (when is-array
                               (make-make-name (pointer-type slot-type)))
                 as rec-type = (when is-array (pointer-type slot-type))
                 collecting
                 `(progn
                    ,@(when handle-type
                        (if is-array
                            (make-array-accessors handle-accessor c-accessor '(w* instance)
                                                  num-accessor rec-fn rec-type)
                            (make-accessor-defuns handle-accessor c-accessor '(w* instance))))
                    ,@(if is-array
                          (make-array-accessors slot-accessor c-accessor '(fw-ptr instance)
                                                  num-accessor rec-fn rec-type)
                          (make-accessor-defuns slot-accessor c-accessor '(fw-ptr instance))))
                   into accessors
                 finally (return accessors))
         (defmethod expand-to-foreign (wrapper (type ,type-name))
           `(w* ,wrapper :pointer))
         (defmethod expand-from-foreign (ptr (type ,type-name))
           `(,',make-name :ptr ,ptr :cffitype ',',name))
         (defmethod translate-to-foreign (wrapper (type ,type-name))
           (w* wrapper :pointer))
         (defmethod translate-from-foreign (ptr (type ,type-name))
           (,make-name :ptr ptr :cffitype ',name))
         (export ',name)))))

(defmacro defcwraptype (name type)
  (let ((symbol-name (symbol-name name)))
    (let ((make-name (make-make-name symbol-name))
          (foreign-name (make-foreign-name symbol-name))
          (type-name (make-type-name symbol-name)))
      `(progn
         (define-foreign-type ,type-name () ()
           (:actual-type ,foreign-name))
         (define-parse-method ,name () (make-instance ',type-name))
         (defctype ,foreign-name ,type)
         (defstruct (,name (:include foreign-wrapper)
                           (:constructor ,make-name)))
         (defmethod expand-to-foreign (wrapper (type ,type-name))
           `(w* ,wrapper :pointer))
         (defmethod expand-from-foreign (ptr (type ,type-name))
           `(,',make-name :ptr ,ptr :cffitype ',',name))
         (defmethod translate-to-foreign (wrapper (type ,type-name))
           (w* wrapper :pointer))
         (defmethod translate-from-foreign (ptr (type ,type-name))
           (,make-name :ptr ptr :cffitype ',name))
         (export ',name)))))

(defmacro make-wrapper ((handle-var ptr-var foreign-type) init-form free-form)
  (let ((err (gensym))
        (make-name (make-make-name (symbol-name foreign-type)))
        (foreign-name (make-foreign-name (symbol-name foreign-type))))
    `(let* ((,ptr-var (libc-calloc (foreign-type-size ',foreign-name) 1))
            (,handle-var (,make-name :ptr ,ptr-var :cffitype ',foreign-type))
            (,err ,init-form))
            (if (eq ,err :ok)
                (tg:finalize ,handle-var (lambda ()
                                           (format t "~A ~A~%" ',free-form ,ptr-var)
                                           ,free-form
                                           (libc-free ,ptr-var)))
                (progn
                  (libc-free ,ptr-var)
                  (setf (fw-ptr ,handle-var) (null-pointer))
                  (error "FreeType error: ~A" ,err)))
       ,handle-var)))
