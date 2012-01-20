(in-package :freetype2-types)

 ;; Memory

(defcfun (libc-calloc "calloc") :pointer
  (nmemb size_t)
  (size size_t))

(export 'libc-calloc)

(defcfun (libc-free "free") :void
  (ptr :pointer))

(export 'libc-free)

(declaim (inline finalize))

#+(not ccl)
(defun finalize (object function)
  (tg:finalize object function))

#+(or ccl)
(progn
  (defvar *strong-finalizers* (make-hash-table))

  (defun finalize (object function)
    (setf (gethash function *strong-finalizers*) t)
    (tg:finalize object (lambda ()
                          (funcall function)
                          (remhash function *strong-finalizers*)))
    object))

(export 'finalize)

 ;; Wrapper Struct

(declaim (inline fw-ptr))

#+(or ccl ecl allegro)
(defstruct (foreign-wrapper (:conc-name #:fw-))
  (ptr #.(cffi:null-pointer) :type #.(type-of (cffi:null-pointer))))

#+(or cmucl sbcl clisp)
(defstruct (foreign-wrapper (:conc-name #:fw-))
  (ptr (cffi:null-pointer) :type #.(type-of (cffi:null-pointer))))

(export 'fw-ptr)

(declaim (inline w* w[] p* &))
(defun w* (wrapper)
  (mem-ref (fw-ptr wrapper) :pointer))

(defun w[] (wrapper index type-cast)
  (let* ((size (foreign-type-size type-cast)))
    (inc-pointer (fw-ptr wrapper) (* size index))))

(defun p* (ptr &optional (type-cast :pointer))
  (mem-ref ptr type-cast))

(defun & (wrapper) (fw-ptr wrapper))

(export '(w* w[] p* &))

(defmethod print-object ((object foreign-wrapper) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "{#X~8,'0X}" (pointer-address (fw-ptr object)))))

;; Apparently define-foreign-type _doesn't_ ensure that CLASS-NAME
;; becomes a subclass of FOREIGN-TYPE
(defclass wrapped-cffitype (cffi::enhanced-foreign-type) ())

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
      instance)))

(defmethod translate-to-foreign (wrapper (type pointer-to-type))
  (declare (ignore type))
  (if wrapper
      (fw-ptr wrapper)
      (null-pointer)))

 ;; CFFI Wrapper Macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-c-conc-name (symbol-name)
    (concatenate 'string "%" (string symbol-name) "-"))

  (defun make-s-conc-name (symbol-name)
    (concatenate 'string (string symbol-name) "-"))

  (defun make-handle-conc-name (handle-type)
    (if handle-type (concatenate 'string (string handle-type) "-")))

  (defun make-type-name (symbol-name)
    (intern (concatenate 'string (string symbol-name) "-CFFITYPE")))

  (defun make-foreign-name (symbol-name)
    (intern (concatenate 'string "FOREIGN-" (string symbol-name))))

  (defun make-make-name (symbol-name)
    (intern (concatenate 'string "%MAKE-" (string symbol-name))))

  (defun accessor-name (type slot)
    (intern (concatenate 'string (string type) "-" (string (car slot)))))

  (defun bitfield-test-name (type slot)
    (intern (concatenate 'string
                         (string type) "-" (string (car slot))
                         "-TEST")))

  (defun make-accessor (type slot handle-name)
    (let ((slot-name (car slot))
          (slot-type (cffi::parse-type (cadr slot)))
          (foreign-type-name (make-foreign-name type))
          (instance-form (if handle-name '(w* instance) '(fw-ptr instance)))
          (accessor-name (accessor-name (or handle-name type) slot)))
      (typecase slot-type
        (wrapped-cffitype
         `(progn
            (defun ,accessor-name (instance)
              (,(make-make-name (cadr slot))
               :ptr (foreign-slot-pointer ,instance-form
                                          ',foreign-type-name
                                          ',slot-name)))
            (defun (setf ,accessor-name) (v instance)
              (setf (foreign-slot-value ,instance-form
                                        ',foreign-type-name
                                        ',slot-name) v))
            (export ',accessor-name)))
        (pointer-to-type
         (let ((rec-type (pointer-type slot-type))
               (rec-fn (make-make-name (pointer-type slot-type))))
           `(progn
              (defun ,accessor-name (instance)
                (let* ((ptr0 (foreign-slot-value ,instance-form
                                                 ',foreign-type-name
                                                 ',slot-name))
                       (num (foreign-slot-value ,instance-form
                                                ',foreign-type-name
                                                ',(pointer-array-size slot-type)))
                       (arr (make-array num)))
                  (loop for i from 0 below num
                        as ptr = (w[] ptr0 i ',rec-type)
                        do (setf (elt arr i) (,rec-fn :ptr ptr)))
                  arr))
              (export ',accessor-name))))
        (cffi::foreign-bitfield
         (let ((test-name (bitfield-test-name (or handle-name type) slot))
               (canonical-type (cffi::canonicalize slot-type)))
           `(progn
              (defun ,accessor-name (instance)
                (foreign-slot-value ,instance-form ',foreign-type-name ',slot-name))
              (defun (setf ,accessor-name) (v instance)
                (setf (foreign-slot-value ,instance-form ',foreign-type-name ',slot-name) v))
              (defmacro ,test-name (instance flags)
                `(let ((instance ,instance))
                   (logtest (foreign-bitfield-value ',',(cffi::name slot-type) ,flags)
                            (mem-ref ,',instance-form ',',canonical-type ,(foreign-slot-offset ',foreign-type-name ',slot-name)))))
              (export ',accessor-name)
              (export ',test-name))))
        (t
         `(progn
            (defun ,accessor-name (instance)
              (foreign-slot-value ,instance-form ',foreign-type-name ',slot-name))
            (defun (setf ,accessor-name) (v instance)
              (setf (foreign-slot-value ,instance-form ',foreign-type-name ',slot-name) v))
            (export ',accessor-name))))))
  
  (defun make-accessors (type slots handle-name)
    (loop for slot in slots
          collecting (make-accessor type slot handle-name) into accessors
          finally (return accessors))))

(defmacro defcwrap (name slots &optional wrapper-slots)
  (let* ((handle-type (if (listp name) (cadr name) nil))
         (name (if (listp name) (car name) name))
         (symbol-str (symbol-name name)))
    (let ((type-name (make-type-name symbol-str))
          (foreign-name (make-foreign-name symbol-str))
          (make-name (make-make-name symbol-str)))
      `(progn
         ;; Structs
         (define-foreign-type ,type-name (wrapped-cffitype) ()
           (:actual-type ,foreign-name))
         (define-parse-method ,name () (make-instance ',type-name))
         (defcstruct (,foreign-name :conc-name ,(make-symbol (make-c-conc-name symbol-str)))
           ,@slots)
         (defstruct (,name (:include foreign-wrapper)
                           (:constructor ,make-name))
           ,@wrapper-slots)

         ;; Accessors
         ,@(make-accessors name slots nil)
         ,@(when handle-type (make-accessors name slots handle-type))

         ;; Translation
         (defmethod translate-to-foreign (wrapper (type ,type-name))
           (if wrapper (w* wrapper) (null-pointer)))
         (defmethod translate-from-foreign (ptr (type ,type-name))
           (unless (null-pointer-p ptr)
             (,make-name :ptr ptr)))

         (defmethod expand-to-foreign (wrapper (type ,type-name))
           `(if ,wrapper (w* ,wrapper) (null-pointer)))
         (defmethod expand-from-foreign (ptr (type ,type-name))
           `(unless (null-pointer-p ,ptr)
              (,',make-name :ptr ,ptr)))

         ;; Export
         (export ',name)
         (export ',type-name)
         (export ',foreign-name)
         (export ',make-name)))))

(defmacro defcwraptype (name type)
  (let ((symbol-str (symbol-name name)))
    (let ((make-name (make-make-name symbol-str))
          (foreign-name (make-foreign-name symbol-str))
          (type-name (make-type-name symbol-str)))
      `(progn
         (define-foreign-type ,type-name (wrapped-cffitype) ()
           (:actual-type ,foreign-name))
         (define-parse-method ,name () (make-instance ',type-name))
         (defctype ,foreign-name ,type)
         (defstruct (,name (:include foreign-wrapper)
                           (:constructor ,make-name)))

         (defmethod translate-to-foreign (wrapper (type ,type-name))
           (if wrapper (w* wrapper) (null-pointer)))
         (defmethod translate-from-foreign (ptr (type ,type-name))
           (unless (null-pointer-p ptr)
             (,make-name :ptr ptr)))
         (defmethod expand-to-foreign (wrapper (type ,type-name))
           `(if ,wrapper (w* ,wrapper) (null-pointer)))
         (defmethod expand-from-foreign (ptr (type ,type-name))
           `(unless (null-pointer-p ,ptr)
              (,',make-name :ptr ,ptr)))
         
         (export ',name)
         (export ',type-name)
         (export ',foreign-name)
         (export ',make-name)))))

(defmacro make-wrapper ((handle-var ptr-var foreign-type) init-form free-form)
  (let ((err (gensym))
        (make-name (make-make-name (symbol-name foreign-type)))
        (foreign-name (make-foreign-name (symbol-name foreign-type))))
    `(let* ((,ptr-var (libc-calloc (foreign-type-size ',foreign-name) 1))
            (,handle-var (,make-name :ptr ,ptr-var))
            (,err ,init-form))
            (if (eq ,err :ok)
                (freetype2-types::finalize ,handle-var
                             (lambda ()
                               #+-(format t "~A ~A~%" ',free-form ,ptr-var)
                               ,free-form
                               (libc-free ,ptr-var)))
                (progn
                  (libc-free ,ptr-var)
                  (setf (fw-ptr ,handle-var) (null-pointer))
                  (error "FreeType error: ~A" ,err)))
       ,handle-var)))

(export 'make-wrapper)
