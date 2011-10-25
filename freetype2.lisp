(in-package :freetype2)

 ;; Loading

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library freetype2
    (t (:default "libfreetype")))

  (use-foreign-library freetype2))

 ;; Types

(defstruct (freetype-handle (:conc-name fth-)
                            (:constructor make-freetype-handle))
  (ptr (cffi-sys:null-pointer) :type cffi-sys:foreign-pointer))

(declaim (inline fth-deref))
(defun fth-deref (handle &optional (type :pointer))
  (mem-ref (fth-ptr handle) type))

(defmacro make-handle ((freetype-var freetype-type)
                       handle-form init-form free-form)
  (let ((err (gensym))
        (handle (gensym)))
    `(let* ((,freetype-var (foreign-alloc ',freetype-type))
            (,err ,init-form)
            (,handle ,handle-form))
            (if (eq ,err :ok)
                (progn
                  (setf (fth-ptr ,handle) ,freetype-var)
                  (tg:finalize ,handle (lambda ()
                                         #+-(format t "~A ~A~%" ',free-form ,freetype-var)
                                         ,free-form
                                         (foreign-free ,freetype-var))))
                (progn
                  (foreign-free ,freetype-var)
                  (error "FreeType error: ~A" ,err)))
       ,handle)))

 ;; Util

(defun auto-free (ptr &optional (free-fn 'cffi-sys:foreign-free))
  (let ((ptr-val (cffi-sys:pointer-address ptr)))
    (tg:finalize ptr (lambda ()
                       (funcall free-fn (cffi-sys:make-pointer ptr-val))))))

 ;; Initialization

(defstruct (freetype-library (:include freetype-handle)))

(defcfun ("FT_Init_FreeType" ft-init-freetype) ft-error
  (library (:pointer ft-library)))

(defcfun ("FT_Done_FreeType" ft-done-freetype) ft-error
  (library ft-library))

(defun make-freetype ()
  (make-handle (library ft-library)
    (make-freetype-library)
    (ft-init-freetype library)
    (ft-done-freetype (mem-ref library 'ft-library))))

(defvar *library* (make-freetype))
(export '*library*)

 ;; Faces

(defcfun ("FT_New_Face" ft-new-face) ft-error
  (library ft-library)
  (pathname :string)
  (face-index ft-long)
  (aface (:pointer ft-face)))

(defcfun ("FT_Done_Face" ft-done-face) ft-error
  (face ft-face))

(defun open-face (pathname &optional (index 0) (library *library*))
  (make-handle (face ft-face)
    (make-freetype-handle)
    (ft-new-face (fth-deref library) pathname index face)
    (ft-done-face (mem-ref face 'ft-face))))
