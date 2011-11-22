(in-package :freetype2)

 ;; Util

(defun make-collected-foreign (type &optional (alloc-fn 'libc-calloc) (free-fn 'libc-free))
  (let* ((ptr (funcall alloc-fn (foreign-type-size type) 1))
         (wrapper (make-instance type)))
    (setf (fw-ptr wrapper) ptr)
    (tg:finalize ptr (lambda () (funcall free-fn ptr)))
    wrapper))

(defmacro ft-error (form &body body)
  "Handle the value of FORM as a freetype return; if the value is not :OK,
raise an error, and run BODY.  Otherwise, do nothing."
  (let ((vsym (gensym)))
    `(let ((,vsym ,form))
       (unless (eq :OK ,vsym)
         ,@body
         (error "Freetype error: ~A" ,vsym)))))

 ;; Types

(defun make-matrix (xx xy yx yy)
  (let ((matrix (make-collected-foreign 'ft-matrix)))
    (setf (ft-matrix-xx matrix) xx)
    (setf (ft-matrix-xy matrix) xy)
    (setf (ft-matrix-yx matrix) yx)
    (setf (ft-matrix-yy matrix) yy)
    matrix))

(defun make-vector (x y)
  (let ((vector (make-collected-foreign 'ft-vector)))
    (setf (ft-vector-x vector) x)
    (setf (ft-vector-y vector) y)
    vector))
