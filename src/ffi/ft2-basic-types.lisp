(in-package :freetype2-types)

 ;; Util

(defun make-collected-foreign (type &optional (alloc-fn 'libc-calloc) (free-fn 'libc-free))
  (let* ((ptr (funcall alloc-fn (foreign-type-size type) 1))
         (wrapper (make-instance type)))
    (setf (fw-ptr wrapper) ptr)
    (tg:finalize ptr (lambda () (funcall free-fn ptr)))
    wrapper))

(export 'make-collected-foreign)

(defmacro ft-error (form &body cleanup)
  "Handle the value of FORM as a freetype return; if the value is not :OK,
raise an error, and run CLEANUP.  Otherwise, take no further action."
  (let ((vsym (gensym)))
    `(let ((,vsym ,form))
       (unless (eq :OK ,vsym)
         ,@cleanup
         (error "Freetype error: ~A" ,vsym)))))

(export 'ft-error)

 ;; Matrices and Vectors

(defun make-matrix (xx xy yx yy)
  (let ((matrix (make-collected-foreign 'ft-matrix)))
    (setf (ft-matrix-xx matrix) xx)
    (setf (ft-matrix-xy matrix) xy)
    (setf (ft-matrix-yx matrix) yx)
    (setf (ft-matrix-yy matrix) yy)
    matrix))

(export 'make-matrix)

(defun make-vector (x y)
  (let ((vector (make-collected-foreign 'ft-vector)))
    (setf (ft-vector-x vector) x)
    (setf (ft-vector-y vector) y)
    vector))

(export 'make-vector)

(defun convert-matrix (matrix)
  (etypecase matrix
    (ft-matrix (& matrix))
    ((array * (2 2))
     (& (make-matrix (aref matrix 0 0)
                     (aref matrix 0 1)
                     (aref matrix 1 0)
                     (aref matrix 1 1))))
    ((or (simple-vector 4)
         (array * (4)))
     (& (make-matrix (aref matrix 0) (aref matrix 1)
                     (aref matrix 2) (aref matrix 3))))
    (null (null-pointer))))

(export 'convert-matrix)

(defun convert-vector (vector)
  (etypecase vector
    (ft-vector (& vector))
    ((or (simple-vector 2)
         (array * 2))
     (& (make-vector (aref vector 0) (aref vector 1))))
    (null (null-pointer))))

(export 'convert-vector)

 ;; Fixed-point

(declaim (inline ft-26dot6-to-float
                 ft-26dot6-to-int
                 ft-16dot16-to-float))

(defun ft-26dot6-to-float (f)
  (declare (type fixnum f))
  (coerce (/ f #x40) 'float))

(export 'ft-26dot6-to-float)

(defun ft-26dot6-to-int (f)
  (declare (type fixnum f))
  (ash f -6))

(export 'ft-26dot6-to-int)

(defun ft-16dot16-to-float (f)
  (declare (type fixnum f))
  (coerce (/ f #x10000) 'float))

(export 'ft-16dot16-to-float)
