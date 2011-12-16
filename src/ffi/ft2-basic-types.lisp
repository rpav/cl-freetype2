(in-package :freetype2-types)

 ;; Util

(defun make-collected-foreign (type &optional (alloc-fn 'libc-calloc) (free-fn 'libc-free))
  (let* ((ptr (funcall alloc-fn (foreign-type-size type) 1))
         (wrapper (make-instance type)))
    (setf (fw-ptr wrapper) ptr)
    (freetype2-types:finalize wrapper (lambda () (funcall free-fn ptr)))
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
  "Make an FT-MATRIX given XX, XY, YX, and YY.  This may be passed directly
to SET-TRANSFORM, and may be more efficient than converting from native
forms."
  (let ((matrix (make-collected-foreign 'ft-matrix)))
    (setf (ft-matrix-xx matrix) xx)
    (setf (ft-matrix-xy matrix) xy)
    (setf (ft-matrix-yx matrix) yx)
    (setf (ft-matrix-yy matrix) yy)
    matrix))

(export 'make-matrix)

(defun make-vector (x y)
  "Make an FT-VECTOR given X and Y.  This may be passed directly
to SET-TRANSFORM, and may be more efficient than converting from native
forms."
  (let ((vector (make-collected-foreign 'ft-vector)))
    (setf (ft-vector-x vector) x)
    (setf (ft-vector-y vector) y)
    vector))

(export 'make-vector)

(defun convert-matrix (matrix)
  "Convert MATRIX into an FT-MATRIX pointer.  If it is already an FT-MATRIX,
simply return the pointer.  Otherwise it may be specified as a 2x2 array or
flat 4-element array.  Specifying NIL will return (NULL-POINTER), which is
also useful in some cases."
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
  "Convert VECTOR into an FT-VECTOR pointer.  If VECTOR is already an
FT-VECTOR, simply return the address.  Otherwise, VECTOR may be
specified as a flat 2-dimensional array.  Specifying NIL will
return (NULL-POINTER), which is also useful in some cases."
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
  "Convert an FT_26dot6 to a native float."
  (declare (type fixnum f))
  (coerce (/ f #x40) 'float))

(export 'ft-26dot6-to-float)

(defun ft-26dot6-to-int (f)
  "Convert an FT_26dot6 to a native integer.  This may be more efficient than
truncating the value returned by FT-26DOT6-TO-FLOAT."
  (declare (type fixnum f))
  (ash f -6))

(export 'ft-26dot6-to-int)

(defun ft-16dot16-to-float (f)
  "Convert an FT_16dot16 to a native float."
  (declare (type fixnum f))
  (coerce (/ f #x10000) 'float))

(export 'ft-16dot16-to-float)

 ;; Display

(defmethod print-object ((object ft-bbox) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "(~A,~A)-(~A,~A) {#x~8,'0X}"
            (ft-bbox-xmin object)
            (ft-bbox-ymin object)
            (ft-bbox-xmax object)
            (ft-bbox-ymax object)
            (pointer-address (fw-ptr object)))))
