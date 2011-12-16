(in-package :freetype2)

(defun outline-copy (library outline)
  "Make a copy of OUTLINE.  Make sure LIBRARY is going to be valid for
the life of the resulting copy.  This means that if you got the value
from an FT_Face, FT_Glyph, or similar parent, THAT PARENT must be
valid, _along with_ the library itself, or use EXTRACT-LIBRARY to get
a fresh handle."
  (let* ((num-points (ft-outline-n-points outline))
         (num-contours (ft-outline-n-contours outline))
         (wrapper
           (make-wrapper (new-outline &new-outline ft-outline)
             (ft-outline-new library num-points num-contours &new-outline)
             (progn
               (format t "~&library: ~A~%" library)
               (ft-outline-done library &new-outline)))))
    (ft-error (ft-outline-copy (fw-ptr outline) (fw-ptr wrapper)))
    wrapper))

(export 'outline-copy)

(defun get-outline (face-or-glyphslot &optional char-or-code)
  "Retrieve an outline *copy* from FACE-OR-GLYPHSLOT.  If CHAR-OR-CODE is not
nil, it will be loaded, and its outline retrieved.  In this case,
FACE-OR-GLYPHSLOT must be an FT-FACE.

If you wish to retrieve the original outline, you may use GET-GLYPH and
FT-OUTLINEGLYPH-OUTLINE, but you *must* retain the glyph for the outline
to remain valid."
  (when char-or-code
    (load-char face-or-glyphslot char-or-code '(:no-bitmap)))
  (let ((glyph (get-glyph face-or-glyphslot)))
    (if (typep glyph 'ft-outlineglyph)
        (outline-copy (extract-freetype (ft-glyph-library glyph))
                      (ft-outlineglyph-outline glyph)))))

(export 'get-outline)

(defvar *decompose-callback*)

(defcallback cb-outline-moveto :int ((to (:pointer ft-vector)) (user :pointer))
  (declare (ignore user))
  (handler-case
      (funcall *decompose-callback* :moveto (convert-from-foreign to 'ft-vector)
               nil nil)
    (error () 1))
  0)

(defcallback cb-outline-lineto :int ((to (:pointer ft-vector)) (user :pointer))
  (declare (ignore user))
  (handler-case
      (funcall *decompose-callback* :lineto (convert-from-foreign to 'ft-vector)
               nil nil)
    (error () 1))
  0)

(defcallback cb-outline-conicto :int ((control (:pointer ft-vector))
                                      (to (:pointer ft-vector))
                                      (user :pointer))
  (declare (ignore user))
  (handler-case
      (funcall *decompose-callback* :conicto
               (convert-from-foreign to 'ft-vector)
               (convert-from-foreign control 'ft-vector)
               nil)
    (error () 1))
  0)

(defcallback cb-outline-cubicto :int ((control1 (:pointer ft-vector))
                                      (control2 (:pointer ft-vector))
                                      (to (:pointer ft-vector))
                                      (user :pointer))
  (declare (ignore user))
  (handler-case
      (funcall *decompose-callback* :cubicto
               (convert-from-foreign to 'ft-vector)
               (convert-from-foreign control1 'ft-vector)
               (convert-from-foreign control2 'ft-vector))
    (error () 1))
  0)

(defvar *outline-funcs*
  (let ((struct (make-collected-foreign 'ft-outline-funcs)))
    (setf (ft-outline-funcs-move-to struct) (callback cb-outline-moveto))
    (setf (ft-outline-funcs-line-to struct) (callback cb-outline-lineto))
    (setf (ft-outline-funcs-conic-to struct) (callback cb-outline-conicto))
    (setf (ft-outline-funcs-cubic-to struct) (callback cb-outline-cubicto))
    struct))

(defmacro do-outline-decompose (outline decompose-lambda &body body)
  "Iterate OUTLINE, passing the operation, a point, and up to two more
points to BODY.

DECOMPOSE-LAMBDA must support passing up to four arguments, e.g., (OP
POINT POINT2 POINT3), (OP POINT &rest POINTS), etc.  OP will be one
of :moveto, :lineto, :conicto, or :cubicto.  POINT will always be the
endpoint.  In the case of :conicto, POINT2 is the control point.  In
the case of :cubicto, POINT2 is the first control point, and POINT3
is the second control point."
    `(let ((freetype2::*decompose-callback*
             (lambda ,decompose-lambda ,@body)))
       (ft-error
           (ft-outline-decompose (fw-ptr ,outline)
                                 (fw-ptr freetype2::*outline-funcs*)
                                 (null-pointer)))))


(defmacro do-char-decompose ((face char-or-code) decompose-lambda &body body)
  "Load CHAR-OR-CODE in FACE, and iterate the outline, passing the operation,
a point, and up to two more points to BODY.  DECOMPOSE-LAMBDA should be
specified as per DO-OUTLINE-DECOMPOSE."
  (let ((outline-glyph (gensym))
        (outline (gensym)))
    `(progn
       (load-char ,face ,char-or-code '(:no-bitmap))
       (unless (eq :outline (ft-glyphslot-format (ft-face-glyph ,face)))
         (error "Unable to load outline data for ~A in ~A"
                ,char-or-code ,face))
       (let* ((,outline-glyph (get-glyph ,face))
              (,outline (ft-outlineglyph-outline ,outline-glyph)))
         (do-outline-decompose ,outline ,decompose-lambda ,@body)))))

(export 'do-outline-decompose)

(defun outline-translate (outline x y)
  (ft-outline-translate (fw-ptr outline) x y))

(export 'outline-translate)

(defun outline-transform (outline matrix)
  (ft-error
      (ft-outline-transform (fw-ptr outline) (convert-matrix matrix))))

(export 'outline-transform)

(defun outline-embolden (outline strength)
  (ft-error (ft-outline-embolden (fw-ptr outline) strength)))

(export 'outline-embolden)

(defun outline-reverse (outline)
  (ft-outline-reverse (fw-ptr outline)))

(export 'outline-reverse)

(defun outline-check (outline)
  (ft-error (ft-outline-check (fw-ptr outline))))

(export 'outline-check)

(defun outline-get-bbox (outline)
  (let ((bbox (make-collected-foreign 'ft-bbox)))
    (ft-error (ft-outline-get-bbox (fw-ptr outline) (fw-ptr bbox)))
    bbox))

(export 'outline-get-bbox)

(defun outline-get-cbox (outline)
  (let ((bbox (make-collected-foreign 'ft-bbox)))
    (ft-outline-get-cbox (fw-ptr outline) (fw-ptr bbox))
    bbox))

(export 'outline-get-cbox)

(defun outline-get-orientation (outline)
  (ft-outline-get-orientation (fw-ptr outline)))

(export 'outline-get-orientation)
