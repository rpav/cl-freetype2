(in-package :freetype2)

(defun new-size (face)
  "Make a new `FT_Size` object for `FACE`.  Note that it is not automatically
activated."
  (make-wrapper (size &size ft-size)
    (ft-new-size face &size)
    (ft-done-size (p* &size))))

(defun activate-size (size)
  "Make `SIZE` the active `FT_Size` for its parent face."
  (ft-error (ft-activate-size size)))
