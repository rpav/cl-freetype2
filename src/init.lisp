(in-package :freetype2)

(defun make-freetype ()
  (make-wrapper (library &library ft-library)
    (ft-init-freetype &library)
    (ft-done-freetype (p* &library))))

(export 'make-freetype)

(defvar *library* (make-freetype))
(export '*library*)
