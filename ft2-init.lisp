(in-package :freetype2)

 ;; Initialization

(defcfun ("FT_Init_FreeType" ft-init-freetype) ft-error
  (library (:pointer ft-library)))

(defcfun ("FT_Done_FreeType" ft-done-freetype) ft-error
  (library :pointer))

(defun make-freetype ()
  (make-wrapper (library &library ft-library)
    (ft-init-freetype &library)
    (ft-done-freetype (p* &library))))

(defvar *library* (make-freetype))
(export '*library*)
