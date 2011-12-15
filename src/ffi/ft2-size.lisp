(in-package :freetype2-ffi)

 ;; C: Size Functions

(defcfun ("FT_New_Size" ft-new-size) ft-error
  (face ft-face)
  (asize (:pointer ft-size)))

(export 'ft-new-size)

(defcfun ("FT_Done_Size" ft-done-size) ft-error
  (size :pointer))

(export 'ft-done-size)

(defcfun ("FT_Activate_Size" ft-activate-size) ft-error
  (size ft-size))

(export 'ft-activate-size)
