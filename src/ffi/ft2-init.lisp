(in-package :freetype2-ffi)

 ;; Initialization

(defcfun ("FT_Init_FreeType" ft-init-freetype) ft-error
  (library (:pointer ft-library)))

(export 'ft-init-freetype)

(defcfun ("FT_Done_FreeType" ft-done-freetype) ft-error
  (library ft-library))

(export 'ft-done-freetype)
