(in-package :freetype2-ffi)

 ;; Initialization

(defcfun ("FT_Init_FreeType" ft-init-freetype) ft-error
  (library (:pointer ft-library)))

(export 'ft-init-freetype)

;; This takes an FT_Library, which is natively a pointer, but if
;; we make it ft-library here, CFFI will expect a wrapper, which
;; is undesirable in the finalizer.
(defcfun ("FT_Done_FreeType" ft-done-freetype) ft-error
  (library :pointer))

(export 'ft-done-freetype)
