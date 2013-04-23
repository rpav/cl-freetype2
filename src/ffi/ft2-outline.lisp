(in-package :freetype2-ffi)

 ;; C: Outlines

(defcfun ("FT_Outline_New" ft-outline-new) ft-error
  (library ft-library)
  (num-points ft-uint)
  (num-contours ft-int)
  (anoutline :pointer))

(export 'ft-outline-new)

(defcfun ("FT_Outline_Done" ft-outline-done) ft-error
  (library ft-library)
  (outline :pointer))

(export 'ft-outline-done)

(defcfun ("FT_Outline_Copy" ft-outline-copy) ft-error
  (source :pointer)
  (target :pointer))

(export 'ft-outline-copy)

(defcfun ("FT_Outline_Translate" ft-outline-translate) :void
  (outline :pointer)
  (x-offset ft-pos)
  (y-offset ft-pos))

(export 'ft-outline-translate)

(defcfun ("FT_Outline_Transform" ft-outline-transform) :void
  (outline :pointer)
  (matrix :pointer))

(export 'ft-outline-transform)

(defcfun ("FT_Outline_Embolden" ft-outline-embolden) ft-error
  (outline :pointer)
  (strength ft-pos))

(export 'ft-outline-embolden)

(defcfun ("FT_Outline_Reverse" ft-outline-reverse) :void
  (outline :pointer))

(export 'ft-outline-reverse)

(defcfun ("FT_Outline_Check" ft-outline-check) ft-error
  (outline :pointer))

(export 'ft-outline-check)

(defcfun ("FT_Outline_Get_BBox" ft-outline-get-bbox) ft-error
  (outline :pointer)
  (abbox :pointer))

(export 'ft-outline-get-bbox)

(defcfun ("FT_Outline_Decompose" ft-outline-decompose) ft-error
  (outline :pointer)
  (func-interface :pointer)
  (user :pointer))

(export 'ft-outline-decompose)

(defcfun ("FT_Outline_Get_CBox" ft-outline-get-cbox) :void
  (outline :pointer)
  (acbox :pointer))

(export 'ft-outline-get-cbox)

;; Not implemented: FT_Outline_Get_Bitmap
;; Not implemented: FT_Outline_Render

(defcfun ("FT_Outline_Get_Orientation" ft-outline-get-orientation)
    ft-orientation
  (outline :pointer))

(export 'ft-outline-get-orientation)
