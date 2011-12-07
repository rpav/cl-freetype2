(in-package :freetype2)

 ;; C: Outlines

(defcfun ("FT_Outline_New" ft-outline-new) ft-error
  (library ft-library)
  (num-points ft-uint)
  (num-contours ft-int)
  (anoutline (:pointer ft-outline)))

(defcfun ("FT_Outline_Done" ft-outline-done) ft-error
  (library ft-library)
  (outline (:pointer ft-outline)))

(defcfun ("FT_Outline_Copy" ft-outline-copy) ft-error
  (source (:pointer ft-outline))
  (target (:pointer ft-outline)))

(defcfun ("FT_Outline_Translate" ft-outline-translate) :void
  (outline (:pointer ft-outline))
  (x-offset ft-pos)
  (y-offset ft-pos))

(defcfun ("FT_Outline_Transform" ft-outline-transform) :void
  (outline (:pointer ft-outline))
  (matrix (:pointer ft-matrix)))

(defcfun ("FT_Outline_Embolden" ft-outline-embolden) ft-error
  (outline (:pointer ft-outline))
  (strength ft-pos))

(defcfun ("FT_Outline_Reverse" ft-outline-reverse) :void
  (outline (:pointer ft-outline)))

(defcfun ("FT_Outline_Check" ft-outline-check) ft-error
  (outline (:pointer ft-outline)))

(defcfun ("FT_Outline_Get_BBox" ft-outline-get-bbox) ft-error
  (outline (:pointer ft-outline))
  (abbox (:pointer ft-bbox)))

(defcfun ("FT_Outline_Decompose" ft-outline-decompose) ft-error
  (outline (:pointer ft-outline))
  (func-interface (:pointer ft-outline-funcs))
  (user :pointer))

(defcfun ("FT_Outline_Get_CBox" ft-outline-get-cbox) :void
  (outline (:pointer ft-outline))
  (acbox (:pointer ft-bbox)))

;; Not implemented: FT_Outline_Get_Bitmap
;; Not implemented: FT_Outline_Render

(defcfun ("FT_Outline_Get_Orientation" ft-outline-get-orientation)
    ft-orientation
  (outline (:pointer ft-outline)))
