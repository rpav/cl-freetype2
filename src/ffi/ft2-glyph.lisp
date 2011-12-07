(in-package :freetype2)

 ;; C: Base

(defcfun ("FT_Render_Glyph" ft-render-glyph) ft-error
  (slot ft-glyphslot)
  (render-mode ft-render-mode))

(defcfun ("FT_Get_SubGlyph_Info" ft-get-subglyph-info) ft-error
  (glyph ft-glyphslot)
  (sub-index ft-uint)
  (p-index (:pointer ft-int))
  (p-flags (:pointer ft-uint))
  (p-arg1 (:pointer ft-int))
  (p-arg2 (:pointer ft-int))
  (p-transform (:pointer ft-matrix)))

 ;; C: Glyph Handling

(defcfun ("FT_Get_Glyph" ft-get-glyph) ft-error
  (slot ft-glyphslot)
  (aglyph (:pointer ft-glyph)))

(defcfun ("FT_Glyph_Copy" ft-glyph-copy) ft-error
  (source ft-glyph)
  (target (:pointer ft-glyph)))

(defcfun ("FT_Glyph_Transform" ft-glyph-transform) ft-error
  (glyph ft-glyph)
  (matrix (:pointer ft-matrix))
  (delta (:pointer ft-vector)))

(defcfun ("FT_Glyph_Get_CBox" ft-glyph-get-cbox) :void
  (glyph ft-glyph)
  (bbox-mode ft-glyph-bbox-mode)
  (acbox (:pointer ft-bbox)))

(defcfun ("FT_Glyph_To_Bitmap" ft-glyph-to-bitmap) ft-error
  (the-glyph (:pointer ft-glyph))
  (render-mode ft-render-mode)
  (origin (:pointer ft-vector))
  (destroy ft-bool))

(defcfun ("FT_Done_Glyph" ft-done-glyph) :void
  (glyph ft-glyph))
