(in-package :freetype2-ffi)

 ;; C: Base

(defcfun ("FT_Render_Glyph" ft-render-glyph) ft-error
  (slot ft-glyphslot)
  (render-mode ft-render-mode))

(export 'ft-render-glyph)

(defcfun ("FT_Get_SubGlyph_Info" ft-get-subglyph-info) ft-error
  (glyph ft-glyphslot)
  (sub-index ft-uint)
  (p-index (:pointer ft-int))
  (p-flags (:pointer ft-uint))
  (p-arg1 (:pointer ft-int))
  (p-arg2 (:pointer ft-int))
  (p-transform :pointer))

(export 'ft-get-subglyph-info)

 ;; C: Glyph Handling

(defcfun ("FT_Get_Glyph" ft-get-glyph) ft-error
  (slot ft-glyphslot)
  (aglyph (:pointer ft-glyph)))

(export 'ft-get-glyph)

(defcfun ("FT_Glyph_Copy" ft-glyph-copy) ft-error
  (source ft-glyph)
  (target (:pointer ft-glyph)))

(export 'ft-glyph-copy)

(defcfun ("FT_Glyph_Transform" ft-glyph-transform) ft-error
  (glyph ft-glyph)
  (matrix :pointer)
  (delta :pointer))

(export 'ft-glyph-transform)

(defcfun ("FT_Glyph_Get_CBox" ft-glyph-get-cbox) :void
  (glyph ft-glyph)
  (bbox-mode ft-glyph-bbox-mode)
  (acbox :pointer))

(export 'ft-glyph-get-cbox)

(defcfun ("FT_Glyph_To_Bitmap" ft-glyph-to-bitmap) ft-error
  (the-glyph (:pointer ft-glyph))
  (render-mode ft-render-mode)
  (origin :pointer)
  (destroy ft-bool))

(export 'ft-glyph-to-bitmap)

(defcfun ("FT_Done_Glyph" ft-done-glyph) :void
  (glyph :pointer))

(export 'ft-done-glyph)
