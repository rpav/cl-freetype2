(in-package :freetype2-ffi)

 ;; C: Bitmap functions

(defcfun ("FT_Bitmap_New" ft-bitmap-new) :void
  (abitmap :pointer))

(export 'ft-bitmap-new)

(defcfun ("FT_Bitmap_Copy" ft-bitmap-copy) ft-error
  (library ft-library)
  (source :pointer)
  (target :pointer))

(export 'ft-bitmap-copy)

(defcfun ("FT_Bitmap_Embolden" ft-bitmap-embolden) ft-error
  (library ft-library)
  (bitmap :pointer)
  (x-strength ft-pos)
  (y-strength ft-pos))

(export 'ft-bitmap-embolden)

(defcfun ("FT_Bitmap_Convert" ft-bitmap-convert) ft-error
  (library ft-library)
  (source :pointer)
  (target :pointer)
  (alignment :int))

(export 'ft-bitmap-convert)

(defcfun ("FT_GlyphSlot_Own_Bitmap" ft-glyphslot-own-bitmap) ft-error
  (slot ft-glyphslot))

(export 'ft-glyphslot-own-bitmap)

(defcfun ("FT_Bitmap_Done" ft-bitmap-done) ft-error
  (library ft-library)
  (bitmap :pointer))

(export 'ft-bitmap-done)

