(in-package :freetype2-ffi)

 ;; C: Base Functions

(defcfun ("FT_New_Face" ft-new-face) ft-error
  (library ft-library)
  (pathname :string)
  (face-index ft-long)
  (aface :pointer))

(export 'ft-new-face)

(defcfun ("FT_New_Memory_Face" ft-new-memory-face) ft-error
  (library ft-library)
  (file-base (:pointer ft-byte))
  (file-size ft-long)
  (face-index ft-long)
  (aface :pointer))

(export 'ft-new-memory-face)

(defcfun ("FT_Open_Face" ft-open-face) ft-error
  (library ft-library)
  (args :pointer)
  (face-index ft-long)
  (face :pointer))

(export 'ft-open-face)

(defcfun ("FT_Attach_File" attach-file) ft-error
  (face ft-face)
  (pathname :string))

(export 'attach-file)

;; NOT IMPLEMENTED: FT_Attach_Stream

(defcfun ("FT_Reference_Face" ft-reference-face) ft-error
  (face ft-face))

(export 'ft-reference-face)

(defcfun ("FT_Done_Face" ft-done-face) ft-error
  (face (:pointer ft-face)))

(export 'ft-done-face)

(defcfun ("FT_Select_Size" ft-select-size) ft-error
  (face ft-face)
  (strike-index ft-int))

(export 'ft-select-size)

(defcfun ("FT_Request_Size" ft-request-size) ft-error
  (face ft-face)
  (req ft-size-request))

(export 'ft-request-size)

(defcfun ("FT_Set_Char_Size" ft-set-char-size) ft-error
  (face ft-face)
  (char-width ft-f26dot6)
  (char-height ft-f26dot6)
  (horz-resolution ft-uint)
  (vert-resolution ft-uint))

(export 'ft-set-char-size)

(defcfun ("FT_Set_Pixel_Sizes" ft-set-pixel-sizes) ft-error
  (face ft-face)
  (pixel-width ft-uint)
  (pixel-height ft-uint))

(export 'ft-set-pixel-sizes)

(defcfun ("FT_Load_Glyph" ft-load-glyph) ft-error
  (face ft-face)
  (glyph-index ft-uint)
  (load-flags ft-load-flags))

(export 'ft-load-glyph)

;; Implemented natively: FT_Load_Char

(defcfun ("FT_Set_Transform" ft-set-transform) :void
  (face ft-face)
  (matrix :pointer)
  (delta :pointer))

(export 'ft-set-transform)

(declaim (inline ft-get-kerning))
(defcfun ("FT_Get_Kerning" ft-get-kerning) ft-error
  (face ft-face)
  (left-glyph ft-uint)
  (right-glyph ft-uint)
  (kern-mode ft-kerning-mode)
  (akerning :pointer))

(export 'ft-get-kerning)

(defcfun ("FT_Get_Track_Kerning" ft-get-track-kerning) ft-error
  (face ft-face)
  (point-size ft-fixed)
  (degree ft-int)
  (akerning (:pointer ft-fixed)))

(export 'ft-get-track-kerning)

(defcfun ("FT_Get_Glyph_Name" ft-get-glyph-name) ft-error
  (face ft-face)
  (glyph-index ft-uint)
  (buffer :pointer)
  (buffer-max ft-uint))

(export 'ft-get-glyph-name)

(defcfun ("FT_Get_Postscript_Name" get-postscript-name) :string
  "=> name-string

Get the PostScript name for the Type1 or TrueType font `FACE`."
  (face ft-face))

(export 'get-postscript-name)

(defcfun ("FT_Select_Charmap" select-charmap) ft-error
  (face ft-face)
  (encoding ft-encoding))

(export 'select-charmap)

(defcfun ("FT_Set_Charmap" ft-set-charmap) ft-error
  (face ft-face)
  (charmap ft-charmap))

(export 'ft-set-charmap)

(defcfun ("FT_Get_Charmap_Index" ft-get-charmap-index) ft-int
  (charmap ft-charmap))

(export 'ft-get-charmap-index)

(declaim (inline ft-get-char-index))
(defcfun ("FT_Get_Char_Index" ft-get-char-index) ft-uint
  (face ft-face)
  (charcode ft-ulong))

(export 'ft-get-char-index)

(defcfun ("FT_Get_First_Char" ft-get-first-char) ft-ulong
  (face ft-face)
  (agindex (:pointer ft-uint)))

(export 'ft-get-first-char)

(defcfun ("FT_Get_Next_Char" ft-get-next-char) ft-ulong
  (face ft-face)
  (char-code ft-ulong)
  (agindex (:pointer ft-uint)))

(export 'ft-get-next-char)

(defcfun ("FT_Get_Name_Index" get-name-index) ft-uint
  "=> index

Get the index for the glyph named `GLYPH-NAME` for `FACE`."
  (face ft-face)
  (glyph-name :string))

(export 'get-name-index)

(defcfun ("FT_Get_FSType_Flags" get-fstype-flags) ft-fstype-flags
  "=> flags

Get the fsType flags for `FACE`."
  (face ft-face))

(export 'get-fstype-flags)

 ;; C: Variant Functions

(defcfun ("FT_Face_GetCharVariantIndex" ft-face-getcharvariantindex)
    ft-uint
  (face ft-face)
  (charcode ft-ulong)
  (variant-selector ft-ulong))

(export 'ft-face-getcharvariantindex)

(defcfun ("FT_Face_GetCharVariantIsDefault" ft-face-getcharvariantisdefault)
    ft-int
  (face ft-face)
  (charcode ft-ulong)
  (variant-selector ft-ulong))

(export 'ft-face-getcharvariantisdefault)

(defcfun ("FT_Face_GetVariantSelectors" ft-face-getvariantselectors)
    (:pointer ft-uint32)
  (face ft-face))

(export 'ft-face-getvariantselectors)

(defcfun ("FT_Face_GetVariantsOfChar" ft-face-getvariantsofchar)
    (:pointer ft-uint32)
  (face ft-face)
  (charcode ft-ulong))

(export 'ft-face-getvariantsofchar)

(defcfun ("FT_Face_GetCharsOfVariant" ft-face-getcharsofvariant)
    (:pointer ft-uint32)
  (face ft-face)
  (variant-selector ft-ulong))

(export 'ft-face-getcharsofvariant)

 ;; Advance Functions

(defcfun ("FT_Get_Advance" ft-get-advance) ft-error
  (face ft-face)
  (gindex ft-uint)
  (load-flags ft-load-flags)
  (padvance (:pointer ft-fixed)))

(export 'ft-get-advance)

(defcfun ("FT_Get_Advances" ft-get-advances) ft-error
  (face ft-face)
  (start ft-uint)
  (count ft-uint)
  (load-flags ft-load-flags)
  (padvances (:pointer ft-fixed)))

(export 'ft-get-advances)
