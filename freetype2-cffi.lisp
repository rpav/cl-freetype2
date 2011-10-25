(in-package :freetype2)

 ;; Basic Types

(defctype ft-pointer :pointer)

(defcstruct ft-vector "FT_Vector"
         (x ft-pos)
         (y ft-pos))

(defcstruct ft-bbox "FT_BBox"
         (xmin ft-pos)
         (ymin ft-pos)
         (xmax ft-pos)
         (ymax ft-pos))

(defcstruct ft-matrix "FT_Matrix"
         (xx ft-fixed)
         (xy ft-fixed)
         (yx ft-fixed)
         (yy ft-fixed))

(defcstruct ft-unitvector "FT_UnitVector"
         (x ft-f2dot14)
         (y ft-f2dot14))

(defcstruct ft-bitmap "FT_Bitmap"
         (rows :int)
         (width :int)
         (pitch :int)
         (buffer :pointer)
         (num-grays :short)
         (pixel-mode :char)
         (palette-mode :char)
         (palette :pointer))

(defcstruct ft-data "FT_Data"
         (pointer :pointer)
         (length ft-int))

(defctype ft-generic-finalizer :pointer)

(defcstruct ft-generic "FT_Generic"
         (data :pointer)
         (finalizer ft-generic-finalizer))

 ;; System Interface
(defctype ft-memory :pointer)
(defctype ft-alloc-func :pointer)
(defctype ft-free-func :pointer)
(defctype ft-realloc-func :pointer)

(defcstruct ft-memoryrec "struct FT_MemoryRec_"
         (user :pointer)
         (alloc ft-alloc-func)
         (free ft-free-func)
         (realloc ft-realloc-func))

(defctype ft-stream :pointer)

(defcstruct ft-streamdesc "FT_StreamDesc"
         (value :long)
         (pointer :pointer))

(defctype ft-stream-iofunc :pointer)
(defctype ft-stream-closefunc :pointer)

(defcstruct ft-streamrec "FT_StreamRec"
         (base :pointer)
         (size :unsigned-long)
         (pos :unsigned-long)
         (descriptor ft-streamdesc)
         (pathname ft-streamdesc)
         (read ft-stream-iofunc)
         (close ft-stream-closefunc)
         (memory ft-memory)
         (cursor :pointer)
         (limit :pointer))

 ;; List Processing
(defctype ft-list :pointer)
(defctype ft-listnode :pointer)

(defcstruct ft-listrec "FT_ListRec"
         (head ft-listnode)
         (tail ft-listnode))

(defcstruct ft-listnoderec "FT_ListNodeRec"
         (prev ft-listnode)
         (next ft-listnode)
         (data :pointer))

(defctype ft-list-iterator :pointer)
(defctype ft-list-destructor :pointer)

 ;; Outline Processing

(defcstruct ft-outline "FT_Outline"
         (n-contours :short)
         (n-points :short)
         (points :pointer)
         (tags :pointer)
         (contours :pointer)
         (flags :int))

 ;; Basic API
(defctype ft-library :pointer)
(defctype ft-face :pointer)
(defctype ft-size :pointer)
(defctype ft-glyphslot :pointer)
(defctype ft-charmap :pointer)
(defctype ft-module :pointer)
(defctype ft-driver :pointer)
(defctype ft-renderer :pointer)
(defctype ft-face-internal :pointer)
(defctype ft-size-internal :pointer)
(defctype ft-subglyph :pointer)
(defctype ft-slot-internal :pointer)
(defctype ft-size-request :pointer)

(defcstruct ft-glyph-metrics "FT_Glyph_Metrics"
         (width ft-pos)
         (height ft-pos)
         (hori-bearing-x ft-pos)
         (hori-bearing-y ft-pos)
         (hori-advance ft-pos)
         (vert-bearing-x ft-pos)
         (vert-bearing-y ft-pos)
         (vert-advance ft-pos))

(defcstruct ft-bitmap-size "FT_Bitmap_Size"
         (height ft-short)
         (width ft-short)
         (size ft-pos)
         (x-ppem ft-pos)
         (y-ppem ft-pos))

(defcstruct ft-charmaprec "FT_CharMapRec"
         (face ft-face)
         (encoding ft-encoding)
         (platform-id ft-ushort)
         (encoding-id ft-ushort))

(defcstruct ft-facerec "FT_FaceRec"
         (num-faces ft-long)
         (face-index ft-long)
         (face-flags ft-long)
         (style-flags ft-long)
         (num-glyphs ft-long)
         (family-name :string)
         (style-name :string)
         (num-fixed-sizes ft-int)
         (available-sizes :pointer)
         (num-charmaps ft-int)
         (charmaps :pointer)
         (generic ft-generic)
         (bbox ft-bbox)
         (units-per-em ft-ushort)
         (ascender ft-short)
         (descender ft-short)
         (height ft-short)
         (underline-position ft-short)
         (underline-thickness ft-short)
         (glyph ft-glyphslot)
         (size ft-size)
         (charmap ft-charmap)
         (driver ft-driver)
         (memory ft-memory)
         (stream ft-stream)
         (sizes-list ft-listrec)
         (autohint ft-generic)
         (extensions :pointer)
         (internal ft-face-internal))

(defcstruct ft-size-metrics "FT_Size_Metrics"
         (x-ppem ft-ushort)
         (y-ppem ft-ushort)
         (x-scale ft-fixed)
         (y-scale ft-fixed)
         (ascender ft-pos)
         (descender ft-pos)
         (height ft-pos)
         (max-advance ft-pos))

(defcstruct ft-sizerec "FT_SizeRec"
         (face ft-face)
         (generic ft-generic)
         (metrics ft-size-metrics)
         (internal ft-size-internal))

(defcstruct ft-glyphslotrec "FT_GlyphSlotRec"
         (library ft-library)
         (face ft-face)
         (next ft-glyphslot)
         (reserved ft-uint)
         (generic ft-generic)
         (metrics ft-glyph-metrics)
         (linear-hori-advance ft-fixed)
         (linear-vert-advance ft-fixed)
         (advance ft-vector)
         (format ft-glyph-format)
         (bitmap ft-bitmap)
         (bitmap-left ft-int)
         (bitmap-top ft-int)
         (outline ft-outline)
         (num-subglyphs ft-uint)
         (subglyphs ft-subglyph)
         (control-data :pointer)
         (control-len :long)
         (lsb-delta ft-pos)
         (rsb-delta ft-pos)
         (other :pointer)
         (internal ft-face-internal))

(defcstruct ft-parameter "FT_Parameter"
         (tag ft-ulong)
         (data ft-pointer))

(defcstruct ft-open-args "FT_Open_Args"
         (flags ft-uint)
         (memory-base :pointer)
         (memory-size ft-long)
         (pathname :pointer)
         (stream ft-stream)
         (driver ft-module)
         (num-params ft-int)
         (params ft-parameter))

(defcstruct ft-size-requestrec "FT_Size_RequestRec"
         (type ft-size-request-type)
         (width ft-long)
         (height ft-long)
         (hori-resolution ft-uint)
         (vert-resolution ft-uint))
