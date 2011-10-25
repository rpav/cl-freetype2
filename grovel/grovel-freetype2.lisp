(in-package :freetype2)

(cc-flags "-I/usr/include/freetype2")
(include "grovel-freetype.h")

(constant (+version-major+ "FREETYPE_MAJOR"))
(constant (+version-minor+ "FREETYPE_MINOR"))
(constant (+version-patch+ "FREETYPE_PATCH"))

 ;; Basic Types
(ctype ft-byte "FT_Byte")
(ctype ft-bytes "FT_Bytes")
(ctype ft-char "FT_Char")
(ctype ft-int "FT_Int")
(ctype ft-uint "FT_UInt")
(ctype ft-uint16 "FT_UInt16")
(ctype ft-int32 "FT_Int32")
(ctype ft-uint32 "FT_UInt32")
(ctype ft-short "FT_Short")
(ctype ft-ushort "FT_UShort")
(ctype ft-long "FT_Long")
(ctype ft-ulong "FT_ULong")
(ctype ft-bool "FT_Bool")
(ctype ft-offset "FT_Offset")
(ctype ft-ptrdist "FT_PtrDist")
(ctype ft-string "FT_String")
(ctype ft-tag "FT_Tag")
(ctype ft-error "FT_Error")
(ctype ft-fixed "FT_Fixed")
(ctype ft-pointer "FT_Pointer")
(ctype ft-pos "FT_Pos")

(cstruct ft-vector "FT_Vector"
         (x "x" :type ft-pos)
         (y "y" :type ft-pos))

(cstruct ft-bbox "FT_BBox"
         (xmin "xMin" :type ft-pos)
         (ymin "yMin" :type ft-pos)
         (xmax "xMax" :type ft-pos)
         (ymax "yMax" :type ft-pos))

(cstruct ft-matrix "FT_Matrix"
         (xx "xx" :type ft-fixed)
         (xy "xy" :type ft-fixed)
         (yx "yx" :type ft-fixed)
         (yy "yy" :type ft-fixed))

(ctype ft-fword "FT_FWord")
(ctype ft-ufword "FT_UFWord")
(ctype ft-f2dot14 "FT_F2Dot14")

(cstruct ft-unitvector "FT_UnitVector"
         (x "x" :type ft-f2dot14)
         (y "y" :type ft-f2dot14))

(ctype ft-f26dot6 "FT_F26Dot6")

(cenum ft-pixel-mode
       ((:none "FT_PIXEL_MODE_NONE"))
       ((:mono "FT_PIXEL_MODE_MONO"))
       ((:gray "FT_PIXEL_MODE_GRAY"))
       ((:gray2 "FT_PIXEL_MODE_GRAY2"))
       ((:gray4 "FT_PIXEL_MODE_GRAY4"))
       ((:lcd "FT_PIXEL_MODE_LCD"))
       ((:lcd-v "FT_PIXEL_MODE_LCD_V"))
       ((:max "FT_PIXEL_MODE_MAX")))

(cstruct ft-bitmap "FT_Bitmap"
         (rows "rows" :type :int)
         (width "width" :type :int)
         (pitch "pitch" :type :int)
         (buffer "buffer" :type :pointer)
         (num-grays "num_grays" :type :short)
         (pixel-mode "pixel_mode" :type :char)
         (palette-mode "palette_mode" :type :char)
         (palette "palette" :type :pointer))

(cenum ft-glyph-format
       ((:none "FT_GLYPH_FORMAT_NONE"))
       ((:composite "FT_GLYPH_FORMAT_COMPOSITE"))
       ((:bitmap "FT_GLYPH_FORMAT_BITMAP"))
       ((:outline "FT_GLYPH_FORMAT_OUTLINE"))
       ((:plotter "FT_GLYPH_FORMAT_PLOTTER")))

(cstruct ft-data "FT_Data"
         (pointer "pointer" :type :pointer)
         (length "length" :type ft-int))

(ctype ft-generic-finalizer "FT_Generic_Finalizer")

(cstruct ft-generic "FT_Generic"
         (data "data" :type :pointer)
         (finalizer "finalizer" :type ft-generic-finalizer))

 ;; System Interface
(ctype ft-memory "FT_Memory")
(ctype ft-alloc-func "FT_Alloc_Func")
(ctype ft-free-func "FT_Free_Func")
(ctype ft-realloc-func "FT_Realloc_Func")

(cstruct ft-memoryrec "struct FT_MemoryRec_"
         (user "user" :type :pointer)
         (alloc "alloc" :type ft-alloc-func)
         (free "free" :type ft-free-func)
         (realloc "realloc" :type ft-realloc-func))

(ctype ft-stream "FT_Stream")

(cstruct ft-streamdesc "FT_StreamDesc"
         (value "value" :type :long)
         (pointer "pointer" :type :pointer))

(ctype ft-stream-iofunc "FT_Stream_IoFunc")
(ctype ft-stream-closefunc "FT_Stream_CloseFunc")

(cstruct ft-streamrec "FT_StreamRec"
         (base "base" :type :pointer)
         (size "size" :type :unsigned-long)
         (pos "pos" :type :unsigned-long)

         (descriptor "descriptor" :type ft-streamdesc)
         (pathname "pathname" :type ft-streamdesc)
         (read "read" :type ft-stream-iofunc)
         (close "close" :type ft-stream-closefunc)

         (memory "memory" :type ft-memory)
         (cursor "cursor" :type :pointer)
         (limit "limit" :type :pointer))

 ;; List Processing
(ctype ft-list "FT_List")
(ctype ft-listnode "FT_ListNode")

(cstruct ft-listrec "FT_ListRec"
         (head "head" :type ft-listnode)
         (tail "tail" :type ft-listnode))

(cstruct ft-listnoderec "FT_ListNodeRec"
         (prev "prev" :type ft-listnode)
         (next "next" :type ft-listnode)
         (data "data" :type :pointer))

(ctype ft-list-iterator "FT_List_Iterator")
(ctype ft-list-destructor "FT_List_Destructor")

 ;; Outline Processing

(cstruct ft-outline "FT_Outline"
         (n-contours "n_contours" :type :short)
         (n-points "n_points" :type :short)

         (points "points" :type :pointer)
         (tags "tags" :type :pointer)
         (contours "contours" :type :pointer)

         (flags "flags" :type :int))


 ;; Basic API
(ctype ft-library "FT_Library")
(ctype ft-face "FT_Face")
(ctype ft-size "FT_Size")
(ctype ft-glyphslot "FT_GlyphSlot")
(ctype ft-charmap "FT_CharMap")

(cenum ft-encoding
       ((:none "FT_ENCODING_NONE"))
       ((:ms-symbol "FT_ENCODING_MS_SYMBOL"))
       ((:unicode "FT_ENCODING_UNICODE"))
       ((:sjis "FT_ENCODING_SJIS"))
       ((:gb2312 "FT_ENCODING_GB2312"))
       ((:big5 "FT_ENCODING_BIG5"))
       ((:wansung "FT_ENCODING_WANSUNG"))
       ((:johab "FT_ENCODING_JOHAB"))
       ((:adobe-standard "FT_ENCODING_ADOBE_STANDARD"))
       ((:adobe-expert "FT_ENCODING_ADOBE_EXPERT"))
       ((:adobe-custom "FT_ENCODING_ADOBE_CUSTOM"))
       ((:adobe-latin-1 "FT_ENCODING_ADOBE_LATIN_1"))
       ((:old-latin-2 "FT_ENCODING_OLD_LATIN_2"))
       ((:apple-roman "FT_ENCODING_APPLE_ROMAN")))

(cstruct ft-glyph-metrics "FT_Glyph_Metrics"
         (width "width" :type ft-pos)
         (height "height" :type ft-pos)
         (hori-bearing-x "horiBearingX" :type ft-pos)
         (hori-bearing-y "horiBearingY" :type ft-pos)
         (hori-advance "horiAdvance" :type ft-pos)
         (vert-bearing-x "vertBearingX" :type ft-pos)
         (vert-bearing-y "vertBearingY" :type ft-pos)
         (vert-advance "vertAdvance" :type ft-pos))

(cstruct ft-bitmap-size "FT_Bitmap_Size"
         (height "height" :type ft-short)
         (width "width" :type ft-short)
         (size "size" :type ft-pos)
         (x-ppem "x_ppem" :type ft-pos)
         (y-ppem "y_ppem" :type ft-pos))

(ctype ft-module "FT_Module")
(ctype ft-driver "FT_Driver")
(ctype ft-renderer "FT_Renderer")

(cstruct ft-charmaprec "FT_CharMapRec"
         (face "face" :type ft-face)
         (encoding "encoding" :type ft-encoding)
         (platform-id "platform_id" :type ft-ushort)
         (encoding-id "encoding_id" :type ft-ushort))

(ctype ft-face-internal "FT_Face_Internal")

(cstruct ft-facerec "FT_FaceRec"
         (num-faces "num_faces" :type ft-long)
         (face-index "face_index" :type ft-long)

         (face-flags "face_flags" :type ft-long)
         (style-flags "style_flags" :type ft-long)

         (num-glyphs "num_glyphs" :type ft-long)

         (family-name "family_name" :type :string)
         (style-name "style_name" :type :string)

         (num-fixed-sizes "num_fixed_sizes" :type ft-int)
         (available-sizes "available_sizes" :type :pointer)

         (num-charmaps "num_charmaps" :type ft-int)
         (charmaps "charmaps" :type :pointer)

         (generic "generic" :type ft-generic)

         (bbox "bbox" :type ft-bbox)

         (units-per-em "units_per_EM" :type ft-ushort)
         (ascender "ascender" :type ft-short)
         (descender "descender" :type ft-short)
         (height "height" :type ft-short)

         (underline-position "underline_position" :type ft-short)
         (underline-thickness "underline_thickness" :type ft-short)

         (glyph "glyph" :type ft-glyphslot)
         (size "size" :type ft-size)
         (charmap "charmap" :type ft-charmap)

         (driver "driver" :type ft-driver)
         (memory "memory" :type ft-memory)
         (stream "stream" :type ft-stream)

         (sizes-list "sizes_list" :type ft-listrec)

         (autohint "autohint" :type ft-generic)
         (extensions "extensions" :type :pointer)

         (internal "internal" :type ft-face-internal))

(ctype ft-size-internal "FT_Size_Internal")

(cstruct ft-size-metrics "FT_Size_Metrics"
         (x-ppem "x_ppem" :type ft-ushort)
         (y-ppem "y_ppem" :type ft-ushort)
         (x-scale "x_scale" :type ft-fixed)
         (y-scale "y_scale" :type ft-fixed)
         (ascender "ascender" :type ft-pos)
         (descender "descender" :type ft-pos)
         (height "height" :type ft-pos)
         (max-advance "max_advance" :type ft-pos))

(cstruct ft-sizerec "FT_SizeRec"
         (face "face" :type ft-face)
         (generic "generic" :type ft-generic)
         (metrics "metrics" :type ft-size-metrics)
         (internal "internal" :type ft-size-internal))

(ctype ft-subglyph "FT_SubGlyph")
(ctype ft-slot-internal "FT_Slot_Internal")

(cstruct ft-glyphslotrec "FT_GlyphSlotRec"
         (library "library" :type ft-library)
         (face "face" :type ft-face)
         (next "next" :type ft-glyphslot)
         (reserved "reserved" :type ft-uint)
         (generic "generic" :type ft-generic)

         (metrics "metrics" :type ft-glyph-metrics)
         (linear-hori-advance "linearHoriAdvance" :type ft-fixed)
         (linear-vert-advance "linearVertAdvance" :type ft-fixed)
         (advance "advance" :type ft-vector)

         (format "format" :type ft-glyph-format)

         (bitmap "bitmap" :type ft-bitmap)
         (bitmap-left "bitmap_left" :type ft-int)
         (bitmap-top "bitmap_top" :type ft-int)

         (outline "outline" :type ft-outline)

         (num-subglyphs "num_subglyphs" :type ft-uint)
         (subglyphs "subglyphs" :type ft-subglyph)

         (control-data "control_data" :type :pointer)
         (control-len "control_len" :type :long)

         (lsb-delta "lsb_delta" :type ft-pos)
         (rsb-delta "rsb_delta" :type ft-pos)

         (other "other" :type :pointer)

         (internal "internal" :type ft-face-internal))

(cstruct ft-parameter "FT_Parameter"
         (tag "tag" :type ft-ulong)
         (data "data" :type ft-pointer))

(cstruct ft-open-args "FT_Open_Args"
         (flags "flags" :type ft-uint)
         (memory-base "memory_base" :type :pointer)
         (memory-size "memory_size" :type ft-long)
         (pathname "pathname" :type :pointer)
         (stream "stream" :type ft-stream)
         (driver "driver" :type ft-module)
         (num-params "num_params" :type ft-int)
         (params "params" :type ft-parameter))

(cenum ft-size-request-type
       ((:nominal "FT_SIZE_REQUEST_TYPE_NOMINAL"))
       ((:real-dim "FT_SIZE_REQUEST_TYPE_REAL_DIM"))
       ((:bbox "FT_SIZE_REQUEST_TYPE_BBOX"))
       ((:cell "FT_SIZE_REQUEST_TYPE_CELL"))
       ((:scales "FT_SIZE_REQUEST_TYPE_SCALES"))
       ((:max "FT_SIZE_REQUEST_TYPE_MAX")))

(cstruct ft-size-requestrec "FT_Size_RequestRec"
         (type "type" :type ft-size-request-type)
         (width "width" :type ft-long)
         (height "height" :type ft-long)
         (hori-resolution "horiResolution" :type ft-uint)
         (vert-resolution "vertResolution" :type ft-uint))

(ctype ft-size-request "FT_Size_Request")

(cenum ft-render-mode
       ((:normal "FT_RENDER_MODE_NORMAL"))
       ((:light "FT_RENDER_MODE_LIGHT"))
       ((:mono "FT_RENDER_MODE_MONO"))
       ((:lcd "FT_RENDER_MODE_LCD"))
       ((:lcd-v "FT_RENDER_MODE_LCD_V"))
       ((:max "FT_RENDER_MODE_MAX")))

(cenum ft-kerning-mode
       ((:default "FT_KERNING_DEFAULT"))
       ((:unfitted "FT_KERNING_UNFITTED"))
       ((:unscaled "FT_KERNING_UNSCALED")))


