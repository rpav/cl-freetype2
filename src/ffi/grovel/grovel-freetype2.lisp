(in-package :freetype2-types)

(cc-flags #+darwin "-I/opt/local/include/freetype2"
          #+darwin "-I/opt/local/include/freetype2/freetype"
          #+darwin "-I/usr/local/include/freetype2"
          #+darwin "-I/usr/local/include/freetype2/freetype"
          #+freebsd "-I/usr/local/include/freetype2"
          #+freebsd "-I/usr/local/include/freetype2/freetype"
          #+(and windows x86-64) "-I/mingw64/include/freetype2"
          #+(and windows x86-64) "-Ic:/msys64/mingw64/include/freetype2"
          #+(and windows x86) "-I/mingw32/include/freetype2"
          #+(and windows x86) "-Ic:/msys32/mingw32/include/freetype2"
          #-darwin "-I/usr/include/freetype2"
          #-darwin "-I/usr/include/freetype2/freetype2"
          #-darwin "-I/usr/include/freetype2/freetype"
          #-darwin "-I/usr/include/freetype")

;; Use pkg-config (if available) to resolve include paths.
;; (The hard-coded paths above don't apply on all systems.)
(pkg-config-cflags "freetype2" :optional t)

(include "grovel-freetype.h")

(constant (+version-major+ "FREETYPE_MAJOR"))
(constant (+version-minor+ "FREETYPE_MINOR"))
(constant (+version-patch+ "FREETYPE_PATCH"))

 ;; C types
(ctype size_t "size_t")

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
;(ctype ft-error "FT_Error")
(ctype ft-fixed "FT_Fixed")
(ctype ft-pos "FT_Pos")
(ctype ft-fword "FT_FWord")
(ctype ft-ufword "FT_UFWord")
(ctype ft-f2dot14 "FT_F2Dot14")
(ctype ft-f26dot6 "FT_F26Dot6")

(cenum ft-glyph-format
       ((:none "FT_GLYPH_FORMAT_NONE"))
       ((:composite "FT_GLYPH_FORMAT_COMPOSITE"))
       ((:bitmap "FT_GLYPH_FORMAT_BITMAP"))
       ((:outline "FT_GLYPH_FORMAT_OUTLINE"))
       ((:plotter "FT_GLYPH_FORMAT_PLOTTER")))

 ;; Basic API
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

(cenum ft-size-request-type
       ((:nominal "FT_SIZE_REQUEST_TYPE_NOMINAL"))
       ((:real-dim "FT_SIZE_REQUEST_TYPE_REAL_DIM"))
       ((:bbox "FT_SIZE_REQUEST_TYPE_BBOX"))
       ((:cell "FT_SIZE_REQUEST_TYPE_CELL"))
       ((:scales "FT_SIZE_REQUEST_TYPE_SCALES"))
       ((:max "FT_SIZE_REQUEST_TYPE_MAX")))

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

 ;; Errors

(cenum ft-error
       ((:ok "FT_Err_Ok"))
       ((:cannot-open-resource "FT_Err_Cannot_Open_Resource"))
       ((:unknown-file-format "FT_Err_Unknown_File_Format"))
       ((:invalid-file-format "FT_Err_Invalid_File_Format"))
       ((:invalid-version "FT_Err_Invalid_Version"))
       ((:lower-module-version "FT_Err_Lower_Module_Version"))
       ((:invalid-argument "FT_Err_Invalid_Argument"))
       ((:unimplemented-feature "FT_Err_Unimplemented_Feature"))
       ((:invalid-table "FT_Err_Invalid_Table"))
       ((:invalid-offset "FT_Err_Invalid_Offset"))
       ((:array-too-large "FT_Err_Array_Too_Large"))
       ((:invalid-glyph-index "FT_Err_Invalid_Glyph_Index"))
       ((:invalid-character-code "FT_Err_Invalid_Character_Code"))
       ((:invalid-glyph-format "FT_Err_Invalid_Glyph_Format"))
       ((:cannot-render-glyph "FT_Err_Cannot_Render_Glyph"))
       ((:invalid-outline "FT_Err_Invalid_Outline"))
       ((:invalid-composite "FT_Err_Invalid_Composite"))
       ((:too-many-hints "FT_Err_Too_Many_Hints"))
       ((:invalid-pixel-size "FT_Err_Invalid_Pixel_Size"))
       ((:invalid-handle "FT_Err_Invalid_Handle"))
       ((:invalid-library-handle "FT_Err_Invalid_Library_Handle"))
       ((:invalid-driver-handle "FT_Err_Invalid_Driver_Handle"))
       ((:invalid-face-handle "FT_Err_Invalid_Face_Handle"))
       ((:invalid-size-handle "FT_Err_Invalid_Size_Handle"))
       ((:invalid-slot-handle "FT_Err_Invalid_Slot_Handle"))
       ((:invalid-charmap-handle "FT_Err_Invalid_CharMap_Handle"))
       ((:invalid-cache-handle "FT_Err_Invalid_Cache_Handle"))
       ((:invalid-stream-handle "FT_Err_Invalid_Stream_Handle"))
       ((:too-many-drivers "FT_Err_Too_Many_Drivers"))
       ((:too-many-extensions "FT_Err_Too_Many_Extensions"))
       ((:out-of-memory "FT_Err_Out_Of_Memory"))
       ((:unlisted-object "FT_Err_Unlisted_Object"))
       ((:cannot-open-stream "FT_Err_Cannot_Open_Stream"))
       ((:invalid-stream-seek "FT_Err_Invalid_Stream_Seek"))
       ((:invalid-stream-skip "FT_Err_Invalid_Stream_Skip"))
       ((:invalid-stream-read "FT_Err_Invalid_Stream_Read"))
       ((:invalid-stream-operation "FT_Err_Invalid_Stream_Operation"))
       ((:invalid-frame-operation "FT_Err_Invalid_Frame_Operation"))
       ((:nested-frame-access "FT_Err_Nested_Frame_Access"))
       ((:invalid-frame-read "FT_Err_Invalid_Frame_Read"))
       ((:raster-uninitialized "FT_Err_Raster_Uninitialized"))
       ((:raster-corrupted "FT_Err_Raster_Corrupted"))
       ((:raster-overflow "FT_Err_Raster_Overflow"))
       ((:raster-negative-height "FT_Err_Raster_Negative_Height"))
       ((:too-many-caches "FT_Err_Too_Many_Caches"))
       ((:invalid-opcode "FT_Err_Invalid_Opcode"))
       ((:too-few-arguments "FT_Err_Too_Few_Arguments"))
       ((:stack-overflow "FT_Err_Stack_Overflow"))
       ((:code-overflow "FT_Err_Code_Overflow"))
       ((:bad-argument "FT_Err_Bad_Argument"))
       ((:divide-by-zero "FT_Err_Divide_By_Zero"))
       ((:invalid-reference "FT_Err_Invalid_Reference"))
       ((:debug-opcode "FT_Err_Debug_OpCode"))
       ((:endf-in-exec-stream "FT_Err_ENDF_In_Exec_Stream"))
       ((:nested-defs "FT_Err_Nested_DEFS"))
       ((:invalid-coderange "FT_Err_Invalid_CodeRange"))
       ((:execution-too-long "FT_Err_Execution_Too_Long"))
       ((:too-many-function-defs "FT_Err_Too_Many_Function_Defs"))
       ((:too-many-instruction-defs "FT_Err_Too_Many_Instruction_Defs"))
       ((:table-missing "FT_Err_Table_Missing"))
       ((:horiz-header-missing "FT_Err_Horiz_Header_Missing"))
       ((:locations-missing "FT_Err_Locations_Missing"))
       ((:name-table-missing "FT_Err_Name_Table_Missing"))
       ((:cmap-table-missing "FT_Err_CMap_Table_Missing"))
       ((:hmtx-table-missing "FT_Err_Hmtx_Table_Missing"))
       ((:post-table-missing "FT_Err_Post_Table_Missing"))
       ((:invalid-horiz-metrics "FT_Err_Invalid_Horiz_Metrics"))
       ((:invalid-charmap-format "FT_Err_Invalid_CharMap_Format"))
       ((:invalid-ppem "FT_Err_Invalid_PPem"))
       ((:invalid-vert-metrics "FT_Err_Invalid_Vert_Metrics"))
       ((:could-not-find-context "FT_Err_Could_Not_Find_Context"))
       ((:invalid-post-table-format "FT_Err_Invalid_Post_Table_Format"))
       ((:invalid-post-table "FT_Err_Invalid_Post_Table"))
       ((:syntax-error "FT_Err_Syntax_Error"))
       ((:stack-underflow "FT_Err_Stack_Underflow"))
       ((:ignore "FT_Err_Ignore"))
       ((:no-unicode-glyph-name "FT_Err_No_Unicode_Glyph_Name"))
       ((:missing-startfont-field "FT_Err_Missing_Startfont_Field"))
       ((:missing-font-field "FT_Err_Missing_Font_Field"))
       ((:missing-size-field "FT_Err_Missing_Size_Field"))
       ((:missing-fontboundingbox-field "FT_Err_Missing_Fontboundingbox_Field"))
       ((:missing-chars-field "FT_Err_Missing_Chars_Field"))
       ((:missing-startchar-field "FT_Err_Missing_Startchar_Field"))
       ((:missing-encoding-field "FT_Err_Missing_Encoding_Field"))
       ((:missing-bbx-field "FT_Err_Missing_Bbx_Field"))
       ((:bbx-too-big "FT_Err_Bbx_Too_Big"))
       ((:corrupted-font-header "FT_Err_Corrupted_Font_Header"))
       ((:corrupted-font-glyphs "FT_Err_Corrupted_Font_Glyphs")))
