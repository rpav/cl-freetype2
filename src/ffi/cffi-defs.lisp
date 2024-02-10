(in-package :freetype2-types)

 ;; Exports from groveller

(export '(+version-major+ +version-minor+ +version-patch+
          size_t ft-byte ft-bytes ft-char ft-int ft-uint
          ft-uint16 ft-int32 ft-uint32 ft-short ft-ushort ft-long
          ft-ulong ft-bool ft-offset ft-ptrdist ft-string ft-tag
          ft-fixed ft-pos ft-fword ft-ufword ft-f2dot14 ft-f26dot6

          ft-glyph-format ft-encoding ft-size-request-type
          ft-render-mode ft-kerning-mode ft-error))

 ;; Bit fields

(defbitfield (ft-load-flags ft-int32)
  (:default #x0)
  :no-scale :no-hinting :render :no-bitmap :vertical-layout :force-autohint
  :crop-bitmap :pedantic

  (:ignore-global-advance-width #x200)
  :no-recurse :ignore-transform

  :monochrome :linear-design

  (:no-autohint #x8000)
  (:target-mono #x20000)

  ;; Only valid for ft-get-advance / ft-get-advances
  (:fast-advance-only #x20000000))

(export 'ft-load-flags)

(defbitfield (ft-face-flags ft-long)
  (:scalable #x1)
  :fixed-sizes :fixed-width :sfnt
  :horizontal :vertical :kerning
  :fast-glyphs :multiple-masters
  :glyph-names :external-stream
  :hinter :cid-keyed :tricky)

(export 'ft-face-flags)

(defbitfield (ft-style-flags ft-long)
  (:italic #x1) :bold)

(export 'ft-style-flags)

(defbitfield (ft-fstype-flags ft-ushort)
  (:installable-embedding #x00)
  (:restricted-license-embedding #x02)
  (:preview-and-print-embedding #x04)
  (:editable-embedding #x08)
  (:no-subsetting #x0100)
  (:bitmap-embedding-only #x0200))

(export 'ft-fstype-flags)

(defbitfield (ft-outline-flags :int)
  (:none #x0) :owner :even-odd-file :reverse-fill :ignore-dropouts
  :smart-dropouts :include-stubs

  (:high-precision #x100) :single-pass)

(export 'ft-outline-flags)

(defbitfield (ft-open-flags ft-uint)
  (:memory #x1) :stream :pathname :driver :params)

(export 'ft-open-flags)

 ;; Enums

(defcenum (ft-pixel-mode :char)
  :none :mono :gray :gray2 :gray4 :lcd :lcd-v)

(export 'ft-pixel-mode)

(defcenum (ft-glyph-bbox-mode ft-uint)
  (:none 0)
  (:subpixels 0)
  (:gridfit 1)
  (:truncate 2)
  (:pixels 3))

(export 'ft-glyph-bbox-mode)

(defcenum ft-orientation
  (:truetype 0)
  (:postscript 1)
  (:fill-right 0)
  (:fill-left 1)
  (:none))

(export 'ft-orientation)

 ;; Basic Types

(defcwraptype ft-pointer :pointer)

(declaim (inline %ft-vector-x %ft-vector-y))
(defcwrap ft-vector
    ((x ft-pos)
     (y ft-pos)))

(defcwrap ft-bbox
    ((xmin ft-pos)
     (ymin ft-pos)
     (xmax ft-pos)
     (ymax ft-pos)))

(defcwrap ft-matrix
    ((xx ft-fixed)
     (xy ft-fixed)
     (yx ft-fixed)
     (yy ft-fixed)))

(defcwrap ft-unitvector
    ((x ft-f2dot14)
     (y ft-f2dot14)))

(defcwrap ft-bitmap
    ((rows :int)
     (width :int)
     (pitch :int)
     (buffer :pointer)
     (num-grays :short)
     (pixel-mode ft-pixel-mode)
     (palette-mode :char)
     (palette :pointer)))

(defcwrap ft-data
    ((pointer :pointer)
     (length ft-int)))

(defcwraptype ft-generic-finalizer :pointer)

(defcwrap ft-generic
    ((data :pointer)
     (finalizer ft-generic-finalizer)))

 ;; System Interface
(defcwraptype ft-memory :pointer)
(defcwraptype ft-alloc-func :pointer)
(defcwraptype ft-free-func :pointer)
(defcwraptype ft-realloc-func :pointer)

(defcwrap (ft-memoryrec ft-memory)
    ((user :pointer)
     (alloc ft-alloc-func)
     (free ft-free-func)
     (realloc ft-realloc-func)))

(defcwraptype ft-stream :pointer)

(defcwrap ft-streamdesc
    ((value :long)
     (pointer :pointer)))

(defcwraptype ft-stream-iofunc :pointer)
(defcwraptype ft-stream-closefunc :pointer)

(defcwrap (ft-streamrec ft-stream)
    ((base :pointer)
     (size :unsigned-long)
     (pos :unsigned-long)
     (descriptor (:struct foreign-ft-streamdesc))
     (pathname (:struct foreign-ft-streamdesc))
     (read ft-stream-iofunc)
     (close ft-stream-closefunc)
     (memory ft-memory)
     (cursor :pointer)
     (limit :pointer)))

 ;; List Processing
(defcwraptype ft-list :pointer)
(defcwraptype ft-listnode :pointer)

(defcwrap (ft-listrec ft-list)
    ((head ft-listnode)
     (tail ft-listnode)))

(defcwrap (ft-listnoderec ft-listnode)
    ((prev ft-listnode)
     (next ft-listnode)
     (data :pointer)))

(defcwraptype ft-list-iterator :pointer)
(defcwraptype ft-list-destructor :pointer)

 ;; Outline Processing

(defcwrap ft-outline
    ((n-contours :short)
     (n-points :short)
     (points :pointer)
     (tags :pointer)
     (contours :pointer)
     (flags ft-outline-flags)))

(defcwrap ft-outline-funcs
    ((move-to :pointer)
     (line-to :pointer)
     (conic-to :pointer)
     (cubic-to :pointer)

     (shift :int)
     (delta ft-pos)))

 ;; Basic API
(defcwraptype ft-library :pointer)
(defcwraptype ft-face :pointer)
(defcwraptype ft-size :pointer)
(defcwraptype ft-glyphslot :pointer)
(defcwraptype ft-charmap :pointer)
(defcwraptype ft-module :pointer)
(defcwraptype ft-driver :pointer)
(defcwraptype ft-renderer :pointer)
(defcwraptype ft-face-internal :pointer)
(defcwraptype ft-size-internal :pointer)
(defcwraptype ft-subglyph :pointer)
(defcwraptype ft-slot-internal :pointer)
(defcwraptype ft-size-request :pointer)

(defcwrap ft-glyph-metrics
    ((width ft-pos)
     (height ft-pos)
     (hori-bearing-x ft-pos)
     (hori-bearing-y ft-pos)
     (hori-advance ft-pos)
     (vert-bearing-x ft-pos)
     (vert-bearing-y ft-pos)
     (vert-advance ft-pos)))

(defcwrap ft-bitmap-size
    ((height ft-short)
     (width ft-short)
     (size ft-pos)
     (x-ppem ft-pos)
     (y-ppem ft-pos)))

(defcwrap (ft-charmaprec ft-charmap)
    ((face ft-face)
     (encoding ft-encoding)
     (platform-id ft-ushort)
     (encoding-id ft-ushort)))

(defcwrap (ft-facerec ft-face)
    ((num-faces ft-long)
     (face-index ft-long)

     (face-flags ft-face-flags)
     (style-flags ft-style-flags)

     (num-glyphs ft-long)

     (family-name :string)
     (style-name :string)

     (num-fixed-sizes ft-int)
     (available-sizes (pointer-to :type ft-bitmap-size
                                  :array-size num-fixed-sizes))
     (num-charmaps ft-int)
     (charmaps (pointer-to :type ft-charmap
                           :array-size num-charmaps))
     (generic (:struct foreign-ft-generic))
     (bbox (:struct foreign-ft-bbox))
     (units-per-em ft-ushort)
     (ascender ft-short)
     (descender ft-short)
     (height ft-short)
     (max-advance-width ft-short)
     (max-advance-height ft-short)
     (underline-position ft-short)
     (underline-thickness ft-short)
     (glyph ft-glyphslot)
     (size ft-size)
     (charmap ft-charmap)
     (driver ft-driver)
     (memory ft-memory)
     (stream ft-stream)
     (sizes-list (:struct foreign-ft-listrec))
     (autohint (:struct foreign-ft-generic))
     (extensions :pointer)
     (internal ft-face-internal)))

(defcwrap ft-size-metrics
    ((x-ppem ft-ushort)
     (y-ppem ft-ushort)
     (x-scale ft-fixed)
     (y-scale ft-fixed)
     (ascender ft-pos)
     (descender ft-pos)
     (height ft-pos)
     (max-advance ft-pos)))

(defcwrap (ft-sizerec ft-size)
    ((face ft-face)
     (generic (:struct foreign-ft-generic))
     (metrics (:struct foreign-ft-size-metrics))
     (internal ft-size-internal)))

(defcwrap (ft-glyphslotrec ft-glyphslot)
    ((library ft-library)
     (face ft-face)
     (next ft-glyphslot)
     (reserved ft-uint)
     (generic (:struct foreign-ft-generic))
     (metrics (:struct foreign-ft-glyph-metrics))
     (linear-hori-advance ft-fixed)
     (linear-vert-advance ft-fixed)
     (advance (:struct foreign-ft-vector))
     (format ft-glyph-format)
     (bitmap (:struct foreign-ft-bitmap))
     (bitmap-left ft-int)
     (bitmap-top ft-int)
     (outline (:struct foreign-ft-outline))
     (num-subglyphs ft-uint)
     (subglyphs ft-subglyph)
     (control-data :pointer)
     (control-len :long)
     (lsb-delta ft-pos)
     (rsb-delta ft-pos)
     (other :pointer)
     (internal ft-face-internal)))

(defcwrap ft-parameter
    ((tag ft-ulong)
     (data ft-pointer)))

(defcwrap ft-open-args
    ((flags ft-open-flags)
     (memory-base :pointer)
     (memory-size ft-long)
     (pathname :pointer)
     (stream ft-stream)
     (driver ft-module)
     (num-params ft-int)
     (params :pointer)))

(defcwrap (ft-size-requestrec ft-size-request)
    ((type ft-size-request-type)
     (width ft-long)
     (height ft-long)
     (hori-resolution ft-uint)
     (vert-resolution ft-uint)))

 ;; Glyphs

(defcwraptype ft-glyph :pointer)
(defcwraptype ft-bitmapglyph :pointer)
(defcwraptype ft-outlineglyph :pointer)

(defcwrap (ft-glyphrec ft-glyph)
    ((library ft-library)
     (clazz :pointer)
     (format ft-glyph-format)
     (advance (:struct foreign-ft-vector))))

(defcwrap (ft-bitmapglyphrec ft-bitmapglyph)
    ((root (:struct foreign-ft-glyphrec))
     (left ft-int)
     (top ft-int)
     (bitmap (:struct foreign-ft-bitmap))))

(defcwrap (ft-outlineglyphrec ft-outlineglyph)
    ((root (:struct foreign-ft-glyphrec))
     (outline (:struct foreign-ft-outline))))
