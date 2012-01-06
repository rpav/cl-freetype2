(defpackage :freetype2-types
  (:use #:cl #:cffi)
  (:nicknames :ft2-types)
  (:documentation "FreeType 2 types and type interface.  This is separate
to avoid requiring FREETYPE2-FFI imports."))

(defpackage :freetype2-ffi
  (:use #:cl #:cffi #:freetype2-types)
  (:documentation "Foreign Function definitions only."))

(defpackage :freetype2
  (:use #:cl #:cffi #:freetype2-types #:freetype2-ffi)
  (:nicknames :ft2)
  (:documentation "
This is a general Freetype 2 wrapper for Common Lisp using CFFI.  It's
geared toward both using Freetype directly by providing a simplified
API, as well as providing access to the underlying C structures and
functions for use with other libraries which may also use Freetype."))
