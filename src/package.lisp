(defpackage :freetype2-types
  (:use #:cl #:cffi))

(defpackage :freetype2-ffi
  (:use #:cl #:cffi #:freetype2-types))

(defpackage :freetype2
  (:use #:cl #:cffi #:freetype2-types #:freetype2-ffi)
  (:nicknames :ft2))
