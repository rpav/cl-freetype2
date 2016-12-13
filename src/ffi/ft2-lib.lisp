(in-package :freetype2)

 ;; Loading

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library freetype2
    (:windows (:or "libfreetype.dll" "libfreetype-6.dll"))
    (t (:default "libfreetype")))

  (use-foreign-library freetype2))
