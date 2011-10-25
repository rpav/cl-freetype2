(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system :cffi-grovel))

(defsystem :freetype2
  :description "Wrapper for the Freetype2 library"

  :depends-on (:cffi :trivial-garbage)
  :serial t

  :components ((:file "package")
               (:module "freetype2-grovel"
                        :pathname "grovel"
                        :components
                        ((:static-file "grovel-freetype.h")
                         (cffi-grovel:grovel-file "grovel-freetype2")))
               (:file "freetype2-cffi")
               (:file "freetype2")))

;; Making an :around COMPILE-OP GROVEL-FILE is sortof the right way to do
;; this, if it didn't override everything else anyway.  Fix.
(push (concatenate 'string "-I"
                   (directory-namestring (asdf:component-pathname (asdf:find-component :freetype2 '("freetype2-grovel")))))
      cffi-grovel::*cc-flags*)
