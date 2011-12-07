(defsystem :cl-freetype2-tests
  :description "Test suite for cl-freetype2"

  :depends-on (:cl-freetype2 :stefil)

  :pathname "t"
  :serial t

  :components
  ((:file "package")
   (:file "suite")
   (:file "library")
   (:file "face")
   (:file "glyph")

   (:static-file "TranscendsGames.otf")
   (:static-file "TranscendsGames.README")))

(defmethod perform ((o test-op) (c (eql (find-system :cl-freetype2-tests))))
  (funcall (intern "FREETYPE2-TESTS" '#:freetype2-tests)))
