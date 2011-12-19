(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system :fiveam))

(defsystem :cl-freetype2-tests
  :description "Test suite for cl-freetype2"

  :depends-on (:cl-freetype2 :fiveam)

  :pathname "t"
  :serial t

  :components
  ((:file "package")
   (:file "suite")
   (:file "library")
   (:file "face")
   (:file "glyph")
   (:file "outline")
   (:file "toy")

   (:static-file "TranscendsGames.otf")
   (:static-file "TranscendsGames.README")))

(defmethod perform ((o test-op) (c (eql (find-system :cl-freetype2-tests))))
  (let ((5am:*test-dribble* *error-output*))
    (5am:run! (intern "FREETYPE2-TESTS" 'freetype2-tests))))
