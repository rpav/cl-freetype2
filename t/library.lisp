(in-package :freetype2-tests)
(in-suite freetype2-tests)

(deftest test-library-exists ()
  (is (typep *library* 'ft-library))
  (is (typep (make-freetype) 'ft-library)))

