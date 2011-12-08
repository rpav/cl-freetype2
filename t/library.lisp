(in-package :freetype2-tests)
(in-suite freetype2-tests)

(test test-library-exists
  (is (typep *library* 'ft-library))
  (is (typep (make-freetype) 'ft-library)))

(test (test-library-versions :depends-on test-library-exists)
  (multiple-value-bind (maj min pat) (freetype-version)
    (is (= maj +version-major+))
    (is (= min +version-minor+))
    (is (= pat +version-patch+))))
