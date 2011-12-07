(in-package :freetype2-tests)
(in-suite freetype2-tests)

(defvar *glyph*)
(defvar *metrics*)

(deftest test-load-glyph ()
  "Verify basic glyph info"
  (finishes (load-char *face* #\j))
  (is (typep (setf *glyph* (ft-face-glyph *face*)) 'ft-glyphslot))
  (is (typep (setf *metrics* (ft-glyphslot-metrics *glyph*)) 'ft-glyph-metrics)))

(deftest test-glyph-metrics ()
  (is (= (ft-glyph-metrics-width *metrics*) 320))
  (is (= (ft-glyph-metrics-height *metrics*) 1408))
  (is (= (ft-glyph-metrics-hori-bearing-x *metrics*) -64))
  (is (= (ft-glyph-metrics-hori-bearing-y *metrics*) 1088)))



