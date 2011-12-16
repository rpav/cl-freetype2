(in-package :freetype2-tests)
(in-suite freetype2-tests)

(defvar *glyphslot*)
(defvar *metrics*)
(defvar *glyph*)

(test (test-load-glyphslot :depends-on test-set-size)
  "Verify basic glyph info"
  (finishes (load-char *face* #\j))
  (is (typep (setf *glyphslot* (ft-face-glyph *face*)) 'ft-glyphslot))
  (is (typep (setf *metrics* (ft-glyphslot-metrics *glyphslot*)) 'ft-glyph-metrics)))

(test (test-glyph-metrics :depends-on test-load-glyphslot)
  (is (= 320 (ft-glyph-metrics-width *metrics*)))
  (is (= 1408 (ft-glyph-metrics-height *metrics*)))
  (is (= -64 (ft-glyph-metrics-hori-bearing-x *metrics*)))
  (is (= 1088 (ft-glyph-metrics-hori-bearing-y *metrics*))))

(test (test-load-glyph :depends-on test-load-glyphslot)
  (is (typep (setf *glyph* (get-glyph *face*)) 'ft-outlineglyph)))

(test (test-render-glyph :depends-on test-load-glyph)
  (finishes (render-glyph *glyphslot*)))
