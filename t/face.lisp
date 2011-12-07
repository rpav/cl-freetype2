(in-package :freetype2-tests)
(in-suite freetype2-tests)

(defvar *face*)

(defvar *test-font-path*
  (asdf:component-pathname
   (asdf:find-component :cl-freetype2-tests '("TranscendsGames.otf"))))

(deftest test-new-font ()
  "Make sure we can open the included font, and that it's nominally
correct.  This is important setup for the rest of the suite."

  (is (check-font-file *test-font-path*))
  (finishes (setf *face* (new-face *test-font-path*)))
  (is (typep *face* 'ft-face))
  (is (string= "TranscendsGames" (ft-face-family-name *face*))))

(deftest test-set-size ()
  "Set the size to 24 points at 72 DPI, and check the size metrics are
correct."
  (finishes
    (set-char-size *face* (* 24 64) 0 72 72))
  (is (= (ft-size-metrics-x-scale (ft-size-metrics (ft-face-size *face*)))
         100663))
  (is (= (ft-size-metrics-y-scale (ft-size-metrics (ft-face-size *face*)))
         100663))
  (is (= (ft-size-metrics-x-ppem (ft-size-metrics (ft-face-size *face*)))
         24))
  (is (= (ft-size-metrics-y-ppem (ft-size-metrics (ft-face-size *face*)))
         24))
  (is (= (ft-size-metrics-ascender (ft-size-metrics (ft-face-size *face*)))
         1280))
  (is (= (ft-size-metrics-descender (ft-size-metrics (ft-face-size *face*)))
         -320))
  (is (= (ft-size-metrics-height (ft-size-metrics (ft-face-size *face*)))
         1664))
  (is (= (ft-size-metrics-max-advance (ft-size-metrics (ft-face-size *face*)))
         1280)))
  
(deftest test-font-info ()
  "Verify basic information about the face."
  (is (= (ft-face-num-faces *face*) 1))
  (is (= (ft-face-face-index *face*) 0))
  (is (equal (ft-face-face-flags *face*)
             '(:scalable :sfnt :horizontal :glyph-names)))
  (is (string= (ft-face-style-name *face*) "Medium"))
  (is (= (ft-face-num-fixed-sizes *face*) 0))
  (is (= (ft-face-num-charmaps *face*) 4))
  (is (= (ft-face-units-per-em *face*) 1000))
  (is (= (ft-face-underline-position *face*) -150))
  (is (= (ft-face-underline-thickness *face*) 50))
  (is (eq (ft-charmap-encoding (ft-face-charmap *face*))
          :unicode))
  (is (not (fixed-face-p *face*))))

(deftest test-font-chars ()
  "Verify some basic char/index information"
  (is (= (get-char-index *face* #\A) 34))
  (is (= (get-char-index *face* #\Null) 0))

  ;; Unfortunately this font lacks any kerning info
  (is (= (get-kerning *face* #\T #\i) 0.0))

  (is (= (get-advance *face* #\T) 13.0))
  (is (= (get-advance *face* #\i) 5.0)))
