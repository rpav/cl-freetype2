(in-package :freetype2-tests)
(in-suite freetype2-tests)

(defvar *metrics-lr*
  '(((7 12) 71 8) ((8 17) 61 8) ((8 17) 51 8) ((10 17) 38 3) ((7 12) 30 8)
    ((7 12) 22 8) ((8 12) 13 8) ((11 17) 1 3)))

(defvar *metrics-rl*
  '(((7 12) -77 8) ((8 17) -69 8) ((8 17) -59 8) ((10 17) -49 3)
    ((7 12) -36 8) ((7 12) -28 8) ((8 12) -20 8) ((11 17) -11 3)))

(defvar *metrics-ud*
  '(((7 12) 1 176) ((8 17) 1 152) ((8 17) 1 128) ((10 17) 1 99) ((7 12) 1 80)
    ((7 12) 1 56) ((8 12) 1 32) ((11 17) 1 3)))

(defvar *metrics-du*
  '(((7 12) 1 -184) ((8 17) 1 -160) ((8 17) 1 -136) ((10 17) 1 -117)
    ((7 12) 1 -88) ((7 12) 1 -64) ((8 12) 1 -40) ((11 17) 1 -21)))

(defun make-metrics (face string direction)
  (let (metrics)
    (do-string-render (face string bitmap x y :direction direction)
      (push (list (list (ft-bitmap-width bitmap)
                        (ft-bitmap-rows bitmap))
                  x y)
            metrics))
    metrics))

(test (test-do-string-render :depends-on test-set-size)
  (is (equal *metrics-lr* (make-metrics *face* "FreeType" :left-right)))
  (is (equal *metrics-rl* (make-metrics *face* "FreeType" :right-left)))
  (is (equal *metrics-ud* (make-metrics *face* "FreeType" :up-down)))
  (is (equal *metrics-du* (make-metrics *face* "FreeType" :down-up))))
