(in-package :freetype2)

 ;; Simple string-to-array

(defun toy-string-to-array (face string direction)
  (let* ((flags (if (or (eq direction :up-down)
                        (eq direction :down-up))
                    '(:vertical-layout)
                    '(:default)))
         (height (round (string-pixel-height face string flags)))
         (width (round (string-pixel-width face string flags)))
         (array (make-array (list height width) :element-type 'unsigned-byte
                            :initial-element 0)))
    (do-string-render (face string bitmap x y direction)
      (case direction
        (:left-right (ablit array bitmap :x x :y y))
        (:right-left (ablit array bitmap :x (+ width x) :y y))
        (:up-down    (ablit array bitmap :x x :y y))
        (:down-up    (ablit array bitmap :x x :y (+ height y)))))
    array))

 ;; Simple output

(defun trgrey (i)
  (cond
    ((> i 200) "██")
    ((> i 150) "▓▓")
    ((> i 100) "▒▒")
    ((> i 50)  "░░")
    (t "  ")))

(defun print-greys (array)
  (loop for i from 0 below (array-dimension array 0)
        do (loop for j from 0 below (array-dimension array 1)
                 do (princ (trgrey (aref array i j))))
        do (princ #\Newline)))

(defun print-mono (array)
  (loop for i from 0 below (array-dimension array 0)
        do (loop for j from 0 below (array-dimension array 1)
                 do (if (> (aref array i j) 0)
                        (princ "██")
                        (princ "  ")))
        do (princ #\Newline)))

(defun print-with-face (face string &optional (direction :left-right))
  "This is a toy function to render STRING using FACE, optionally
specifying DIRECTION as one of :left-right, :right-left, :up-down, or
:down-up.  Some glyphs may cut off or wrap strangely depending on
their metrics.  This is also not guaranteed to be a particularly
efficient implementation."
  (let ((array (toy-string-to-array face string direction)))
    (if (eq (ft-bitmap-pixel-mode (ft-glyphslot-bitmap (ft-face-glyph face)))
            :mono)
        (print-mono array)
        (print-greys array))))

(export 'print-with-face)
