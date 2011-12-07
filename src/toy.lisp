(in-package :freetype2)

 ;; Simple string-to-array

(defun toy-string-to-array (face string &optional (load-flags '(:default)))
  (let ((flags-value (convert-to-foreign load-flags 'ft-load-flags))
        (vert-flag (convert-to-foreign '(:vertical-layout) 'ft-load-flags)))
    (let ((array (make-array (list (round (string-pixel-height face string flags-value))
                                   (round (string-pixel-width face string flags-value)))
                             :element-type 'unsigned-byte))
          (max-ascender (face-ascender-pixels face))
          (advances (get-string-advances face string flags-value))
          (kerning (if (= 0 (logand flags-value vert-flag))
                       (get-string-kerning face string)
                       nil)))
      (loop with x = 0
            for i from 0 below (length string)
            as c = (elt string i)
            as a = (elt advances i)
            as k = (if kerning (elt kerning i) 0)
            do
               (load-char face c flags-value)
               (let ((glyphslot (render-glyph face)))
                 #+-(format t "~&Load ~A | Blit to ~A,~A | Advance X by ~A (kern ~A)~%"
                            c x (round (- max-ascender (ft-glyphslot-bitmap-top glyphslot)))
                            a k)
                 (ablit-from-nonzero array (bitmap-to-array (ft-glyphslot-bitmap glyphslot))
                                     :x (round (max 0 (+ x (ft-glyphslot-bitmap-left glyphslot))))
                                     :y (round (- max-ascender (ft-glyphslot-bitmap-top glyphslot)))))
               (incf x (+ a k)))
      array)))
    

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

(defun print-with-face (face string &optional (load-flags '(:default)))
  (let ((array (toy-string-to-array face string load-flags)))
    (if (eq (ft-bitmap-pixel-mode (ft-glyphslot-bitmap (ft-face-glyph face)))
            :mono)
        (print-mono array)
        (print-greys array))))
