(in-package :freetype2)

 ;; Glyph Handling

(defun render-glyph (face-or-glyphslot &optional (render-mode :normal))
  "Render the loaded glyph in `FACE-OR-GLYPHSLOT`, optionally specifying
`RENDER-MODE`.  Return the rendered glyphslot."
  (etypecase face-or-glyphslot
    (ft-glyphslot
     (ft-error (ft-render-glyph face-or-glyphslot render-mode))
     face-or-glyphslot)
    (ft-face
     (ft-error (ft-render-glyph (ft-face-glyph face-or-glyphslot) render-mode))
     (ft-face-glyph face-or-glyphslot))))

(export 'render-glyph)

(defun get-glyph (face-or-glyphslot)
  "=> GLYPH
Get the `FT_Glyph` from `FACE-OR-GLYPHSLOT`."
  (let ((glyphslot (etypecase face-or-glyphslot
                     (ft-glyphslot face-or-glyphslot)
                     (ft-face (ft-face-glyph face-or-glyphslot)))))
    (case (ft-glyphslot-format glyphslot)
      (:bitmap
       (make-wrapper (glyph &glyph ft-bitmapglyph)
           (ft-get-glyph glyphslot &glyph)
           (ft-done-glyph (p* &glyph))))
      (:outline
       (make-wrapper (glyph &glyph ft-outlineglyph)
           (ft-get-glyph glyphslot &glyph)
           (ft-done-glyph (p* &glyph))))
      (t (make-wrapper (glyph &glyph ft-glyph)
           (ft-get-glyph glyphslot &glyph)
           (ft-done-glyph (p* &glyph)))))))

(export 'get-glyph)
