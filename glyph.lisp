(in-package :freetype2)

 ;; Glyph Handling

(defun render-glyph (face-or-glyphslot &optional (render-mode :normal))
  (etypecase face-or-glyphslot
    (ft-glyphslot
     (ft-render-glyph face-or-glyphslot render-mode)
     face-or-glyphslot)
    (ft-face
     (ft-render-glyph (ft-face-glyph face-or-glyphslot) render-mode)
     (ft-face-glyph face-or-glyphslot))))
