(in-package :freetype2)

 ;; Render a bunch of glyphs

(defmacro do-string-render ((face string bitmap-var x-var y-var
                             &optional (direction :left-right) (load-char-function 'load-char))
                            &body body)
  "Load, render, and compute metrics for each character in STRING in
an optimal manner. `FACE` should be set up appropriately (e.g., size).
`BITMAP-VAR` is passed to the block as an ft-bitmap, `X-VAR` and `Y-VAR` are
coordinates for each glyph.  `DIRECTION` may be specified as `:left-right`,
`:right-left`, `:up-down`, or `:down-up`.  `LOAD-CHAR-FUNCTION` is a minimal
hook for application-side caching; it should be compatible with
[`LOAD-CHAR`](#LOAD-CHAR).  cl-freetype2 does not do any caching itself."
  (let ((c1 (gensym))
        (c2 (gensym))
        (x (gensym))
        (y (gensym))
        (advance (gensym))
        (max-ascender (gensym))
        (len (gensym))
        (glyphslot (gensym))
        (kern (gensym))
        (vertical-p (gensym)))
    `(let ((,max-ascender (face-ascender-pixels ,face))
           (,len (length ,string))
           (,vertical-p (or (eq ,direction :up-down) (eq ,direction :down-up))))
       (loop with ,x = 0.0 and ,y = 0.0
             for i from 0 below ,len
             as ,c1 = (aref ,string i)
             as ,c2 = (if (< i (1- ,len))
                          (aref ,string (1+ i))
                          nil)
             as ,kern = (if (and ,c2 (not ,vertical-p))
                            (get-kerning ,face ,c1 ,c2)
                            0.0)
             do
                (,load-char-function ,face ,c1 (if ,vertical-p '(:vertical-layout) '(:default)))
                (let ((,advance (get-loaded-advance ,face ,vertical-p))
                      (,glyphslot (render-glyph ,face)))
                  (case ,direction
                    (:right-left (decf ,x (+ ,advance ,kern)))
                    (:down-up (decf ,y ,advance)))

                  (let ((,x-var (round (+ ,x (ft-glyphslot-bitmap-left ,glyphslot))))
                        (,y-var (round (+ ,y (- ,max-ascender (ft-glyphslot-bitmap-top ,glyphslot)))))
                        (,bitmap-var (ft-glyphslot-bitmap ,glyphslot)))
                    ,@body)

                  (case ,direction
                    (:left-right (incf ,x (+ ,advance ,kern)))
                    (:up-down (incf ,y ,advance))))))))

(export 'do-string-render)
