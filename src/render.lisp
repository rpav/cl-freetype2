(in-package :freetype2)

 ;; Render a bunch of glyphs

(defun default-load-render (face char vertical-p)
  "=> BITMAP, ADVANCE, TOP, LEFT

This is the default `LOAD-FUNCTION` for `DO-STRING-RENDER`.  It is also
called in the case that a custom `LOAD-FUNCTION` returns `NIL`, convenient
for caching.

Custom functions must be compatible, though any (non-`NIL`) value may
be returned in the place of `BITMAP`.  Note that cl-freetype2 does nothing
else for you.  If you want your cache populated, you must do this yourself,
for instance, within the [`DO-STRING-RENDER`](#DO-STRING-RENDER) loop."
  (load-char face char (if vertical-p '(:vertical-layout) '(:default)))
  (let ((glyphslot (render-glyph face)))
    (values (ft-glyphslot-bitmap glyphslot)
            (get-loaded-advance face vertical-p)
            (ft-glyphslot-bitmap-left glyphslot)
            (ft-glyphslot-bitmap-top glyphslot))))

(export 'default-load-render)

(defmacro do-string-render ((face string bitmap-var x-var y-var
                             &key
                               (direction :left-right)
                               (load-function 'default-load-render)
                               (baseline-y-p nil)
                               (offsets-p t))
                            &body body)
  "Load, render, and compute metrics for each character in STRING in
an optimal manner. `FACE` should be set up appropriately (e.g., size).
`BITMAP-VAR` is passed to the block as an ft-bitmap, `X-VAR` and
`Y-VAR` are coordinates for each glyph.  `DIRECTION` may be specified
as `:left-right`, `:right-left`, `:up-down`, or `:down-up`.
`LOAD-FUNCTION` by default loads and renders a glyph, returning an
`FT-BITMAP`.  A custom function may be used in place to assist in
caching.  cl-freetype2 does not do any caching itself.  See the
documentation for [`DEFAULT-LOAD-RENDER`](#DEFAULT-LOAD-RENDER) for
details.

`BASELINE-Y-P`, if set (not default), will give `Y` in terms of
*baseline* rather than an offset to the upper edge.  `OFFSETS-P`, if
set (default), will calculate `LEFT` offset into `X`.  This is
critical for correct inter-glyph spacing, but some things (e.g. Cairo)
calculate this separately."
  (once-only (face string)
    (with-gensyms (c1 c2 x y left top
                   advance max-ascender len
                   kern vertical-p)
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
                  (let (,bitmap-var ,advance ,left ,top)
                    (multiple-value-setq (,bitmap-var ,advance ,left ,top)
                        (funcall ,(if (symbolp load-function)
                                           `(function ,load-function)
                                           load-function)
                                 ,face ,c1 ,vertical-p))
                    ,(unless (eq load-function 'default-load-render)
                       `(unless ,bitmap-var
                          (multiple-value-setq (,bitmap-var ,advance ,left ,top)
                            (default-load-render ,face ,c1 ,vertical-p))))
                    (case ,direction
                      (:right-left (decf ,x (+ ,advance ,kern)))
                      (:down-up (decf ,y ,advance)))

                    (let ((,x-var (round ,(if offsets-p `(+ ,x ,left) x)))
                          (,y-var (round ,(if baseline-y-p
                                              `(+ ,y ,max-ascender)
                                              `(+ ,y (- ,max-ascender ,top))))))
                      ,@body)

                    (case ,direction
                      (:left-right (incf ,x (+ ,advance ,kern)))
                      (:up-down (incf ,y ,advance)))))))))

(export 'do-string-render)
