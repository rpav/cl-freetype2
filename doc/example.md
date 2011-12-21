{anchor examples}

Example
=======

{anchor examples-rendering}

Rendering
---------

An example using the toy interface:

    (defparameter *face* (new-face "/usr/share/fonts/corefonts/times.ttf"))

    ;; Set the size to 24 points and 72 DPI
    (set-char-size *face* (* 24 64) 0 72 72)

    ;; Trivial output:
    (print-with-face *face* "Hello")

    ;; Output (if this doesn't display, make sure your encoding is
    ;; set to UTF-8, and that your "fixed-width" font is
    ;; fixed-enough, or you'll just get a jumble):
                                                            ▒▒▓▓██      ▒▒▓▓██                            
    ████████████      ████████████                        ████████    ████████                            
      ▒▒████░░          ▒▒████░░                              ████        ████                            
        ████              ████                                ████        ████                            
        ████              ████                                ████        ████                            
        ████              ████                                ████        ████                            
        ████              ████            ░░██████▓▓          ████        ████          ░░▓▓████▓▓░░      
        ████              ████          ▒▒██░░  ░░████        ████        ████        ░░██░░    ▓▓██▒▒    
        ██████████████████████        ░░██▒▒      ░░██▒▒      ████        ████        ██▒▒        ████░░  
        ████              ████        ▓▓██          ████      ████        ████      ▒▒██          ▒▒██▓▓  
        ████              ████        ██████████████████      ████        ████      ████            ████  
        ████              ████        ████                    ████        ████      ████            ████  
        ████              ████        ████░░          ░░      ████        ████      ████░░          ████  
        ████              ████        ▓▓██▓▓        ░░▓▓      ████        ████      ▓▓██▒▒          ██▒▒  
        ████              ████        ░░████▓▓    ░░██░░      ████        ████      ░░████        ░░██    
      ░░████░░          ░░████░░        ▓▓██████████▒▒      ▒▒████▒▒    ▒▒████▒▒      ▒▒████    ░░██░░    
    ████████████      ████████████        ▒▒██████░░      ████████████████████████      ░░▓▓████▓▓░░      


The above is a minimally-required set of calls to render some text.
The `PRINT-WITH-FACE` function is a toy, but a working toy, useful to
experiment with other API calls and see a result.  Behind the scenes,
`PRINT-WITH-FACE` is using the `DO-STRING-RENDER` macro, which is
likely of more use in a "real" application:
    
    (let (metrics)
      (do-string-render (*face* "Hello" bitmap x y)
        (push (list x y) metrics))
      (print (nreverse metrics)))

    ;; Output: ((1 5) (18 10) (28 4) (34 4) (41 10))

The `DO-STRING-RENDER` macro uses a generalized algorithm for
computing the coordinates for each glyph, taking into account the
offsets and kerning.  The coordinate system here assumes (0,0) is
upper-left, and the X,Y pair is offsets from the origin to the
_upper-left_ _corner_ of `BITMAP`.  This is a very common screen
coordinate system, and easily translated into zero-baseline or similar.

Other directional layouts are supported, but the coordinate system
does not change:

    (let (metrics)
      (do-string-render (*face* "Hello" bitmap x y)
        (push (list x y) metrics))
      (print (nreverse metrics)))

    ;; Output: ((-16 5) (-27 10) (-34 4) (-40 4) (-51 10))

In this case, glyphs are rendered right-to-left, into negative `X`,
but coordinates are still to the upper-left corner of the glyph, from
the origin.  This simplifies multidirectional rendering considerably.

**Note:** While cl-freetype2 will render in any direction using the
proper metrics, it does _not_ currently support `GPOS`/`GSUB`,
`mort`/`morx`/`lcar` or similar tables and does not do ligature
substitution, because FreeType 2 does not provide access to this
information beyond a `FT_Byte*` pointer.  I hope to write a parser at
some point.  However until then, there's no real support for languages
which require these tables to render properly.

We have not discussed `BITMAP`, which is an untranslated `ft-bitmap`
struct.  The function `BITMAP-TO-ARRAY` has been provided for trivial
translation to a native lisp form, if desired.  This is not automatic
because the primary use may be passing the bitmap to another C
function.

{anchor examples-metrics}

Loading and Metrics
-------------------

You need not rely on `DO-STRING-RENDER`, however; far more basic
functions are available:

    (load-char *face* #\A)
    (render-glyph *face*)          ;; => #<FT-GLYPHSLOT>, may be ignored

Various metrics are available:

    (get-advance *face* #\A)       ;; => 17.0
    (get-kerning *face* #\T #\i)   ;; => -1.0

Or other interesting information:

    (fixed-face-p *face*)          ;; => NIL
    (get-glyph-name *face* #\&)    ;; => "ampersand", 9
    (get-postscript-name *face*)   ;; => "TimesNewRomanPSMT"

    (ft-face-face-flags *face*)    ;; => (:SCALABLE :SFNT :HORIZONTAL :KERNING :GLYPH-NAMES :HINTER)

(The unfortunate naming of the latter is due to `FT-FACE` being the
type, and `FACE-FLAGS` being the slot-name.  Consistency with FreeType
naming is more desirable than occasional aesthetics.)

Refer to the cl-freetype2 and FreeType API references for a more
complete picture of what each provide.

{anchor examples-outlines}

Outlines
--------

While FreeType2 does simple and fast rendering of fonts to bitmaps,
interesting things can be done with the actual outline data for
outline fonts.  cl-freetype2 also provides convenient access to this:

    (do-char-decompose (*face* #\A) (op &rest points)
      (format t "~&OP: ~A ~A~%" op
              (mapcar (lambda (v)
                        (if v (list (ft-vector-x v)
                                    (ft-vector-y v))))
                      points)))

    ;; Output:
    OP: MOVETO ((708 320) NIL NIL)
    OP: LINETO ((250 320) NIL NIL)
    OP: LINETO ((192 180) NIL NIL)
    OP: CONICTO ((167 113) (167 135) NIL)
    OP: CONICTO ((189 81) (167 95) NIL)
    OP: CONICTO ((284 64) (211 68) NIL)
    OP: LINETO ((284 0) NIL NIL)
    OP: LINETO ((-30 0) NIL NIL)
    OP: LINETO ((-30 64) NIL NIL)
    OP: CONICTO ((52 86) (34 73) NIL)
    OP: CONICTO ((137 199) (91 114) NIL)
    OP: LINETO ((500 1024) NIL NIL)
    OP: LINETO ((556 1024) NIL NIL)
    OP: LINETO ((896 194) NIL NIL)
    OP: CONICTO ((974 90) (939 114) NIL)
    OP: CONICTO ((1071 64) (1009 67) NIL)
    OP: LINETO ((1071 0) NIL NIL)
    OP: LINETO ((688 0) NIL NIL)
    OP: LINETO ((688 64) NIL NIL)
    OP: CONICTO ((770 79) (749 66) NIL)
    OP: CONICTO ((792 112) (792 93) NIL)
    OP: CONICTO ((762 192) (792 137) NIL)
    OP: LINETO ((708 320) NIL NIL)
    OP: MOVETO ((698 384) NIL NIL)
    OP: LINETO ((496 867) NIL NIL)
    OP: LINETO ((277 384) NIL NIL)
    OP: LINETO ((698 384) NIL NIL)
    
Doing something interesting with this is beyond our scope, though.
