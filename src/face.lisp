(in-package :freetype2)

 ;; Face

(defmethod print-object ((object ft-face) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "\"~A ~A\" {#x~8,'0X}"
                (ft-face-family-name object)
                (ft-face-style-name object)
                (pointer-address (fw-ptr object)))))

(defun check-font-file (pathname &optional (library *library*))
  "=> NUM-FACES or NIL
Verify `PATHNAME` is a supported format by calling `FT_Open_Face`
with a negative face index.  If the face is supported, return the
number of faces in the file.  Otherwise, NIL."
  (with-foreign-objects ((c-open-args 'ft-open-args)
                         (face 'ft-face))
    (with-foreign-string (cpathname (namestring pathname))
      (let ((args (%make-ft-open-args :ptr c-open-args)))
        (setf (ft-open-args-flags args) :pathname
              (ft-open-args-memory-base args) (null-pointer)
              (ft-open-args-memory-size args) 0
              (ft-open-args-pathname args) cpathname
              (ft-open-args-stream args) nil
              (ft-open-args-driver args) nil
              (ft-open-args-num-params args) 0
              (ft-open-args-params args) (null-pointer))
        (if (eq :ok (ft-open-face library c-open-args -1 face))
            (let ((num-faces (ft2-types::%ft-facerec-num-faces (p* face))))
              (ft-done-face (p* face))
              num-faces)
            nil)))))

(export 'check-font-file)

(defun new-face (pathname &optional (index 0) (library *library*))
  "Make a new `FT-FACE` from `PATHNAME`, optionally specifying `INDEX` as the
face index."
  (make-wrapper (face &face ft-face)
    (ft-new-face library (namestring pathname) index &face)
    (ft-done-face (p* &face))))

(export 'new-face)

(defmacro with-open-face ((var pathname &optional (index 0) (library '*library*))
                     &body body)
  "Make a new `FT-FACE` on the stack from `PATHNAME`, closing and freeing
at the end of `BODY`"
  (let ((ptr (gensym)))
  `(let ((,var (%make-ft-face)))
     (with-foreign-object (,ptr 'ft-face)
       (unwind-protect
            (progn
              (ft-error (ft-new-face ,library (namestring ,pathname) ,index ,ptr))
              (setf (fw-ptr ,var) ,ptr)
              ,@body)
         (unless (null-pointer-p (fw-ptr ,var))
           (ft-done-face (p* ,ptr))))))))

(export 'with-open-face)

(defun fixed-face-p (face)
  "=> boolean
Check if `FACE` is fixed-width according to its flags"
  (ft-face-face-flags-test face '(:fixed-width)))

(export 'fixed-face-p)

(defun set-char-size (face char-width char-height horz-resolution vert-resolution)
  "Set the size for `FACE` to `CHAR-WIDTH` and `CHAR-HEIGHT`, specified in 1/64
**points**.  `HORZ-RESOLUTION` and `VERT-RESOLUTION` specify the DPI.

If either `CHAR-WIDTH` or `CHAR-HEIGHT` are 0, the other value is used."
  (ft-error (ft-set-char-size face char-width char-height
                              horz-resolution vert-resolution)))

(export 'set-char-size)

(defun set-pixel-sizes (face pixel-width pixel-height)
  "Set the size for `FACE` in **pixels** to `PIXEL-WIDTH` and `PIXEL-HEIGHT`.
Especially useful for fixed-size bitmap fonts."
  (ft-error (ft-set-pixel-sizes face pixel-width pixel-height)))

(export 'set-pixel-sizes)

(defun get-char-index (face char-or-code)
  "=> index
Get the index in `FACE` for `CHAR-OR-CODE`, which may be either a character,
or an integer code."
  (etypecase char-or-code
    (character (ft-get-char-index face (char-code char-or-code)))
    (integer (ft-get-char-index face char-or-code))))

(export 'get-char-index)

(defun load-glyph (face glyph-index &optional (load-flags :default))
  "Load a glyph in `FACE` for `GLYPH-INDEX`, optionally specifying `LOAD-FLAGS`.
Note that this requires a *glyph index*.  To directly load a character
or code, use [`LOAD-CHAR`](#LOAD-CHAR)."
  (ft-error (ft-load-glyph face glyph-index load-flags)))

(export 'load-glyph)

(defun load-char (face char-or-code &optional (load-flags :default))
  "Load a glyph in `FACE` for `CHAR-OR-CODE`, optionally specifying `LOAD-FLAGS`.
`CHAR-OR-CODE` may be specified as per [`GET-CHAR-INDEX`](#GET-CHAR-INDEX)."
  (load-glyph face (get-char-index face char-or-code) load-flags))

(export 'load-char)

(defun set-transform (face matrix delta)
  "Set the tranformation matrix for `FACE` to `MATRIX`, a 2x2
transformation matrix specified as per [`CONVERT-MATRIX`](cl-freetype2-types.html#CONVERT-MATRIX),
and `DELTA`, a translation vector specified as per
[`CONVERT-VECTOR`](cl-freetype2-types.html#CONVERT-VECTOR)."
  (let ((ft-matrix (convert-matrix matrix))
        (ft-vector (convert-vector delta)))
    (ft-set-transform face ft-matrix ft-vector)))

(export 'set-transform)

(defun get-kerning (face char1 char2 &optional mode)
  "=> kerning-float
Get the kerning between `CHAR1` and `CHAR2` for `FACE`, optionally specifying
the kerning mode `MODE`.

By default this returns the kerning in *pixels*.  If `MODE` is specified,
an untranslated value is returned as per `FT`_Get_Kerning with the given
flags."
  (let ((index1 (get-char-index face char1))
        (index2 (get-char-index face char2)))
    (with-foreign-object (v 'ft-vector)
      (ft-error (ft-get-kerning face index1 index2 (or mode :default) v))
      (let ((kern (freetype2-types::%ft-vector-x v)))
        (if (or mode (fixed-face-p face))
            kern
            (ft-26dot6-to-float kern))))))

(export 'get-kerning)

(defun get-string-kerning (face string &optional mode)
  "=> ARRAY
Return an array of kerning values for each pair of characters in `STRING`,
for `FACE`, optionally specifying the kerning mode `MODE`.  The results are as
per `GET-KERNING`."
  (let ((kern (make-array (length string) :initial-element 0)))
    (loop for i from 0 below (1- (length string))
          as c1 = (aref string i)
          as c2 = (aref string (1+ i))
          do (setf (aref kern i)
                   (get-kerning face c1 c2 mode)))
    kern))

(export 'get-string-kerning)

(defun get-track-kerning (face point-size degree)
  "=> ARRAY
Return the track kerning at `POINT-SIZE` for `FACE` given `DEGREE`.  See
the documentation for `FT_Get_Track_Kerning`.  It seems that only a few
Type 1 fonts use this."
  (with-foreign-object (akerning 'ft-fixed)
    (setf (mem-ref akerning 'ft-fixed) 0)
    (ft-error (ft-get-track-kerning face point-size degree akerning))
    (mem-ref akerning 'ft-fixed)))

(export 'get-track-kerning)

(defun get-glyph-name (face char-or-code)
  "=> name-string
Get the symbolic name and length in `FACE` for `CHAR-OR-CODE`."
  (with-foreign-pointer (buffer 64 len)
    (ft-error (ft-get-glyph-name face (get-char-index face char-or-code)
                                 buffer len))
    (foreign-string-to-lisp buffer :max-chars len)))

(export 'get-glyph-name)

(defun get-loaded-advance (face vertical-p)
  "=> advance-float
Get the glyph advance value in `FACE` for the already-loaded glyph.  If
`VERTICAL-P` is true, get the vertical advance, otherwise, get the
horizontal advance."
  (let ((advance (ft-glyphslot-advance (ft-face-glyph face))))
    (ft-26dot6-to-float (if vertical-p
                            (ft-vector-y advance)
                            (ft-vector-x advance)))))

(export 'get-loaded-advance)

(defun get-advance (face char-or-code &optional load-flags)
  "=> advance-float
Get the advance in `FACE` for `CHAR-OR-CODE`, optionally specifying `LOAD-FLAGS`.

This attempts to use `FT_Get_Advance` to efficiently retrieve an advance.  In
this case, `LOAD-FLAGS` are not used.

Failing this, this will load the glyph and retrieve the advance using
`GET-LOADED-ADVANCE`, specifying `VERTICAL-P` by whether the :vertical-layout
face-flag is set.  This is generally slower, but many fonts do not have a
facility for fast advance retrieval."
  (let ((gindex (get-char-index face char-or-code))
        (flags-value (convert-to-foreign load-flags 'ft-load-flags))
        (fast-flag (convert-to-foreign '(:fast-advance-only) 'ft-load-flags))
        (vert-flag (convert-to-foreign '(:vertical-layout) 'ft-load-flags)))
    (with-foreign-object (padvance 'ft-fixed)
      (if (eq :ok (ft-get-advance face gindex (logior flags-value fast-flag) padvance))
          (mem-ref padvance 'ft-fixed)
          (progn
            (load-glyph face gindex load-flags)
            (get-loaded-advance face (logtest vert-flag flags-value)))))))

(export 'get-advance)

(defun get-string-advances (face string &optional load-flags)
  "=> ARRAY
Get an array of advances for the string `STRING` for face `FACE`, optionally
specifying `LOAD-FLAGS`."
  (let ((advance (make-array (length string) :element-type 'float
                                             :initial-element 0.0)))
    (loop for c across string
          for i from 0
          do (setf (aref advance i)
                   (get-advance face c load-flags)))
    advance))

(export 'get-string-advances)

;; From freetype2-ffi, no need for wrapping:
(export 'get-postscript-name)
(export 'get-fstype-flags)
