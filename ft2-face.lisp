(in-package :freetype2)

 ;; Faces

(defcfun ("FT_New_Face" ft-new-face) ft-error
  (library ft-library)
  (pathname :string)
  (face-index ft-long)
  (aface :pointer))

(defcfun ("FT_Done_Face" ft-done-face) ft-error
  (face :pointer))

(defcfun ("FT_Set_Char_Size" set-char-size) ft-error
  (face ft-face)
  (char-width ft-f26dot6)
  (char-height ft-f26dot6)
  (horz-resolution ft-uint)
  (vert-resolution ft-uint))

(defcfun ("FT_Set_Pixel_Sizes" set-pixel-sizes) ft-error
  (face ft-face)
  (pixel-width ft-uint)
  (pixel-height ft-uint))

(defcfun ("FT_Get_Char_Index" ft-get-char-index) ft-uint
  (face ft-face)
  (charcode ft-ulong))

(defcfun ("FT_Load_Glyph" ft-load-glyph) ft-error
  (face ft-face)
  (glyph-index ft-uint)
  (load-flags ft-load-flags))

(defcfun ("FT_Select_Charmap" select-charmap) ft-error
  (face ft-face)
  (encoding ft-encoding))

(defcfun ("FT_Set_Transform" ft-set-transform) :void
  (face ft-face)
  (matrix (:pointer ft-matrix))
  (delta (:pointer ft-vector)))

(defcfun ("FT_Render_Glyph" ft-render-glyph) ft-error
  (slot ft-glyphslot)
  (render-mode ft-render-mode))

(defmethod print-object ((object ft-face) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "\"~A ~A\" {#x~8,'0X}"
                (ft-face-family-name object)
                (ft-face-style-name object)
                (pointer-address (fw-ptr object)))))

(defun new-face (pathname &optional (index 0) (library *library*))
  (make-wrapper (face &face ft-face)
    (ft-new-face library pathname index &face)
    (ft-done-face (p* &face))))

(defun get-char-index (face char-or-code)
  (etypecase char-or-code
    (character (ft-get-char-index face (char-code char-or-code)))
    (integer (ft-get-char-index face char-or-code))))

(defun load-glyph (face glyph-index &optional (load-flags :default))
  (ft-load-glyph face glyph-index load-flags))

(defun load-char (face char-or-code &optional (load-flags :default))
  (load-glyph face (get-char-index face char-or-code) load-flags))

(defun convert-matrix (matrix)
  (etypecase matrix
    (ft-matrix (& matrix))
    ((array * (2 2))
     (& (make-matrix (aref matrix 0 0)
                     (aref matrix 0 1)
                     (aref matrix 1 0)
                     (aref matrix 1 1))))
    ((or (simple-vector 4)
         (array * (4)))
     (& (make-matrix (aref matrix 0) (aref matrix 1)
                     (aref matrix 2) (aref matrix 3))))
    (null (null-pointer))))

(defun convert-vector (vector)
  (etypecase vector
    (ft-vector (& vector))
    ((or (simple-vector 2)
         (array * 2))
     (& (make-vector (aref vector 0) (aref vector 1))))
    (null (null-pointer))))

(defun set-transform (face matrix delta)
  (let ((ft-matrix (convert-matrix matrix))
        (ft-vector (convert-vector delta)))
    (ft-set-transform face ft-matrix ft-vector)))

(defun render-glyph (face-or-glyphslot &optional (render-mode :normal))
  (etypecase face-or-glyphslot
    (ft-glyphslot
     (ft-render-glyph face-or-glyphslot render-mode)
     face-or-glyphslot)
    (ft-face
     (ft-render-glyph (ft-face-glyph face-or-glyphslot) render-mode)
     (ft-face-glyph face-or-glyphslot))))
