(in-package :freetype2)

 ;; C: Base Functions

(defcfun ("FT_New_Face" ft-new-face) ft-error
  (library ft-library)
  (pathname :string)
  (face-index ft-long)
  (aface (:pointer ft-face)))

(defcfun ("FT_New_Memory_Face" ft-new-memory-face) ft-error
  (library ft-library)
  (file-base (:pointer ft-byte))
  (file-size ft-long)
  (face-index ft-long)
  (aface :pointer))

(defcfun ("FT_Open_Face" ft-open-face) ft-error
  (library ft-library)
  (args (:pointer ft-open-args))
  (face-index ft-long)
  (face (:pointer ft-face)))

(defcfun ("FT_Attach_File" ft-attach-file) ft-error
  (face ft-face)
  (pathname :string))

;; NOT IMPLEMENTED: FT_Attach_Stream

(defcfun ("FT_Reference_Face" ft-reference-face) ft-error
  (face ft-face))

(defcfun ("FT_Done_Face" ft-done-face) ft-error
  (face (:pointer ft-face)))

(defcfun ("FT_Select_Size" ft-select-size) ft-error
  (face ft-face)
  (strike-index ft-int))

(defcfun ("FT_Request_Size" ft-request-size) ft-error
  (face ft-face)
  (req ft-size-request))

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

(defcfun ("FT_Load_Glyph" ft-load-glyph) ft-error
  (face ft-face)
  (glyph-index ft-uint)
  (load-flags ft-load-flags))

;; Implemented natively: FT_Load_Char

(defcfun ("FT_Set_Transform" ft-set-transform) :void
  (face ft-face)
  (matrix (:pointer ft-matrix))
  (delta (:pointer ft-vector)))

(defcfun ("FT_Get_Kerning" ft-get-kerning) ft-error
  (face ft-face)
  (left-glyph ft-uint)
  (right-glyph ft-uint)
  (kern-mode ft-kerning-mode)
  (akerning (:pointer ft-vector)))

(defcfun ("FT_Get_Track_Kerning" ft-get-track-kerning) ft-error
  (face ft-face)
  (point-size ft-fixed)
  (degree ft-int)
  (akerning (:pointer ft-fixed)))

(defcfun ("FT_Get_Glyph_Name" ft-get-glyph-name) ft-error
  (face ft-face)
  (glyph-index ft-uint)
  (buffer ft-pointer)
  (buffer-max ft-uint))

(defcfun ("FT_Get_Postscript_Name" ft-get-postscript-name) :string
  (face ft-face))

(defcfun ("FT_Select_Charmap" select-charmap) ft-error
  (face ft-face)
  (encoding ft-encoding))

(defcfun ("FT_Set_Charmap" ft-set-charmap) ft-error
  (face ft-face)
  (charmap ft-charmap))

(defcfun ("FT_Get_Charmap_Index" ft-get-charmap-index) ft-int
  (charmap ft-charmap))

(defcfun ("FT_Get_Char_Index" ft-get-char-index) ft-uint
  (face ft-face)
  (charcode ft-ulong))

(defcfun ("FT_Get_First_Char" ft-get-first-char) ft-ulong
  (face ft-face)
  (agindex (:pointer ft-uint)))

(defcfun ("FT_Get_Next_Char" ft-get-next-char) ft-ulong
  (face ft-face)
  (char-code ft-ulong)
  (agindex (:pointer ft-uint)))

(defcfun ("FT_Get_Name_Index" ft-get-name-index) ft-uint
  (face ft-face)
  (glyph-name :string))

(defcfun ("FT_Get_FSType_Flags" get-fstype-flags) ft-fstype-flags
  (face ft-face))

 ;; C: Variant Functions

(defcfun ("FT_Face_GetCharVariantIndex" ft-face-getcharvariantindex)
    ft-uint
  (face ft-face)
  (charcode ft-ulong)
  (variant-selector ft-ulong))

(defcfun ("FT_Face_GetCharVariantIsDefault" ft-face-getcharvariantisdefault)
    ft-int
  (face ft-face)
  (charcode ft-ulong)
  (variant-selector ft-ulong))

(defcfun ("FT_Face_GetVariantSelectors" ft-face-getvariantselectors)
    (:pointer ft-uint32)
  (face ft-face))

(defcfun ("FT_Face_GetVariantsOfChar" ft-face-getvariantsofchar)
    (:pointer ft-uint32)
  (face ft-face)
  (charcode ft-ulong))

(defcfun ("FT_Face_GetCharsOfVariant" ft-face-getcharsofvariant)
    (:pointer ft-uint32)
  (face ft-face)
  (variant-selector ft-ulong))
                                      
 ;; Lisp Functions

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
