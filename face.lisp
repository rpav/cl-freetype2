(in-package :freetype2)

 ;; Face

(defmethod print-object ((object ft-face) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "\"~A ~A\" {#x~8,'0X}"
                (ft-face-family-name object)
                (ft-face-style-name object)
                (pointer-address (fw-ptr object)))))

(defun check-font-file (library pathname)
  (with-foreign-object (c-open-args 'ft-open-args)
    (with-foreign-string (cpathname pathname)
      (let ((args (%make-ft-open-args :ptr c-open-args)))
        (setf (ft-open-args-flags args) :pathname
              (ft-open-args-memory-base args) (null-pointer)
              (ft-open-args-memory-size args) 0
              (ft-open-args-pathname args) cpathname
              (ft-open-args-stream args) nil
              (ft-open-args-driver args) nil
              (ft-open-args-num-params args) 0
              (ft-open-args-params args) (null-pointer))
        (ft-open-face library c-open-args -1 (null-pointer))))))

(defun new-face (pathname &optional (index 0) (library *library*))
  (make-wrapper (face &face ft-face)
    (ft-new-face library pathname index &face)
    (ft-done-face (p* &face))))

(export 'new-face)

(defun get-char-index (face char-or-code)
  (etypecase char-or-code
    (character (ft-get-char-index face (char-code char-or-code)))
    (integer (ft-get-char-index face char-or-code))))

(export 'get-char-index)

(defun load-glyph (face glyph-index &optional (load-flags :default))
  (ft-load-glyph face glyph-index load-flags))

(export 'load-glyph)

(defun load-char (face char-or-code &optional (load-flags :default))
  (load-glyph face (get-char-index face char-or-code) load-flags))

(export 'load-char)

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

(export 'set-transform)

(defun get-kerning (face char1 char2 &optional (mode :default))
  (let ((index1 (get-char-index face char1))
        (index2 (get-char-index face char2)))
    (with-foreign-object (v 'ft-vector)
      (ft-error (ft-get-kerning face index1 index2 mode v))
      (list (%ft-vector-x v) (%ft-vector-y v)))))

(export 'get-kerning)

(defun get-track-kerning (face point-size degree)
  (with-foreign-object (akerning 'ft-fixed)
    (setf (mem-ref akerning 'ft-fixed) 0)
    (ft-error (ft-get-track-kerning face point-size degree akerning))
    (mem-ref akerning 'ft-fixed)))

(export 'get-track-kerning)
