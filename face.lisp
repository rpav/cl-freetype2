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

(defun fixed-face-p (face)
  (not (null (find :fixed-width (ft-face-face-flags face)))))

(export 'fixed-face-p)

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

(defun set-transform (face matrix delta)
  (let ((ft-matrix (convert-matrix matrix))
        (ft-vector (convert-vector delta)))
    (ft-set-transform face ft-matrix ft-vector)))

(export 'set-transform)

(defun get-kerning (face char1 char2 &optional mode)
  (let ((index1 (get-char-index face char1))
        (index2 (get-char-index face char2)))
    (with-foreign-object (v 'ft-vector)
      (ft-error (ft-get-kerning face index1 index2 (or mode :default) v))
      (let ((kern (%ft-vector-x v)))
        (if (or mode (fixed-face-p face))
            kern
            (ft-26dot6-to-int kern))))))

(export 'get-kerning)

(defun get-string-kerning (face string &optional mode)
  (let ((kern (make-array (length string) :initial-element 0)))
    (loop for i from 1 below (length string)
          as c1 = (aref string (1- i))
          as c2 = (aref string i)
          do (setf (aref kern i)
                   (get-kerning face c1 c2 mode)))
    kern))

(defun get-track-kerning (face point-size degree)
  (with-foreign-object (akerning 'ft-fixed)
    (setf (mem-ref akerning 'ft-fixed) 0)
    (ft-error (ft-get-track-kerning face point-size degree akerning))
    (mem-ref akerning 'ft-fixed)))

(export 'get-track-kerning)

(defun get-glyph-name (face char-or-code)
  (with-foreign-pointer (buffer 64 len)
    (ft-error (ft-get-glyph-name face (get-char-index face char-or-code)
                                 buffer len))
    (foreign-string-to-lisp buffer :max-chars len)))

(export 'get-glyph-name)

(defun get-advance (face char-or-code &optional load-flags)
  (let ((gindex (get-char-index face char-or-code)))
    (with-foreign-object (padvance 'ft-fixed)
      (if (eq :ok (ft-get-advance face gindex (cons :fast-advance-only load-flags) padvance))
          (mem-ref padvance 'ft-fixed)
          (progn
            (load-glyph face gindex load-flags)
            (ft-26dot6-to-float
             (ft-vector-x (ft-glyphslot-advance (ft-face-glyph face)))))))))

(export 'get-advance)

(defun get-string-advances (face string &optional load-flags)
  (let ((advance (make-array (length string) :element-type 'float
                                             :initial-element 0.0)))
    (loop for c across string
          for i from 0
          do (setf (aref advance i)
                   (get-advance face c load-flags)))
    advance))

(export 'get-string-advances)
