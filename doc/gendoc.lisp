(in-package :freetype2)

 ;; Parts

(defvar *part-processor* (make-hash-table))

(defun add-processor (name function)
  (setf (gethash name *part-processor*) function))

 ;; Utility

(defun read-all (file-stream)
  (let ((output (make-array (file-length file-stream)
                            :element-type (stream-element-type file-stream))))
    (read-sequence output file-stream)
    output))

#+sbcl
(defun arglist (function)
  (sb-introspect:function-lambda-list function))

#+ccl
(defun arglist (function)
  (multiple-value-bind (arglist binding)
      (ccl:arglist function)
    (declare (ignore binding))
    arglist))

#+clisp
(defun arglist (function)
  (ext:arglist function))

 ;; Gendoc Macro

(defmacro gendoc ((&key (stream *standard-output*) (title "GenDoc Documentation") css path)
                  &body parts)
  (let ((part (gensym))
        (proc-name (gensym))
        (processor (gensym))
        (html (gensym)))
    `(let ((*default-pathname-defaults* ,path))
       (cl-who:with-html-output (,html ,stream :prologue t :indent t)
         (:head (and ,title (cl-who:htm (:title (cl-who:str ,title))))
           (and ,css (cl-who:htm (:link :rel "stylesheet" :type "text/css" :href ,css))))
         (:html
           (loop for ,part in ',parts
                 do (let* ((,proc-name (pop ,part))
                           (,processor (gethash ,proc-name *part-processor*)))
                      (funcall ,processor ,stream ,proc-name ,part))))))))

 ;; Processors

(defun process-text-file (stream name part)
  (declare (ignore name))
  (let ((filename (car part))
        (*standard-output* stream))
    (write-line "<pre>")
    (with-open-file (input filename)
      (princ (read-all input)))
    (write-line "</pre>")))

(add-processor :text-file #'process-text-file)
(add-processor :txt #'process-text-file)

(defun process-markdown-file (stream name part)
  (declare (ignore name))
  (let ((filename (car part)))
    (with-open-file (input filename)
      (markdown:markdown (read-all input) :stream stream))))

(add-processor :markdown-file #'process-markdown-file)
(add-processor :mdf #'process-markdown-file)

(defun special-p (symbol)
  (or (boundp symbol)
      (documentation symbol 'variable)))

(defun function-p (symbol)
  (and (fboundp symbol)
       (not (macro-function symbol))))

(defun macro-p (symbol)
  (macro-function symbol))

(defun apiref-symbols (type package)
  (let (symbols)
    (loop for symbol being each external-symbol in package
          if (ecase type
               (:special (special-p symbol))
               (:macro (macro-p symbol))
               (:function (function-p symbol)))
            do (push symbol symbols))
    (sort symbols #'string<)))

(defun apiref-spec (type sym)
  (declare (ignore type))
  (string sym))

(defun apiref-lambda (type sym)
  (ecase type
    (:special "")
    ((or :macro :function)
     (if (arglist sym)
         (write-to-string (arglist sym))
         "()"))))

(defun apiref-result (type sym)
  (ecase type
    (:special "")
    ((or :macro :function)
     (let ((ds (documentation sym 'function)))
       (if (and (> (length ds) 0)
                (string= (subseq ds 0 2) "=>"))
           (subseq ds 0 (position #\Newline ds))
           "")))))

(defun apiref-doc (type sym)
  (or 
   (ecase type
     (:special (documentation sym 'variable))
     ((or :macro :function)
      (let ((ds (documentation sym 'function)))
        (if (and (> (length ds) 0)
                 (string= (subseq ds 0 2) "=>"))
            (subseq ds (position #\Newline ds))
            ds))))
   "*Undocumented!*"))

(defun apiref-section-symbol (stream type symbol)
  (cl-who:with-html-output (html stream :indent t)
    (:a :name symbol :class "apiref-row")
    (:div :class "apiref-spec"
      (cl-who:str (apiref-spec type symbol)))
    (:div :class "apiref-lambda"
      (cl-who:str (apiref-lambda type symbol)))
    (:div :class "apiref-result"
      (cl-who:str (apiref-result type symbol)))
    (:div :class "apiref-doc"
      (cl-who:str
       (with-output-to-string (s)
         (markdown:markdown (apiref-doc type symbol) :stream s))))))

(defun gen-apiref (stream package)
  (let ((*package* package))
    (cl-who:with-html-output (html stream :indent t)
      (:h2 "Special Variables")
      (loop for sym in (apiref-symbols :special package)
            do (apiref-section-symbol stream :special sym))
      (:h2 "Functions")
      (loop for sym in (apiref-symbols :function package)
            do (apiref-section-symbol stream :function sym))
      (:h2 "Macros")
      (loop for sym in (apiref-symbols :macro package)
            do (apiref-section-symbol stream :macro sym)))))

(defun process-apiref (stream name package-list)
  (declare (ignore name))
  (cl-who:with-html-output (html stream :indent t)
    (loop for package-name in package-list
          do (cl-who:htm
              (:a :name (concatenate 'string
                                     "REFERENCE-"
                                     (string package-name)))
              (:h1 "Reference: " (cl-who:str package-name))
              (let ((package (find-package package-name)))
                (if package
                    (gen-apiref stream package)
                    (cl-who:htm (:p "Package not found."))))))))

(add-processor :apiref 'process-apiref)
