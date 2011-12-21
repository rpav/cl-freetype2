(in-package :freetype2)

(defun generate-freetype2-docs (path stream)
  (gendoc (:path path :stream stream
           :title "cl-freetype2"
           :css "ft2docs.css")
    (:mdf "abstract.md")
    (:mdf "toc.md")
    (:mdf "intro.md")
    (:mdf "example.md")
    (:apiref :freetype2)
    (:mdf "internal.md")
    (:mdf "about.md")))

(defun generate-freetype2-types-docs (path stream)
  (gendoc (:path path :stream stream
           :title "cl-freetype2-types"
           :css "ft2docs.css")
    (:apiref :freetype2-types)))

(defun generate-freetype2-ffi-docs (path stream)
  (gendoc (:path path :stream stream
           :title "cl-freetype2-ffi"
           :css "ft2docs.css")
    (:apiref :freetype2-ffi)))

(defun generate-docs ()
  (let ((path (asdf:component-pathname (asdf:find-component :cl-freetype2-doc '("gendoc")))))
    (with-open-file (stream (merge-pathnames "cl-freetype2.html" path)
                            :direction :output :if-exists :supersede)
      (generate-freetype2-docs path stream))
    (with-open-file (stream (merge-pathnames "cl-freetype2-types.html" path)
                            :direction :output :if-exists :supersede)
      (generate-freetype2-types-docs path stream))
    (with-open-file (stream (merge-pathnames "cl-freetype2-ffi.html" path)
                            :direction :output :if-exists :supersede)
      (generate-freetype2-ffi-docs path stream))))
