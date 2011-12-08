(in-package :freetype2)

(defvar *library*)

(defun freetype-version (&optional (library *library*))
  (if (null-pointer-p (fw-ptr library))
      (values 0 0 0)
      (with-foreign-objects ((amajor 'ft-int)
                             (aminor 'ft-int)
                             (apatch 'ft-int))
        (ft-library-version library amajor aminor apatch)
        (values (mem-ref amajor 'ft-int)
                (mem-ref aminor 'ft-int)
                (mem-ref apatch 'ft-int)))))

(export 'freetype-version)

(defmethod print-object ((object ft-library) stream)
  (multiple-value-bind (maj min pat) (freetype-version object)
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "(FreeType ~A.~A.~A) {#x~8,'0X}" maj min pat
              (pointer-address (fw-ptr object))))))

(defun make-freetype ()
  (make-wrapper (library &library ft-library)
    (progn
      (let ((err (ft-init-freetype &library)))
        (when (eq :ok err)
          (multiple-value-bind (maj min pat) (freetype-version library)
            (unless (and (= maj +version-major+)
                         (= min +version-minor+)
                         (= pat +version-patch+))
              (warn "Freetype2 version mismatch (got ~A.~A.~A, built with ~A.~A.~A), consider rebuilding"
                    maj min pat +version-major+ +version-minor+ +version-patch+))))
        err))
    (ft-done-freetype (p* &library))))

(export 'make-freetype)

(setf *library* (make-freetype))
(export '*library*)
