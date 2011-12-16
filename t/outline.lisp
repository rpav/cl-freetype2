(in-package :freetype2-tests)
(in-suite freetype2-tests)

(defvar *outline*)
(defvar *curves*)

(defvar *result-a-curve*
  '((:MOVETO (220 0) NIL NIL) (:LINETO (424 844) NIL NIL)
    (:LINETO (627 0) NIL NIL) (:LINETO (817 0) NIL NIL)
    (:LINETO (527 1088) NIL NIL) (:LINETO (327 1088) NIL NIL)
    (:LINETO (31 0) NIL NIL) (:LINETO (220 0) NIL NIL))
  "This is a validated result for the A glyph in the test font.")

(test (test-load-outline :depends-on test-set-size)
  (finishes (setf *outline* (get-outline *face* #\A)))
  (finishes (outline-check *outline*)))

(test (test-outline-boxes :depends-on test-load-outline)
  (let (bbox cbox)
    (is (typep (setf cbox (outline-get-cbox *outline*)) 'ft-bbox))
    (is (typep (setf bbox (outline-get-bbox *outline*)) 'ft-bbox))
    (is (= 31 (ft-bbox-xmin cbox)))
    (is (= 0 (ft-bbox-ymin cbox)))
    (is (= 817 (ft-bbox-xmax cbox)))
    (is (= 1088 (ft-bbox-ymax cbox)))
    (is (= 31 (ft-bbox-xmin bbox)))
    (is (= 0 (ft-bbox-ymin bbox)))
    (is (= 817 (ft-bbox-xmax bbox)))
    (is (= 1088 (ft-bbox-ymax bbox)))))

(test (test-outline-orientation :depends-on test-load-outline)
  (is (eq :fill-left (outline-get-orientation *outline*))))

(test (test-outline-decompose :depends-on test-load-outline)
  (let (curves)
    (finishes
      (do-outline-decompose *outline* (&rest curve)
        (let ((v (cadr curve)))
          (setf (cadr curve) (list (ft-vector-x v)
                                   (ft-vector-y v)))
          (push curve curves))))
      (setf *curves* (nreverse curves)))
  (is (equal *result-a-curve* *curves*)))
