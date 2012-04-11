(in-package :freetype2)

 ;; Basic bitmap functions

(defun bitmap-new (&optional (library *library*))
  "=> BITMAP

Create a new FT_Bitmap."
  (make-wrapper (bitmap &bitmap ft-bitmap)
    (progn (ft-bitmap-new &bitmap) :ok)
    (ft-bitmap-done library &bitmap)))

(export 'bitmap-new)

(defun bitmap-convert (bitmap alignment &optional (library *library*))
  "=> NEW-BITMAP

Convert `BITMAP` to an 8bpp bitmap with `ALIGNMENT`-byte alignment."
  (let ((target (bitmap-new library)))
    (ft-bitmap-convert library
                       (fw-ptr bitmap)
                       (fw-ptr target) alignment)
    target))

(export 'bitmap-convert)

 ;; String utility
(defun string-pixel-width (face string &optional (load-flags '(:default)))
  "Get the pixel width of STRING in FACE given LOAD-FLAGS."
  (let ((flags-value (convert-to-foreign load-flags 'ft-load-flags))
        (vert-flag (convert-to-foreign '(:vertical-layout) 'ft-load-flags)))
    (if (= 0 (logand flags-value vert-flag))
       (if (fixed-face-p face)
           (* (length string)
              (ft-26dot6-to-int
               (ft-size-metrics-max-advance (ft-size-metrics (ft-face-size face)))))
           (+ (reduce #'+ (get-string-advances face string load-flags))
              (reduce #'+ (get-string-kerning face string))))
       (ft-size-metrics-x-ppem (ft-size-metrics (ft-face-size face))))))

(export 'string-pixel-width)

(defun face-ascender-pixels (face)
  "Return the max ascender for FACE, in pixels."
  (ft-26dot6-to-float
   (ft-size-metrics-ascender (ft-size-metrics (ft-face-size face)))))

(export 'face-ascender-pixels)

(defun face-descender-pixels (face)
  "Return the max descender for FACE, in pixels."
  (ft-26dot6-to-float
   (- (ft-size-metrics-descender (ft-size-metrics (ft-face-size face))))))

(export 'face-descender-pixels)

(defun string-pixel-height (face string &optional (load-flags '(:default)))
  "Get the pixel height of STRING in FACE given LOAD-FLAGS."
  (let ((flags-value (convert-to-foreign load-flags 'ft-load-flags))
        (vert-flag (convert-to-foreign '(:vertical-layout) 'ft-load-flags)))
    (if (/= 0 (logand flags-value vert-flag))
        (if (fixed-face-p face)
            (* (length string)
               (ft-size-metrics-y-ppem (ft-size-metrics (ft-face-size face))))
            (reduce #'+ (get-string-advances face string flags-value)))
        (+ (face-ascender-pixels face) (face-descender-pixels face)))))

(export 'string-pixel-height)

 ;; Bitmap

(defun nth-mono-pixel (row n)
  (multiple-value-bind (q offset) (truncate n 8)
    (let ((byte (mem-ref row :unsigned-char q)))
    (if (logbitp (- 7 offset) byte) 1 0))))

(defun nth-gray-pixel (row n)
  (mem-ref row :unsigned-char n))

(defun bitmap-to-array (bitmap)
  "=> ARRAY

Convert `BITMAP` from internal `FT_Bitmap`'s internal representation to
a native array.  This is specified for a `FT-BITMAP-PIXEL-FORMAT` of `:MONO`,
`:GRAY`, `:LCD`, and `:LCD-V`.

Note that for :LCD and :LCD-V, the result is a either 3\\*width or
3\\*height, respectively.  This may change in the future."
  (let ((buffer (ft-bitmap-buffer bitmap))
        (rows (ft-bitmap-rows bitmap))
        (width (ft-bitmap-width bitmap))
        (pitch (ft-bitmap-pitch bitmap))
        (format (ft-bitmap-pixel-mode bitmap)))
    (let ((pixel-fn (ecase format
                      (:mono #'nth-mono-pixel)
                      (:gray #'nth-gray-pixel)
                      (:lcd #'nth-gray-pixel)
                      (:lcd-v #'nth-gray-pixel)))
          (array (make-array (list rows width) :element-type 'unsigned-byte)))
      (declare (function pixel-fn))
      #+-(format t "buffer: ~A rows: ~A width: ~A pitch: ~A format: ~A~%"
                 buffer rows width pitch format)
      (loop for i from 0 below rows
            as ptr = (inc-pointer buffer (* i pitch))
            do (loop for j from 0 below width
                     do (setf (aref array i j) (funcall pixel-fn ptr j)))
            finally (return (values array format))))))

(export 'bitmap-to-array)

 ;; ABLIT

(defun flat-array (arr)
  (make-array (apply #'* (array-dimensions arr))
              :displaced-to arr))

(defun row-width (arr)
  (let ((dim (array-dimensions arr)))
    (if (> (array-rank arr) 2)
        (* (car dim) (caddr dim))
        (car dim))))

(defun ablit (arr1 arr2 &key (x 0) (y 0))
  "Destructivly copy arr2 into arr1 for 2- and 3-dimensional (Y:X, Y:X:RGB(A))
arrays.  X and Y may be specified as a 2D offset into ARR1."
  (assert (= (array-rank arr1) (array-rank arr2)))
  (let ((flat1 (flat-array arr1))
        (flat2 (flat-array arr2))
        (height1 (row-width arr1))
        (height2 (row-width arr2))
        (width1 (array-dimension arr1 1))
        (width2 (array-dimension arr2 1))
        (xoff (* x (if (= (array-rank arr1) 3)
                       (array-dimension arr1 2)
                       1))))
    (loop for y2 from 0 below height2
          for y1 from y below height1
          do (let ((x1 (+ (* y1 width1) xoff))
                   (x2 (* y2 width2)))
               (replace flat1 flat2
                        :start1 x1
                        :end1 (* (1+ y1) width1)
                        :start2 x2
                        :end2 (+ x2 width2)))))
  arr1)


(defun ablit-from-nonzero (arr1 arr2 &key (x 0) (y 0))
  "Destructivly copy arr2 into arr1 for 2- and 3-dimensional (Y:X,
Y:X:RGB(A)) arrays.  X and Y may be specified as a 2D offset into
ARR1.  Copying is started from the first nonzero element in each row.
This is a hack to make kerned fonts render properly with the toy
interface."
  (assert (= (array-rank arr1) (array-rank arr2)))
  (let ((flat1 (flat-array arr1))
        (flat2 (flat-array arr2))
        (height1 (row-width arr1))
        (height2 (row-width arr2))
        (width1 (array-dimension arr1 1))
        (width2 (array-dimension arr2 1))
        (xoff (* x (if (= (array-rank arr1) 3)
                       (array-dimension arr1 2)
                       1))))
    (loop for y2 from 0 below height2
          for y1 from y below height1
          as start2 = (* y2 width2)
          as end2 = (+ start2 width2)
          do (let ((x1 (+ (* y1 width1) xoff))
                   (x2 (position-if-not #'zerop flat2 :start start2
                                                      :end end2)))
               (when x2
                 (replace flat1 flat2
                          :start1 (+ x1 (- x2 start2))
                          :end1 (* (1+ y1) width1)
                          :start2 x2
                          :end2 end2)))))
  arr1)
