(in-package :freetype2)

 ;; Bitmap

(defun nth-mono-pixel (row n)
  (multiple-value-bind (q offset) (truncate n 8)
    (let ((byte (mem-ref row :unsigned-char q)))
    (if (logbitp (- 7 offset) byte) 1 0))))

(defun nth-gray-pixel (row n)
  (mem-ref row :unsigned-char n))

(defun bitmap-to-array (bitmap)
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
            finally (return array)))))

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
        (width1 (row-width arr1))
        (width2 (row-width arr2))
        (height1 (array-dimension arr1 1))
        (height2 (array-dimension arr2 1))
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
