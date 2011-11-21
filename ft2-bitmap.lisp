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
