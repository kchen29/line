;;pixels - 2D array of pixels
(defun write-ppm (filename size pixels)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream
            "P3 ~a ~a 255 ~{~%~{~{~a ~a ~a ~}~}~}"
            (first size) (second size) (2d-array-to-list pixels))))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

;;screen is a 2d array of pixels
;;pixel is a '(r g b)
;;assume octant I for now
(defun draw-line (x0 y0 x1 y1 screen pixel)
  )

;;draws a-size x a-size image
(defun main (a-size)
  (let* ((dimensions (list a-size a-size))
         (screen (make-array dimensions :initial-element '(0 0 0)))
         (color '(0 255 0)))
    (draw-line 249 0 0 249 screen color)
    (write-ppm "output.ppm" dimensions screen)))

(main 500)
