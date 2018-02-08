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

;;plots a point on the screen
(defun plot (x y screen pixel)
  (setf (aref screen x y) pixel))

;;screen is a 2d array of pixels
;;pixel is a '(r g b)
;;assume octant I for now
(defun draw-line (x0 y0 x1 y1 screen pixel)
  (do* ((x x0 (1+ x))
        (y y0)
        (A (- y1 y0))
        (B (- x0 x1))
        (d (+ (* 2 A) B) (+ d (* 2 A))))
       ((> x x1))
    (plot x y screen pixel)
    (when (> d 0)
      (incf y)
      (incf d (* 2 B)))))

;;draws a-size x a-size image
(defun main (a-size)
  (let* ((dimensions (list a-size a-size))
         (screen (make-array dimensions :initial-element '(0 0 0)))
         (color '(0 255 0)))
    (draw-line 0 0 249 249 screen color)
    (write-ppm "output.ppm" dimensions screen)))

(main 500)
