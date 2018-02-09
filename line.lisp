;;pixels - 2D array of pixels
(defun write-ppm (filename size pixels)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream
            "P3 ~a ~a 255 ~{~%~{~{~a ~a ~a ~}~}~}"
            (first size) (second size) (2d-array-to-list pixels))))

;;modifies place to put so that (0, 0) is lower left corner of screen
;;increasing x traverses horizontally
(defun 2d-array-to-list (array)
  (loop for y from (1- (array-dimension array 1)) downto 0
        collect (loop for x below (array-dimension array 0)
                      collect (aref array x y))))

;;plots a point on the screen
;;note: does not copy the pixel
(defun plot (x y screen pixel)
  (setf (aref screen x y) pixel))

;;screen is a 2d array of pixels
;;pixel is a '(r g b)
(defun draw-octant-1-line (x0 y0 x1 y1 screen pixel)
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

(defun draw-octant-2-line (x0 y0 x1 y1 screen pixel)
  (do* ((x x0)
        (y y0 (1+ y))
        (A (- y1 y0))
        (B (- x0 x1))
        (d (+ A (* 2 B)) (+ d (* 2 B))))
       ((> y y1))
    (plot x y screen pixel)
    (when (< d 0)
      (incf x)
      (incf d (* 2 A)))))

(defun draw-octant-7-line (x0 y0 x1 y1 screen pixel)
  (do* ((x x0)
        (y y0 (1- y))
        (A (- y1 y0))
        (B (- x0 x1))
        (d (- A (* 2 B)) (- d (* 2 B))))
       ((< y y1))
    (plot x y screen pixel)
    (when (> d 0)
      (incf x)
      (incf d (* 2 A)))))

(defun draw-octant-8-line (x0 y0 x1 y1 screen pixel)
  (do* ((x x0 (1+ x))
        (y y0)
        (A (- y1 y0))
        (B (- x0 x1))
        (d (- (* 2 A) B) (+ d (* 2 A))))
       ((> x x1))
    (plot x y screen pixel)
    (when (< d 0)
      (decf y)
      (decf d (* 2 B)))))

(defun draw-line (x0 y0 x1 y1 screen pixel)
  (when (minusp (- x1 x0))
    (rotatef x0 x1)
    (rotatef y0 y1))
  (let ((xdif (- x1 x0))
        (ydif (- y1 y0)))
    (if (>= ydif 0)
        (if (minusp (- ydif xdif))
            (draw-octant-1-line x0 y0 x1 y1 screen pixel)
            (draw-octant-2-line x0 y0 x1 y1 screen pixel))
        (if (minusp (+ ydif xdif))
            (draw-octant-7-line x0 y0 x1 y1 screen pixel)
            (draw-octant-8-line x0 y0 x1 y1 screen pixel)))))

;;draws a-size x a-size image
(defun main (a-size)
  (let* ((dimensions (list a-size a-size))
         (half-way (/ a-size 2))
         (full-way (1- a-size))
         (screen (make-array dimensions :initial-element '(0 0 0)))
         (color '(0 255 0)))
    
    ;;octant 1 and 5
    (draw-line 0 0 full-way full-way screen color)
    (draw-line 0 0 full-way half-way screen color)
    (draw-line full-way full-way 0 half-way screen color)

    ;;octant 8 and 4
    (setf color '(0 255 255))
    (draw-line 0 full-way full-way 0 screen color)
    (draw-line 0 full-way full-way half-way screen color)
    (draw-line full-way 0 0 half-way screen color)

    ;;octant 2 and 6
    (setf color '(255 0 0))
    (draw-line 0 0 half-way full-way screen color)
    (draw-line full-way full-way half-way 0 screen color)

    ;;octant 7 and 3
    (setf color '(255 0 255))
    (draw-line 0 full-way half-way 0 screen color)
    (draw-line full-way 0 half-way full-way screen color)

    ;;horizontal and vertical
    (setf color '(255 255 0))
    (draw-line 0 half-way full-way half-way screen color)
    (draw-line half-way 0 half-way full-way screen color)
    
    (write-ppm "output.ppm" dimensions screen)))

(main 500)
