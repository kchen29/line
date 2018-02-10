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

