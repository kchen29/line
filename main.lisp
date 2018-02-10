(eval-when (:compile-toplevel :load-toplevel)
  (load "draw"))

(defun main (a-size filename)
  "Tests draw-line, making an A-SIZE by A-SIZE image. Outputs to FILENAME."
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
    
    (write-ppm filename dimensions screen)
    (display filename)))

(main 500 "output.ppm")
