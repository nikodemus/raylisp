;;; Sketch

(defun make-bsp (objects direction)
  (multiple-value-bind (min max) (extreme-bounds objects)
    (let ((mid (vector-div (vector-add min max) 2.0)))
      (multiple-value-bind (left right goodp) (split-objects objects direction mid)
        (if goodp
            (let ((next (flip-direction direction)))
              (%make-bsp
               :direction direction
               :point mid
               :left (make-bsp left next)
               :right (make-bsp right next)))
            objects)))))
