;;;; by Nikodemus Siivola <nikodemus@random-state.net>, 2009.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :raylisp)

(defclass sphere (scene-object)
  ((radius
    :initform 1.0 :initarg :radius :reader radius-of)
   (location
    :initform +origin+ :initarg :location
    :reader location-of)))

(defun sphere-matrix (sphere)
  (let ((r (radius-of sphere)))
    (matrix* (translate (location-of sphere))
             (scale* r r r))))

(defmethod compute-object-properties ((sphere sphere) scene transform &key shading-object)
  (multiple-value-bind (inverse adjunct/inverse)
      (inverse-and-adjunct/inverse-matrix (matrix* transform (sphere-matrix sphere)))
    (list
     :intersection
     (unless shading-object
       (sb-int:named-lambda sphere-intersection (ray)
         (declare (optimize speed))
         (let* ((o2 (transform-point (ray-origin ray) inverse))
                (d2 (transform-direction (ray-direction ray) inverse))
                (limit +epsilon+))
           (declare (dynamic-extent o2 d2))
           (let ((t1 (quadratic-roots-above limit
                                            (dot-product d2 d2)
                                            (* 2.0 (dot-product d2 o2))
                                            (- (dot-product o2 o2) 1.0))))
             (when (< limit t1 (ray-extent ray))
               (setf (ray-extent ray) t1)
               t)))))
     :normal
     (lambda (point)
       (let ((p (transform-point point adjunct/inverse)))
         (%normalize p p))))))

(defmethod compute-object-extents ((sphere sphere) transform)
  (transform-bounds (vec -1.0 -1.0 -1.0)
                    (vec 1.0 1.0 1.0)
                    (matrix* transform (sphere-matrix sphere))))

(defmethod compute-csg-properties ((sphere sphere) scene transform)
  (let* ((inverse (inverse-matrix (matrix* transform (sphere-matrix sphere))))
	 (compiled (compile-scene-object sphere scene transform :shading-object sphere)))
    (list
     ;; FIXME: To reduce consing even further: stack allocate
     ;; the csg-interactions and pass a continuation in here.
     ;; ...alternatively, pre-allocate a csg-intersection buffer.
     :all-intersections
     (sb-int:named-lambda sphere-all-intersections (origin direction)
       (declare (optimize speed))
       (let ((o (transform-point origin inverse))
             (d (transform-direction direction inverse))
             (limit +epsilon+))
         (declare (dynamic-extent o d))
         (multiple-value-bind (r1 r2)
             (quadratic-roots-above limit
                                    (dot-product d d)
                                    (* 2.0 (dot-product d o))
                                    (- (dot-product o o) 1.0))
           (cond ((= limit r1)
                  #())
                 ((= limit r2)
                  (simple-vector (make-csg-intersection :distance r1 :object compiled)))
                 (t
                  (simple-vector (make-csg-intersection :distance r1 :object compiled)
                                 (make-csg-intersection :distance r2 :object compiled)))))))
     :inside
     (lambda (point)
       (> (+ 1.0 +epsilon+)
          (let ((p (transform-point point inverse)))
            (declare (dynamic-extent p))
            (vec-length p)))))))
