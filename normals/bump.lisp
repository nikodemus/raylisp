;;; Provides diffuse and ambient components.

(in-package :raylisp)

(defclass bump-normal (normal)
  ((height
    :initarg :height
    :initform 1.0
    :reader normal-height)))

(defmethod compute-perturbation-function ((normal bump-normal) transform)
  (let ((inverse (inverse-matrix transform))
        (height (coerce (normal-height normal) 'single-float)))
    (perturbation-lambda bump-normal (result normal point)
      (declare (optimize speed))
      (let ((p2 (transform-point point inverse)))
        (declare (dynamic-extent p2))
        (let ((noise (vector-dnoise p2)))
          (%vec* noise noise height)
          (%vec+ result normal noise)
          (%normalize result result)
          result)))))