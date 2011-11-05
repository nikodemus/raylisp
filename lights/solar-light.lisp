(in-package :raylisp)

(defclass solar-light (scene-light color-mixin direction-mixin)
  ()
  (:documentation
   "Solar light appears to shine from the same direction and distance
everywhere in the scene, simulating an effectively infinitely distant light
source such as the sun or moon."))

(defmethod compute-light-properties ((light solar-light) scene)
  (let ((nd (normalize (direction-of light)))
	(color (color-of light)))
    ;; I'm sure there was a reason not to use SHADOW-FUNCTION here...
    (list
     :incident-light
     (constantly nd)
     :illumination
     (lambda (point lv counters)
       (with-ray (ray :origin point :direction lv)
	 (if (find-scene-intersection ray scene counters t)
	     (values +black+ -1.0)
	     (values color 1.0)))))))
