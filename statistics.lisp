(in-package :raylisp)

;;;# Statistics

;;; FIXME: This is fairly horrible.

(let ((camera-rays 0)
      (reflected 0)
      (refracted 0)
      (intersections 0)
      (hits 0)
      (shadow-tests 0)
      (shadows 0)
      (start 0)
      (stop 0))
  (declare (type unsigned-byte camera-rays reflected refracted intersections
                 hits shadow-tests shadows start stop))
  (defun start-counters ()
    (setf camera-rays 0
	  reflected 0
	  refracted 0
	  intersections 0
	  hits 0
	  shadow-tests 0
	  shadows 0
	  start (get-internal-run-time)))
  (defun stop-counters ()
    (setf stop (get-internal-run-time)))
  (defun get-counters ()
    (list :camera camera-rays
	  :reflected reflected
	  :refracted refracted
	  :intersections intersections
	  :hits hits
	  :shadow-tests shadow-tests
	  :shadows shadows
	  :time (float (/ (- stop start) internal-time-units-per-second))))
  (macrolet ((incf* (place)
               `(incf ,place)
               #+nil
	       `(setf ,place (ldb (byte 32 0) (1+ ,place)))))
    (declare (optimize speed))
    (defun note-camera-ray () (incf* camera-rays))
    (defun note-reflected-ray () (incf* reflected))
    (defun note-refracted-ray () (incf* refracted))
    (defun note-intersection () (incf* intersections))
    (defun note-hit () (incf* hits))
    (defun note-shadow () (incf* shadows))
    (defun note-shadow-test () (incf* shadow-tests))))

;;;## Reporting
;;;
;;; Raylisp collects statistics of the rendering process, so that
;;; we have an idea what the time was spent in.
;;;
;;; TODO: benchmark with and without statistic collection: we can't
;;; afford to spend too much time here.

(defvar *render-verbose* t)

(defun print-report (scene counters &optional (stream *render-verbose*))
  (let ((intersections (getf counters :intersections))
	(hits (getf counters :hits))
	(shadow-tests (getf counters :shadow-tests))
	(shadows (getf counters :shadows)))
    (format stream "~&Objects: ~A~%~
                    Lights:  ~A~%~
                    Initial rays: ~A~%~
                    Reflections:  ~A~%~
                    Refractions:  ~A~%~
                    Intersection tests/hits: ~A / ~A~50T~@[~D%~]~%~
                    Shadow tests/hits:       ~A / ~A~50T~@[~D%~]~%~
                    Internal runtime: ~A seconds~%"
	    (length (scene-objects scene))
	    (length (scene-lights scene))
	    (getf counters :camera)
	    (getf counters :reflected)
	    (getf counters :refracted)
	    intersections hits 
	    (unless (zerop intersections)
	      (round (* 100 (/ hits intersections))))
	    shadow-tests shadows 
	    (unless (zerop shadows)
	      (round (* 100 (/ shadows shadow-tests))))
	    (getf counters :time))))

(defun maybe-report (scene)
  (when *render-verbose*
    (print-report scene (get-counters) *render-verbose*)))

