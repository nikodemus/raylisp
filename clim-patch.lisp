;;;; Thanks to Andy Hefner!

(in-package :clim-clx)

;;;; Image drawing

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'clim::medium-draw-pixels* :clim)
  (export 'clim::medium-get-pixels*  :clim))

(defgeneric clim::medium-draw-pixels* (medium array x y &key &allow-other-keys))
(defgeneric clim::medium-get-pixels* (medium array x y &key width height &allow-other-keys))

;;; TODO: Extract indexed pattern drawing and convert to one of these functions.

(defmethod clim::medium-draw-pixels* ((sheet sheet) array x y &key)
  (with-sheet-medium (medium sheet)
    (clim::medium-draw-pixels* medium array x y)))

(defmethod clim::medium-draw-pixels* 
    ((medium clx-medium) array x y &key &allow-other-keys)
  (let* ((width  (array-dimension array 1))
         (height (array-dimension array 0))
         (image (xlib:create-image :width width :height height :data array
                                                               :bits-per-pixel 32
                                                               :depth 24
                                                               :format :z-pixmap)))
    (with-clx-graphics (medium)
      (xlib:put-image mirror gc image :x x :y y :width width :height height))))
 
(defmethod clim::medium-get-pixels* 
    ((medium clx-medium) array x y &key width height &allow-other-keys)
  (let* ((width  (or width (array-dimension array 1)))
         (height (or height (array-dimension array 0))))
    (with-clx-graphics (medium)
      (xlib:image-z-pixarray (xlib:get-image mirror :x x :y y :format :z-pixmap :width width :height height)))))
