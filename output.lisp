(in-package :raylisp)

;;;# Image file output
;;;
;;; In the future images should be responsible for their own part in the
;;; rendering process, determining the size, etc.

(defun save-ppm (raster pathname)
  (let ((width (array-dimension raster 0))
	(height (array-dimension raster 1)))
    (with-open-file (ppm pathname 
		     :direction :output :if-exists :supersede
		     :element-type '(unsigned-byte 8))
      (flet ((puts (string)
	       (write-sequence (map 'simple-vector #'char-code string) 
			       ppm)
	       (write-byte (char-code #\newline) ppm))
	     (color-byte (x y c)
	       (min 255 (truncate (* 255.0 (aref (aref raster x y) c))))))
	(puts "P6")
	(puts (format nil "~D ~D" width height))
	(puts "255")
	(dotimes (y height)
	  (dotimes (x width)
	    (dotimes (c 3)
	      (write-byte (color-byte x y c) ppm)))))
      (truename ppm))))

