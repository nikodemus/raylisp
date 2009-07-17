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

;;;; SHADERS
;;;;
;;;; Subclasses of SHADER implement different object shaders: during scene
;;;; compilation COMPUTE-SHADER-FUNCTION is called for shaders attached to
;;;; scene objects. The returned function is then called during rendering to
;;;; compute the visible color for points on object surface.

(deftype shader-function ()
  "Shader function type: functions returned by COMPUTE-SHADER-FUNCTION should
have this type. First argument is the intersection pont, second is the surface
normal at point (on visible side), third is the dot product of the outward
pointing surface normal and ray direction, and fourth is the ray. The returned
value is the visible color."
  `(function (vec vec single-float ray t) (values vec &optional)))

(defmacro shader-lambda (&whole form name lambda-list &body body)
  "Provides automatic type declarations for shader function, and allows the
function to be named, and adds a block with that name around the body."
  (destructuring-bind (vector normal n.d ray counters) lambda-list
    (declare (ignore counters))
    (multiple-value-bind (forms declarations doc)
        (parse-body body :documentation t :whole form)
      `(sb-int:named-lambda ,name ,lambda-list
         ,@(when doc (list doc))
         (declare (type vec ,vector ,normal)
                  (type single-float ,n.d)
                  (type ray ,ray)
                  (optimize (sb-c::recognize-self-calls 0)))
         ,@declarations
         (the vec (values (block ,name ,@forms)))))))

(defun constant-shader-function (value)
  (check-type value vec)
  (shader-lambda constant-shader-lambda (point normal n.d ray counters)
    (declare (ignore point normal n.d ray counters) (optimize (safety 0)))
    value))

(defgeneric compute-shader-function (shader object scene transform))

(defmethod compute-shader-function :around (shader object scene transform)
  ;; Since the rest of the system trusts the types of shaders functions,
  ;; make sure the function type looks about right.
  (check-function-type (call-next-method) 'shader-function))

(defmethod compute-shader-function :around ((shader shader) object scene transform)
  ;; Apply the transform of the shader: otherwise all subclasses need to do
  ;; this.
  (call-next-method shader object scene (matrix* transform (transform-of shader))))

(declaim (ftype (function (t t t t) (values shader-function &optional))
                compile-shader))
(defun compile-shader (shader object scene transform)
  (declare (type scene-object object))
  (if shader
      (compute-shader-function shader object scene transform)
      (constantly black)))

(declaim (inline coefficient))
(defun coefficient (value shader)
  (declare (float value))
  (/ value (the float (shader-weight shader))))

(declaim (inline shade))
(defun shade (object ray counters)
  (declare (optimize speed))
  (let* ((point (adjust-vec (ray-origin ray) (ray-direction ray)
                            (ray-extent ray)))
	 (normal (funcall (object-normal object) point))
	 (n.d (dot-product normal (ray-direction ray))))
    (flet ((%shade (n)
             (funcall (object-shader object)
                      point
                      n
                      n.d
                      ray
                      counters)))
      (if (plusp n.d)
          (let ((n2 (vec* normal -1.0)))
            (declare (dynamic-extent n2))
            (%shade n2))
          (%shade normal)))))
