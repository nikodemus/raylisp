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

(deftype noise-vector ()
  `(simple-array (unsigned-byte 8) (512)))

;;; Generate a noise vector from a given seed: the noise vector repeats all values
;;; from 0 to 255 in random order twice (same order both times.)
(eval-when (:compile-toplevel :load-toplevel)
  (defun generate-noise-vector (seed)
    (let ((*random-state* (sb-kernel::%make-random-state
                           :state (sb-kernel::init-random-state seed)))
          (vector (make-array 512 :element-type '(unsigned-byte 8))))
      ;; Fill the low half
      (dotimes (i 256)
        (setf (aref vector i) i))
      ;; Shuffle it
      (shuffle vector :start 0 :end 256)
      ;; Copy to upper half
      (replace vector vector :start1 256 :start2 0)
      vector)))

(declaim (type noise-vector
               **perlin-noise-vector-0**
               **perlin-noise-vector-1**
               **perlin-noise-vector-2**))
;;; This is the same noise vector Perlin uses -- generating a random one to
;;; use instead would be rank heresy. :)
(sb-ext:defglobal **perlin-noise-vector-0**
    (coerce '(151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225
              140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247
              120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57
              177 33 88 237 149 56 87 174 20 125 136 171 168 68 175
              74 165 71 134 139 48 27 166 77 146 158 231 83 111 229
              122 60 211 133 230 220 105 92 41 55 46 245 40 244 102
              143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89
              18 169 200 196 135 130 116 188 159 86 164 100 109 198
              173 186 3 64 52 217 226 250 124 123 5 202 38 147 118
              126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28
              42 223 183 170 213 119 248 152 2 44 154 163 70 221 153
              101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113
              224 232 178 185 112 104 218 246 97 228 251 34 242 193
              238 210 144 12 191 179 162 241 81 51 145 235 249 14 239
              107 49 192 214 31 181 199 106 157 184 84 204 176 115
              121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24
              72 243 141 128 195 78 66 215 61 156 180 151 160 137 91
              90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30
              69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0
              26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237
              149 56 87 174 20 125 136 171 168 68 175 74 165 71 134
              139 48 27 166 77 146 158 231 83 111 229 122 60 211 133
              230 220 105 92 41 55 46 245 40 244 102 143 54 65 25 63
              161 1 216 80 73 209 76 132 187 208 89 18 169 200 196
              135 130 116 188 159 86 164 100 109 198 173 186 3 64 52
              217 226 250 124 123 5 202 38 147 118 126 255 82 85 212
              207 206 59 227 47 16 58 17 182 189 28 42 223 183 170
              213 119 248 152 2 44 154 163 70 221 153 101 155 167 43
              172 9 129 22 39 253 19 98 108 110 79 113 224 232 178
              185 112 104 218 246 97 228 251 34 242 193 238 210 144
              12 191 179 162 241 81 51 145 235 249 14 239 107 49 192
              214 31 181 199 106 157 184 84 204 176 115 121 50 45 127
              4 150 254 138 236 205 93 222 114 67 29 24 72 243 141
              128 195 78 66 215 61 156 180)
            '(simple-array (unsigned-byte 8) (*))))

(sb-ext:defglobal **perlin-noise-vector-1** (generate-noise-vector 2398239827))
(sb-ext:defglobal **perlin-noise-vector-2** (generate-noise-vector 1001412303))

(declaim (inline fade-noise))
(defun fade-noise (f)
  (declare (single-float f))
  (* f f f (+ (* f (- (* f 6) 15)) 10)))

(declaim (inline noise-grad))
(defun noise-grad (hash x y z)
  (let* ((h (logand hash 15))
         (u (if (< h 8) x y))
         (v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))))
    (+ (if (logtest h 1)
           (- u)
           u)
       (if (logtest h 2)
           (- v)
           v))))

;;; 3-dimensional noise. Implementation based on Ken Perlin's paper 'Improving
;;; Noise' (http://mrl.nyu.edu/~perlin/paper445.pdf) and his reference
;;; implementation (http://mrl.nyu.edu/~perlin/noise)
(declaim (inline noise3))
(defun noise3 (x y z &optional (noise **perlin-noise-vector-0**))
  (declare (single-float x y z)
           (type noise-vector noise)
           (optimize speed))
  (macrolet ((%floor (f) `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                            (floor ,f))))
    (let-values
        ;; Compute unit cube: CX, CY, CZ, and the relative point
        ;; within that cube: X, Y, and Z.
        (((cx x) (%floor x))
         ((cy y) (%floor y))
         ((cz z) (%floor z)))
      (declare (type (signed-byte 32) cx cy cz))
      (let ((cx (logand cx 255))
            (cy (logand cy 255))
            (cz (logand cz 255)))
        (declare (type (unsigned-byte 16) cx cy cz))
        (let ((u (fade-noise x))        ; Fade curves
              (v (fade-noise y))        ; for each main axis
              (w (fade-noise z)))
          (declare (type float u v w))
          (let* ((A  (+ (aref noise cx) cy)) ; Hash coordinates
                 (AA (+ (aref noise A) cz))  ; for cube corners
                 (AB (+ (aref noise (1+ A)) cz))
                 (B  (+ (aref noise (1+ cx)) cy))
                 (BA (+ (aref noise B) cz))
                 (BB (+ (aref noise (1+ B)) cz)))
            ;; Blend and add results from corners
            (lerp w
                  (lerp v
                        (lerp u
                              (noise-grad (aref noise AA) x y z)
                              (noise-grad (aref noise BA) (- x 1) y z))
                        (lerp u
                              (noise-grad (aref noise AB) x (- y 1) z)
                              (noise-grad (aref noise BB) (- x 1) (- y 1) z)))
                  (lerp v
                        (lerp u
                              (noise-grad (aref noise (1+ AA)) x y (- z 1))
                              (noise-grad (aref noise (1+ BA)) (- x 1) y (- z 1)))
                        (lerp u
                              (noise-grad (aref noise (1+ AB)) x (- y 1) (- z 1))
                              (noise-grad (aref noise (1+ BB)) (- x 1) (- y 1) (- z 1)))))))))))

(declaim (ftype (function (vec) (values single-float &optional)) noise))
(defun noise (vec)
  (noise3 (aref vec 0) (aref vec 1) (aref vec 2)))

(declaim (ftype (function (vec vec) (values vec &optional)) %noise-vec))
(defun %noise-vec (result vec)
  (let ((x (aref vec 0))
        (y (aref vec 1))
        (z (aref vec 2)))
    (setf (aref result 0) (noise3 x y z **perlin-noise-vector-0**)
          (aref result 1) (noise3 x y z **perlin-noise-vector-1**)
          (aref result 2) (noise3 x y z **perlin-noise-vector-2**))
    result))

(declaim (ftype (function (vec vec) (values vec &optional)) %abs-noise-vec))
(defun %abs-noise-vec (result vec)
  (let ((x (aref vec 0))
        (y (aref vec 1))
        (z (aref vec 2)))
    (setf (aref result 0) (abs (noise3 x y z **perlin-noise-vector-0**))
          (aref result 1) (abs (noise3 x y z **perlin-noise-vector-1**))
          (aref result 2) (abs (noise3 x y z **perlin-noise-vector-2**)))
    result))

(declaim (ftype (function (vec) (values vec &optional)) noise-vec)
         (inline noise-vec))
(defun noise-vec (vec)
  (%noise-vec (alloc-vec) vec))

(declaim (ftype (function (vec single-float single-float) (values single-float &optional))
                turbulence))
(defun turbulence (vec lo hi)
  "Turbulance function. Based on Ken Perlin's SIGGRAPH 92 course notes."
  (declare (type float lo hi))
  (let ((x (aref vec 0))
        (y (aref vec 1))
        (z (aref vec 2))
        (r 0.0))
    (declare (type float x y z r))
    (labels ((recurse (freq)
               (declare (type float freq))
               (cond ((< freq hi)
                      (setf r (+ r (/ (abs (noise3 x y z)) freq))
                            x (+ x x)
                            y (+ y y)
                            z (+ z z))
                      (recurse (+ freq freq)))
                     (t
                      r))))
      (recurse lo))))

(declaim (ftype (function (vec (and fixnum unsigned-byte) single-float)
                          (values single-float &optional))
                perlin-noise))
(defun perlin-noise (vec octaves persistence)
  (declare (optimize speed))
  (let ((total 0.0)
        (x (aref vec 0))
        (y (aref vec 1))
        (z (aref vec 2)))
    (declare (single-float total))
    (do ((n octaves (1- n))
         ;; freq = (expt 2.0 i)
         (freq 1.0 (+ freq freq))
         ;; ampl = (expt persistence i)
         (ampl 1.0 (* ampl persistence)))
        ((<= n 0) total)
      (declare (single-float freq ampl))
      (incf total (* ampl (noise3 (* x freq) (* y freq) (* z freq)))))))
