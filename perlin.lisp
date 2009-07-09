(in-package :raylisp)

(declaim (ftype (function (vec) (values float &optional)) vector-noise))
(declaim (ftype (function (vec float float) (values float &optional)) turbulence perlin-noise))

(declaim (type (simple-array (unsigned-byte 8) (512)) +perlin-noise-vector+))
(define-constant +perlin-noise-vector+
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
            '(simple-array (unsigned-byte 8) (*)))
  :test #'equalp)

(locally (declare (optimize speed))
  (labels
     ((lerp (v a b)
        (declare (type float v a b))
        (+ a (* v (- b a))))
      (%%truncate (f)
        (declare (type float f))
        (let ((res (sb-kernel:%unary-truncate f)))
              (values res (- f res))))
      (%truncate (f)
        (declare (type float f))
        (if (< #.(float most-negative-fixnum) f #.(float most-positive-fixnum))
            (let ((res (sb-ext:truly-the fixnum (sb-kernel:%unary-truncate f))))
              (values res (- f res)))
            (%%truncate f)))
      (%floor (f)
        (declare (type float f))
        (multiple-value-bind (tru rem) (%truncate f)
          (if (and (not (zerop rem)) (minusp f))
              (values (1- tru) (1+ rem))
              (values tru rem))))
      (noise1 (i)
        (declare (type (integer 0 512) i) (optimize (safety 0)))
        (aref +perlin-noise-vector+ i))
      (noise3 (x y z)
        (declare (float x y z))
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
            (flet ((fade (e)
                     (declare (float e))
                     ;; Fade curve
                     (+ (* 6 (power e 5))
                        (* -15 (power e 4))
                        (* 10 (power e 3))))
                   (grad (hash x y z)
                     (declare (type (unsigned-byte 16) hash)
                              (type float x y z))
                     ;; Low 4bits hash code
                     ;; into 12 grad.dirs
                     (let* ((h (logand hash 15))
                            (u (if (or (< h 8) (= h 12) (= h 13))
                                   x
                                   y))
                            (v (if (or (< h 4) (= h 12) (= h 13))
                                   y
                                   z)))
                       (+ (if (= 0 (logand h 1)) u (- u))
                          (if (= 0 (logand h 2)) v (- v))))))
              (declare (inline grad))
              (let ((u (fade x))        ; Fade curves
                    (v (fade y))        ; for each main axis
                    (w (fade z)))
                (declare (type float u v w))
                (let* ((A  (+ (noise1 cx) cy))   ; Hash coordinates
                       (AA (+ (noise1 A) cz))    ; for cube corners
                       (AB (+ (noise1 (1+ A)) cz))
                       (B  (+ (noise1 (1+ cx)) cy))
                       (BA (+ (noise1 B) cz))
                       (BB (+ (noise1 (1+ B)) cz)))
                  ;; Blend and add results from corners
                  (lerp
                   w
                   (lerp
                    v
                    (lerp
                     u
                     (grad (noise1 AA) x y z)
                     (grad (noise1 BA) (- x 1) y z))
                    (lerp
                     u
                     (grad (noise1 AB) x (- y 1) z)
                     (grad (noise1 BB) (- x 1) (- y 1) z)))
                   (lerp
                    v
                    (lerp
                     u
                     (grad (noise1 (1+ AA)) x y (- z 1))
                     (grad (noise1 (1+ BA)) (- x 1) y (- z 1)))
                    (lerp
                     u
                     (grad (noise1 (1+ AB)) x (- y 1) (- z 1))
                     (grad (noise1 (1+ BB)) (- x 1) (- y 1) (- z 1))))))))))))
   (declare (ftype (function (float float float) (values float &optional)) noise3))
   (declare (inline %floor %truncate))

   (defun vector-noise (v)
     "3-dimensional noise. Implementation based on Ken Perlin's paper
'Improving Noise' (http://mrl.nyu.edu/~perlin/paper445.pdf) and his reference
implementation (http://mrl.nyu.edu/~perlin/noise). An interesting property --
not a bug -- of this implementation is that noise *seems* to be 0.0 at each
point on a unit-cube lattice."
     (with-arrays (v)
       (noise3 (v 0) (v 1) (v 2))))

   (defun turbulence (v lo hi)
     "Turbulance function. Based on Ken Perlin's SIGGRAPH 92 course notes."
     (declare (type float lo hi))
     (with-arrays (v)
       (let ((x (v 0))
             (y (v 1))
             (z (v 2))
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
           (recurse lo)))))

   (defun perlin-noise (v octaves persistence)
     (with-arrays (v)
       (let ((total 0.0)
             (x (v 0))
             (y (v 1))
             (z (v 2)))
         ;; FIXME: Check this: compare versus GRT implementation.
         (do ((n octaves (1- n))
              (freq 1.0 (+ freq freq))
              (ampl 1.0 (* ampl persistence)))
             ((zerop n) total)
           (declare (float freq ampl))
           (incf total (* ampl (noise3 (* x freq) (* y freq) (* z freq))))))))))
