;;; Sketching out a nicer-to-write SDL
;;;
;;; IN-SCENE, OBJECT, and TEXTURE seem unproblematic. So does having
;;; convenience arguments :SCALE, :TRANSLATE, etc. So is :COLOR-MAP instead
;;; of :TYPE :COLOR in addition :MAP <something> ... Rest I'm not sure of.
;;;
;;; PATTERN does not seem too bad. LIGHT I'm sure sure of: the naming seems
;;; a bit off.

(in-package :raylisp-user)

;;; Names the scene. If a previous scene already existed, we clear it up first.
;;; Using :EXTEND T would have kept previous contents.
(in-scene "Test Metals")

;;; MARBLE FLOOR
(flet ((marble (color1 color2 &optional matrix)
         (pattern marble
           :scale 0.5
           :matrix matrix
           :map `((0.0 ,color1)
                  (0.9 ,color1)
                  (1.0 ,color2)))))
  (object plane
    :shader
    (texture
      :scale 6.0
      :pigment (pattern tiles
                 :color-map (list
                             (marble +black+ +white+ (rotate* 1.0 2.7 0.3))
                             (marble +white+ +black+ (translate* 1.2 0.5 1.5))
                             (marble +white+ +black+ (rotate* 0.0 1.0 0.0))
                             (marble +black+ ++white++)))
      :diffuse 1.0)))

;;; Small sky-sphere
(object sphere
  :radius 300.0
  :shader (texture :pigment (rgb 0.6 0.8 1.0)))

;;; Three coppery balls
(flet ((ball (p r o)
         (object sphere
           :location (v p 2.0 -2)
           :radius 3.0
           :shader (texture
                    :pigment (v 1.0 0.55 0.4)
                    :diffuse (- 1.0 r)
                    :reflection r
                    :brilliance 2.0
                    :specular r
                    :roughness o
                    :metallic t
                    :fresnel 0.5))))
  (ball -6 0.5  0.005)
  (ball 0.0 0.6 0.004)
  (ball 6 0.75  0.0025))

;;; Spotlight
(light spot
  :location (v -10 10 -10)
  :point-at (v -3 0 -2)
  :color +white+
  :aperture 0.8)

(light point
  :fill-light t
  :location (v -10 10 -10)
  :color (v 0.1 0.1 0.1))

(camera pinhole
  :name :default
  :location (v 0 6 -10)
  :look-at +origin+)

(render-scene :show t)
