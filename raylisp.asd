(in-package :asdf)

(defsystem :raylisp
  :depends-on (:alexandria :sb-cga)
  :components
  ((:module "kernel"
            :serial t
            :components ((:file "package")
                         (:file "base")
                         (:file "math" )
                         (:file "defaults")
                         (:file "perlin")
                         (:file "statistics")
                         (:file "kernel")
                         (:file "kd-tree")
                         (:file "mixins")
                         (:file "scene")
                         (:file "shader")
                         (:file "pattern")
                         (:file "protocol")
                         (:file "render")
                         (:file "output")))
   (:module "cameras"
            :depends-on ("kernel")
            :components ((:file "orthogonal")
                         (:file "panoramic")
                         (:file "pinhole")))
   (:module "lights"
            :depends-on ("kernel")
            :components ((:file "line-light")
                         (:file "point-light")
                         (:file "solar-light")
                         (:file "spotlight")))
   (:module "objects"
            :depends-on ("kernel")
            :components ((:file "csg")
                         (:file "box" :depends-on ("csg"))
                         (:file "cylinder")
                         (:file "mesh")
                         (:file "plane" :depends-on ("csg"))
                         (:file "sphere" :depends-on ("csg"))
                         (:file "triangle")))
   (:module "patterns"
            :depends-on ("kernel")
            :components ((:file "checker")
                         (:file "gradient")
                         (:file "marble")
                         (:file "tiles")
                         (:file "wood")))
   (:module "shaders"
            :depends-on ("kernel")
            :components ((:file "bump")
                         (:file "composite")
                         (:file "flat")
                         (:file "noise")
                         (:file "phong")
                         (:file "raytrace")
                         (:file "solid")
                         (:file "texture")))
   (:module "models"
            :components ((:static-file "teapot.obj")))
   (:file "tests" :depends-on
          ("kernel"  "cameras" "lights" "objects" "patterns" "shaders"))))
