(in-package :asdf)

(defsystem :raylisp-gui
  :depends-on (:raylisp :mcclim)
  :components
  ((:module "gui"
            :components ((:file "clim-patch")
                         (:file "gui" :depends-on ("clim-patch"))))))
