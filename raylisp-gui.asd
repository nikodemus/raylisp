(defsystem :raylisp-gui
  :depends-on (:raylisp :mcclim)
  :components ((:file "clim-patch")
               (:file "gui" :depends-on ("clim-patch"))))
