(asdf:defsystem #:clos-calendar
  :description "A calendar library(?) making use of CLOS"
  :author "John Lorentzson (Duuqnd)"
  :license  "Unlicense"
  :version "0.0.1"
  :serial t
  :depends-on (#:local-time #:closer-mop #:str #:messagebox)
  :components ((:file "package")
               (:file "clos-calendar")
               (:file "crappy-tui")))
