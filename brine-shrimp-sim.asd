;;;; brine-shrimp-sim.asd

(asdf:defsystem #:brine-shrimp-sim
  :description "A gag game."
  :author "Tassos Manganaris <tassos.manganaris@gmail.com>"
  :license  "GPL V3"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl.sdl2
               #:rtg-math #:rtg-math.vari
               #:dendrite #:skitter #:cepl.skitter.sdl2
               #:livesupport #:classimp #:split-sequence
               #:dirt #:temporal-functions #:with-setf)
  :components ((:file "package")
               (:file "brine-shrimp-sim")))
