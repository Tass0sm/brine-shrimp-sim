;;;; package.lisp

(uiop:define-package #:brine-shrimp-sim
  (:mix #:cl #:cepl
        #:rtg-math #:vari #:livesupport
        #:cepl.skitter
        #:temporal-functions
        #:rtg-math #:with-setf
        #:qua)
  (:export #:main))
