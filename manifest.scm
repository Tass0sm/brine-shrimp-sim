(use-modules (tassos-guix develop)
             (tassos-guix packages lisp-xyz)
             (gnu packages lisp-xyz)
             (guix gexp)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system asdf)
             ((guix licenses) #:prefix license:))

(define %source-dir
  (dirname (current-filename)))

(define brine-shrimp-simulator
  (package
   (name "brine-shrimp-simulator")
   (version "0.0.1")
   (source
    (local-file %source-dir
                #:recursive? #t
                #:select? (git-predicate %source-dir)))
   (build-system asdf-build-system/sbcl)
   (native-inputs
    (list sbcl-cepl.sdl2
          sbcl-real-rtg-math
          sbcl-dendrite sbcl-skitter sbcl-cepl.skitter.sdl2
          sbcl-livesupport sbcl-classimp sbcl-split-sequence
          sbcl-dirt sbcl-temporal-functions sbcl-with-setf
          sbcl-nineveh))
   (home-page "")
   (synopsis "")
   (description "")
   (license license:gpl3+)))

(de->manifest
 (development-environment
  (package brine-shrimp-simulator)))
