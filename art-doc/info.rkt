#lang info

(define version "0.0.1")
(define collection "art")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "scribble-math"
                     "racket-doc"
                     "art-lib"))
(define scribblings '(("scribblings/art.scrbl" (multi-page) (language))))
(define clean '("compiled" "doc" "doc/art"))
