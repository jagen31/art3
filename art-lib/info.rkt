#lang info

(define version "0.0.1")

(define collection "art")

(define deps
  '(["base" #:version "8.9"]
    ["collections-lib" #:version "1.3"]
    "data-lib" "draw-lib" "fmt" "htdp-lib" "images-lib" "plot-gui-lib" "plot-lib"))
(define build-deps '("rackunit"))
