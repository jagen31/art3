#lang info

(define version "0.0.1")

(define collection "art")

(define deps
  '("draw-lib"
    "images-lib"
    "plot-gui-lib"
    "plot-lib"
    ["base" #:version "8.9"]
    ["collections-lib" #:version "1.3"]
    "data-lib" "fmt" "htdp-lib"))
(define build-deps '("rackunit"))
