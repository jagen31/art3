#lang info

(define version "0.0.1")

(define collection "art")

(define deps
  '(["base" #:version "8.9"]
    ["collections-lib" #:version "1.3"]
    ["portaudio" #:version "0.1"]
    "rsound" "data-lib" "fmt" "htdp-lib"))
(define build-deps '("rackunit"))
