#lang racket

(require "../../../common/core.rkt" "../../../common/stdlib.rkt" 
         "../../../common/coordinate/interval.rkt" "../../../common/coordinate/subset.rkt"
  (for-syntax syntax/parse racket/match racket/list) rsound rsound/envelope sf2-parser)
(provide (all-defined-out))

;;;;;;; TONES - these are pretty easy to have a computer perform.
(define-art-object (tone [freq]))


;; MIDI- an alternative to sine waves
(define-art-object (midi [num]))
(define-art-object (instrument-map [instruments]))
