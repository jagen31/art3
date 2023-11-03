#lang racket

(require "../../common/core.rkt" "../../common/stdlib.rkt" 
         "../../common/coordinate/interval.rkt" "../../common/coordinate/subset.rkt" 
         (for-syntax syntax/parse))
(provide (all-defined-out))

;; define-coordinate interval (copy-coordinate std:interval) or something
;; define-coordinate voice (copy-coordinate std:subset) or something

(define-art-object (tempo [bpm]))
(define-art-object (instrument [name]))

(define-rewriter m@
  (λ (stx)
    (syntax-parse stx
      [(_ [start*:number end*:number (voice:id ...)] expr ...)
       (qq-art stx (@ [(interval (start start*) (end end*)) (subset voice ...)] expr ...))])))
