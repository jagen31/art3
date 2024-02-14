#lang racket

(require art/private/lib art/private/draw art/private/core (for-syntax syntax/parse))
(provide (all-from-out art/private/lib art/private/draw art/private/core ))

(define-syntax dr
  (syntax-parser
    [(_ [w:number h:number] body ...) (syntax/loc this-syntax (realize (draw-realizer [w h]) body ...))]
    ;; FIXME jagen these are random defaults
    [(_ body ...) (syntax/loc this-syntax (realize (draw-realizer [800 100]) body ...))]))

(define-syntax qr (syntax-parser [(_ body ...) (syntax/loc this-syntax (realize (quote-realizer) body ...))]))

(provide dr qr)
