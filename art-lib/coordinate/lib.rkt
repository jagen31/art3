#lang racket

#;(define-coordinate-class interval)

#;(coordinate-family+ interval
  #:bindings
    [(format-id this-coordinate )]
  #:helper-functions
    []
  #:merge-rules 
    [(hom-merge-rule
       )]
  #:within?-rules
    [(hom-within?-rule
       )]
  #:rewriters
    [])

#;(begin-for-syntax
  (struct coordinate-family/s []))

#;(define-syntax (define-coordinate-family stx)
  (syntax-parse stx
    [(_ coord-name:id)
     (define-syntax coord-name (coordinate-family/s))]))

#;(define-syntax (coordinate-family+ stx)
  (syntax-parse stx
    [(_ coord-name:id )]))