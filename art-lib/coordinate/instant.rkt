#lang racket

(require art/private/core (for-syntax syntax/parse racket/list))
(provide (all-defined-out))

;;;;;;;;;;; INSTANT COORDINATE THINGS
(define-hom-merge-rule instant 
  (λ (l r _ __ ___)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (error 'merge-instant "oops, cannot merge instant for now"))))
  
(define-hom-within?-rule instant (λ (l r _ __ ___)
  (syntax-parse #`(#,l #,r)
    [(({~datum instant} lix) ({~datum instant} rix))
     (= (syntax-e #'lix) (syntax-e #'rix))])))

(define-coordinate (instant [type]))

(define-for-syntax (expr-instant stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'instant)
    [(_ t) (syntax-e #'t)]
    [_ #f]))