#lang racket

(require "../core.rkt" (for-syntax syntax/parse racket/list))
(provide (all-defined-out))

;;;;;;;;;;; INSTANT COORDINATE THINGS
(define-hom-merge-rule instant 
  (λ (l r _ __ ___)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (error 'merge-instant "oops, cannot merge instant for now"))))
  
(define-hom-within?-rule within? (λ (l r) #t))

(define-coordinate (instant [type]))
