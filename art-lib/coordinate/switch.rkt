#lang racket

(require "../core.rkt" (for-syntax syntax/parse racket/list))
(provide (all-defined-out))

;;;;;;;;;;; SWITCH COORDINATE THINGS
(define-hom-merge-rule switch 
  (λ (l r _ __ ___)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (error 'merge-switch "oops, cannot merge switches"))))
  
(define-hom-within?-rule switch (λ (l r _ __ ___) #t))

(define-coordinate (switch [type]))