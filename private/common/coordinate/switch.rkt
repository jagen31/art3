#lang racket

(require "../core.rkt" (for-syntax syntax/parse racket/list))
(provide (all-defined-out))

;;;;;;;;;;; SWITCH COORDINATE THINGS
(begin-for-syntax
  (define (merge-switch l r)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (error 'merge-switch "oops, cannot merge switches")))
  
  (define (switch-within? l r) #t))

(define-coordinate (switch [type] merge-switch switch-within?))