#lang racket

(require "../core.rkt" (for-syntax syntax/parse racket/list))
(provide (all-defined-out))

;;;;;;;;;;; SWITCH COORDINATE THINGS
(begin-for-syntax
  (define (merge-instant l r)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (error 'merge-instant "oops, cannot merge instant for now")))
  
  (define (instant-within? l r) #t))

(define-coordinate (instant [type] merge-instant instant-within?))
