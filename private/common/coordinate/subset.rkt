#lang racket

(require "../core.rkt" (for-syntax syntax/parse racket/list racket/set syntax/id-set))
(provide (all-defined-out))

;;;;;;;;;;; SUBSET COORDINATE THINGS
(define-hom-merge-rule subset 
  (λ(l r)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (syntax-parse #`(#,l #,r)
        ;; FIXME ....
        [((_ item:id ...) (_ item*:id ...))
         #:with (result ...) 
           (free-id-set->list (immutable-free-id-set (append (syntax->list #'(item ...)) (syntax->list #'(item* ...)))))
         (qq-art l (subset result ...))]
        [_ 
         (error 'oops "whoops")]))))
  
(define-hom-within?-rule subset
  (λ (l r)
    (let/ec break
      (unless r (break #t))
      (unless l (break #f))
      (syntax-parse #`(#,l #,r)
        [((_ item:id ...) (_ item*:id ...))
         (free-id-subset? (immutable-free-id-set (syntax->list #'(item* ...))) (immutable-free-id-set (syntax->list #'(item ...))))]))))

(define-coordinate (subset []))
