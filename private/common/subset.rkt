#lang racket

(require "core.rkt" "stdlib.rkt" (for-syntax syntax/parse racket/list racket/set syntax/id-set))
(provide (all-defined-out))

;;;;;;;;;;; SUBSET COORDINATE THINGS
(begin-for-syntax
  (define (merge-subset l r)
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
         (println "I AM HERE")
         (error 'oops "whoops")])))
  
  (define (subset-within? l r)
    (let/ec break
      (unless r (break #t))
      (unless l (break #f))
      (syntax-parse #`(#,l #,r)
        [((_ item:id ...) (_ item*:id ...))
         (println #'(item ...))
         (println #'(item* ...))
         (free-id-subset? (immutable-free-id-set (syntax->list #'(item ...))) (immutable-free-id-set (syntax->list #'(item* ...))))]))))

(define-coordinate (subset [] merge-subset subset-within?))

(define-rewriter ss@
  (λ(stx)
    (syntax-parse stx
      [(_ [item ...] expr ...)
       (qq-art stx (@ [(subset item ...)] expr ...))])))