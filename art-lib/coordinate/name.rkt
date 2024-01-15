#lang racket

(require art/private/core (for-syntax syntax/parse racket/list racket/syntax racket/string))
(provide (all-defined-out) (for-syntax (all-defined-out)))

;;;;;;;;;;; NAME COORDINATE CLASS

(define-syntax define-name-coordinate
  (λ (stx)
    (syntax-parse stx
      [(_ name:id)
       #:with [do-merge-name do-name-within? expr-single-name expr-name context-ref/name]
              (list (format-id #'name "do-merge-~a" #'name) (format-id #'name"do-~a-within?" #'name) 
                    (format-id #'name "expr-single-~a" #'name) (format-id #'name "expr-~a" #'name)
                    (format-id #'name "context-ref/~a" #'name))

#'(begin
;;;;;;;;;;; NAME COORDINATE THINGS
(define-hom-merge-rule name
  (λ (l r _ __ ___)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (syntax-parse #`(#,l #,r)
        [((_ namel (... ...)) (__ namer (... ...)))
         (qq-art r (name namel (... ...) namer (... ...)))]))))
  
(define-hom-within?-rule name (λ (l r _ __ ___)
  (syntax-parse #`(#,l #,r)
    [(({~datum name} lname (... ...)) ({~datum name} rname (... ...)))
     ;; FIXME jagen use identifiers?
     (list-prefix? (syntax->datum #'(rname (... ...))) (syntax->datum #'(lname (... ...))) )])))

(define-coordinate (name [sym]))

(define-for-syntax (expr-single-name stx)
  (define names (expr-name stx))
  (format-id #f (string-join (map (compose symbol->string syntax->datum) names) ".")))

(define-for-syntax (expr-name stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'name) 
    [(_ n:id (... ...)) (syntax->list #'(n (... ...)))]
    [_ '()]))

(define-for-syntax (context-ref/name ctxt name) 
  (car (filter 
         (λ (expr) (equal? name (map syntax->datum (cdr (syntax->list (context-ref (get-id-ctxt expr) #'name))))))
         ctxt))))])))
