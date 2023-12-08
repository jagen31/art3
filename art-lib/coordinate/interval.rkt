#lang racket

(require "../core.rkt" (for-syntax syntax/parse racket/list racket/syntax))
(provide (all-defined-out) (for-syntax (all-defined-out)))

;;;;;;;;;;; INTERVAL COORDINATE FAMILY

(define-syntax define-interval-coordinate
  (λ (stx)
    (syntax-parse stx
      [(_ interval:id)
       #:with [do-merge-interval do-interval-within? expr-start expr-end]
              (list (format-id #'interval "do-merge-~a" #'interval) (format-id #'interval "do-~a-within?" #'interval) 
                    (format-id #'interval "expr-~a-start" #'interval) (format-id #'interval "expr-~a-end" #'interval))

;; part of body of 'define-interval-coordinate'!!!! ^^^^^^
#'(begin
(define-for-syntax (do-merge-interval l r)
  (let/ec break
    (unless l (break r))
    (unless r (break l))
    (syntax-parse #`(#,l #,r)
      ;; FIXME ....
      #:datum-literals [start end]
      [((_ (start s1*:number) (end e1*:number)) (_ (start s2*:number) (end e2*:number)))

       (define s1 (syntax-e #'s1*))
       (define s2 (syntax-parse #'s2* [val:number (syntax-e #'val)] [_ #f]))
       (define s* (if s1 (if s2 (+ s1 s2) s1) (or s2 (break #f))))

       (define e2 (syntax-parse #'e2* [val:number (syntax-e #'val)] [_ #f]))
       (define e1 (syntax-e #'e1*))
       (when (and (not e1) (not e2)) (break #f))
       (define e* (and e2 (+ s1 e2)))

       #;(unless (or (not e1) (not e*) (< e* e1)) (println "oops") #;(raise-syntax-error 'merger (format "end is outside of parent interval: ~s" e2) e2))

       (qq-art r (interval (start #,s*) (end #,e*)))]
      [_ 
        (println l)
        (println r)
       (error 'oops "whoops")])))

(define-hom-merge-rule interval
  (λ (l r __ ___ ____) (do-merge-interval l r)))

(define-for-syntax (do-interval-within? l r)
  (let/ec break
    (unless r (break #t))
    (unless l (break #f))
    (syntax-parse #`(#,l #,r)
      #:datum-literals [start end]
      [((_ (start s1*:number) (end e1*:number)) (_ (start s2*:number) (end e2*:number)))
       (define-values (s1 e1 s2 e2) (values (syntax-e #'s1*) (syntax-e #'e1*) (syntax-e #'s2*) (syntax-e #'e2*)))
       (and (>= s1 s2) (<= e1 e2))])))

(define-hom-within?-rule interval
  (λ (l r _ __ ___) (do-interval-within? l r)))

(define-coordinate (interval [start end]))

(define-for-syntax (expr-start stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'interval) 
    [(_ (_ s) _) (syntax-e #'s)]
    [_ 0]))

(define-for-syntax (expr-end stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'interval) 
    [(_ _ (_ e)) (syntax-e #'e)]
    [_ +inf.0])))])))
