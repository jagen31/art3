#lang racket

(require "../core.rkt" (for-syntax syntax/parse racket/list))
(provide (all-defined-out))

;;;;;;;;;;; INTERVAL COORDINATE THINGS
(begin-for-syntax
  (define (merge-interval l r)
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
         (error 'oops "whoops")])))
  
  (define (interval-within? l r)
    (let/ec break
      (unless r (break #t))
      (unless l (break #f))
      (syntax-parse #`(#,l #,r)
        #:datum-literals [start end]
        [((_ (start s1*:number) (end e1*:number)) (_ (start s2*:number) (end e2*:number)))
         (define-values (s1 e1 s2 e2) (values (syntax-e #'s1*) (syntax-e #'e1*) (syntax-e #'s2*) (syntax-e #'e2*)))
         (and (>= s1 s2) (<= e1 e2))]))))

(define-coordinate (interval [start end] merge-interval interval-within?))
