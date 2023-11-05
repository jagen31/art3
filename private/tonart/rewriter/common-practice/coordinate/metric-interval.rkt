#lang racket

(require "../../../../common/core.rkt" "../../../../common/stdlib.rkt" "../../../../common/coordinate/interval.rkt" 
         "../../../realizer/electronic/lib.rkt"
         (for-syntax syntax/parse racket/match racket/list))
(provide (all-defined-out))

;;;;;;;;;;; METRIC INTERVAL THINGS
  (define-hom-merge-rule metric-interval 
    (λ (l r)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (syntax-parse #`(#,l #,r)
        ;; FIXME ....
        #:datum-literals [start end]
        [((_ (start ms1*:number bs1*:number) (end me1*:number be1*:number)) (_ (start ms2*:number bs2*:number) (end me2*:number be2*:number)))
  
         (define ms1 (syntax-e #'ms1*))
         (define bs1 (syntax-e #'bs1*))
         (define ms2 (syntax-parse #'ms2* [val:number (syntax-e #'val)] [_ #f]))
         (define bs2 (syntax-parse #'bs2* [val:number (syntax-e #'val)] [_ #f]))
         (define ms* (if ms1 (if ms2 (+ ms1 ms2) ms1) (or ms2 (break #f))))
  
         (define me2 (syntax-parse #'me2* [val:number (syntax-e #'val)] [_ #f]))
         (define be2 (syntax-parse #'be2* [val:number (syntax-e #'val)] [_ #f]))
         (define me1 (syntax-e #'me1*))
         (define be1 (syntax-e #'be1*))
         (when (and (not me1) (not me2)) (break #f))
         (define me* (and me2 (+ ms1 me2)))

         #;(unless (or (not e1) (not e*) (< e* e1)) (println "oops") #;(raise-syntax-error 'merger (format "end is outside of parent interval: ~s" e2) e2))
  
         (qq-art r (metric-interval (start #,ms* #,bs2) (end #,me* #,be2)))]
        [_ (error 'oops "whoops")]))))
  
  (define-hom-within?-rule metric-interval-within? 
    (λ (l r)
    (let/ec break
      (unless r (break #t))
      (unless l (break #f))
      (syntax-parse #`(#,l #,r)
        #:datum-literals [start end]
        [((_ (start ms1*:number bs1*:number) (end me1*:number be1*:number)) (_ (start ms2*:number bs2*:number) (end me2* be2*:number)))
         (define-values (ms1 bs1 me1 be1 ms2 bs2 me2 be2) 
           (values (syntax-e #'ms1*) (syntax-e #'bs1*) (syntax-e #'me1*) (syntax-e #'be1*) 
                   (syntax-e #'ms2*) (syntax-e #'bs2*) (syntax-e #'me2*) (syntax-e #'be2*)))
         (and (or (> ms1 ms2) (and (= ms1 ms2) (>= bs1 bs2)) (or (<= me1 me2) (and (= me1 me2) (<= be1 be2)))))]))))

(define-coordinate (metric-interval [start end]))