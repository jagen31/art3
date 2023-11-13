#lang racket

(require "../../../../common/core.rkt" "../../../../common/stdlib.rkt" "../../../../common/coordinate/interval.rkt" 
         "../../../realizer/electronic/lib.rkt"
         (for-syntax syntax/parse racket/match racket/list))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(module+ test
  (require rackunit (for-syntax rackunit)))

;;;;;;;;;;; METRIC INTERVAL THINGS

(define-for-syntax (do-merge-metric-interval l r)
  (let/ec break
    (unless l (break r))
    (unless r (break l))
    (syntax-parse #`(#,l #,r)
      ;; FIXME ....
      #:datum-literals [start end]
      [((_ (start ms1*:number bs1*:number) (end me1*:number be1*:number)) (_ (start ms2*:number bs2*:number) (end me2*:number be2*:number)))

       (define (compute-offset m1 b1 m2 b2)
         (define m* (+ m1 m2 (- 2)))
         (define b* (+ b1 b2 (- 2)))
         (define m** (+ (floor (/ b* 4)) m*))
         (define b** (float-modulo b* 4))
         (values (add1 m**) (add1 b**)))

       (define ms1 (syntax-e #'ms1*))
       (define bs1 (syntax-e #'bs1*))
       (define ms2 (syntax-parse #'ms2* [val:number (syntax-e #'val)] [_ #f]))
       (define bs2 (syntax-parse #'bs2* [val:number (syntax-e #'val)] [_ #f]))

       (define me2 (syntax-parse #'me2* [val:number (syntax-e #'val)] [_ #f]))
       (define be2 (syntax-parse #'be2* [val:number (syntax-e #'val)] [_ #f]))
       (define me1 (syntax-e #'me1*))
       (define be1 (syntax-e #'be1*))

       (when (and (not me1) (not me2)) (break #f))

       (define-values (ms* bs*) (compute-offset ms1 bs1 ms2 bs2))
       (define-values (me* be*) (compute-offset ms1 bs1 me2 be2))

       #;(unless (or (not e1) (not e*) (< e* e1)) (println "oops") #;(raise-syntax-error 'merger (format "end is outside of parent interval: ~s" e2) e2))

       (qq-art r (metric-interval (start #,ms* #,bs*) (end #,me* #,be*)))]
      [_ 
        (println l)
        (println r)
        (error 'oops "whoops")])))

(module+ test
  (begin-for-syntax
    (check-equal?
      (syntax->datum (do-merge-metric-interval #'(metric-interval (start 1 3) (end 5 4)) #'(metric-interval (start 2 3) (end 3 2))))
      (syntax->datum #'(metric-interval (start 3 1) (end 3 4))))))

(define-hom-merge-rule metric-interval 
  (λ (l r _ __ ___) (do-merge-metric-interval l r)))

(define-for-syntax (do-metric-interval-within? l r)
  (let/ec break
    (unless r (break #t))
    (unless l (break #f))
    (syntax-parse #`(#,l #,r)
      #:datum-literals [start end]
      [((_ (start ms1*:number bs1*:number) (end me1*:number be1*:number)) (_ (start ms2*:number bs2*:number) (end me2* be2*:number)))
       (define-values (ms1 bs1 me1 be1 ms2 bs2 me2 be2) 
         (values (syntax-e #'ms1*) (syntax-e #'bs1*) (syntax-e #'me1*) (syntax-e #'be1*) 
                 (syntax-e #'ms2*) (syntax-e #'bs2*) (syntax-e #'me2*) (syntax-e #'be2*)))
       (and (or (> ms1 ms2) (and (= ms1 ms2) (>= bs1 bs2))) (or (< me1 me2) (and (= me1 me2) (<= be1 be2))))])))

(define-hom-within?-rule metric-interval
  (λ (l r _ __ ___) (do-metric-interval-within? l r)))
 

(define-coordinate (metric-interval [start end]))
