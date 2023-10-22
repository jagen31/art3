#lang racket

(require "../common/core.rkt" "../common/stdlib.rkt" "../common/interval.rkt" "music.rkt" (for-syntax syntax/parse))
(provide (all-defined-out))

(define-art-object (st-flavian []))

(define-mapping-rewriter (st-flavian->notes [(: melodies st-flavian)])
  (λ (melody)
    (qq-art melody
        (-- 0 [1 (note f 0 4)] 
          [1 (note f 0 4)]  [1 (note e 0 4)]  [1 (note f 0 4)] [1 (note a 0 4)]
          [1 (note g 0 4)]  [1 (note g 0 4)]  [1 (note f 0 4)] [1 (note f 0 4)]
          [1 (note b -1 4)] [1 (note a 0 4)]  [1 (note f 0 4)] [1 (note g 0 4)]
          [3 (note a 0 4)]                                     [1 (note a 0 4)]
          [1 (note a 0 4)]  [1 (note b -1 4)] [1 (note c 0 5)] [1 (note a 0 4)]
          [1 (note f 0 4)]  [1 (note g 0 4)]  [1 (note a 0 4)] [1 (note a 0 4)]
          [1 (note g 0 4)]  [1 (note f 0 4)]  [1 (note f 0 4)] [1 (note e 0 4)]
          [3 (note f 0 4)]))))

(define-art-object (stuttgart []))

(define-mapping-rewriter (stuttgart->notes [(: melody stuttgart)])
  (λ (melody)
    (qq-art melody
      (-- 0 
        [1 (note d 0 4)] [1 (note d 0 4)] [1 (note g 0 4)] [1 (note g 0 4)]
        [1 (note a 0 4)] [1 (note a 0 4)] [1 (note b 0 4)] [1 (note g 0 4)]
        [1 (note d 0 5)] [1 (note d 0 5)] [1 (note e 0 5)] [1 (note c 0 5)]
        [1 (note a 0 4)] [1 (note d 0 5)] [2 (note b 0 4)] 
        [1 (note b 0 4)] [1 (note b 0 4)] [1 (note a 0 4)] [1 (note b 0 4)]
        [1 (note g 0 4)] [1 (note a 0 4)] [1 (note g 0 4)] [1 (note f 1 4)]
        [1 (note g 0 4)] [1 (note e 0 4)] [1 (note d 0 4)] [1 (note g 0 4)]
        [1 (note g 0 4)] [1 (note f 1 4)] [2 (note g 0 4)]))))

(define-art-object (thaxted-a []))

(define-mapping-rewriter (thaxted-a->notes [(: melody thaxted-a)])
  (λ (melody)
    (qq-art melody
      (-- 0 [1/2 (note d 0 4)] [1/2 (note f 0 4)] 
            [3/2 (note g 0 4)] [1/2 (note b -1 4)] [3/4 (note a 0 4)] [1/4 (note f 0 4)] 
            [1/2 (note b -1 4)] [1/2 (note c 0 5)] [1 (note b -1 4)] [1 (note a 0 4)] 
            [1/2 (note g 0 4)] [1/2 (note a 0 4)] [1 (note g 0 4)] [1 (note f 0 4)] [2 (note d 0 4)] [1/2 (note d 0 4)] [1/2 (note f 0 4)] 
            [3/2 (note g 0 4)] [1/2 (note b -1 4)] [3/4 (note a 0 4)] [1/4 (note f 0 4)] 
            [1/2 (note b -1 4)] [1/2 (note c 0 5)] [1 (note d 0 5)] [1 (note d 0 5)] 
            [1/2 (note d 0 5)] [1/2 (note c 0 5)] [1 (note b -1 4)] [1 (note c 0 5)] [2 (note b -1 4)]))))
