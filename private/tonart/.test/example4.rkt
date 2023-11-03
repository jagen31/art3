#lang racket

(require "../../common/core.rkt" "../../common/stdlib.rkt" 
         "../../common/coordinate/interval.rkt" "../../common/coordinate/subset.rkt" 
         "../../common/coordinate/switch.rkt" "../../common/coordinate/instant.rkt" 
         "../rewriter/stdlib.rkt" "../rewriter/common-practice/lib.rkt" 
         "../realizer/electronic/lib.rkt" 
         "../realizer/electronic/linuxsampler/lib.rkt"
         "../realizer/electronic/rsound/lib.rkt"
  rsound (for-syntax syntax/parse))

(define-simple-rewriter do-it expand-do-it (repeat 6 (i@ [0 6] (rhythm 2 2 2))))

(define linuxsampler-string
  (perform linuxsampler-performer 

    (i@ [0 18] (instrument-map [strings . 069_Quintadena8Viola4]))

    (i@ [0 18] (do-it) (expand-do-it) (expand-repeat) (seq (note a 0 3) (note b 0 3) (note c 0 4)) (apply-rhythm))
    (i@ [0 18] (tempo 120) (instrument strings) (note->midi) (d/dt))))

(displayln linuxsampler-string)

#;(define result 
  (perform music-rsound-performer 

    ;; the keys
    (ss@ (accomp) (-- 0 [3 (key d 0 major)] [3 (key c 0 minor)] [3 (key g 0 major)]))

    ;; repeat this pattern
    (m@ [0 9 (accomp)] (repeat 3 (-- 0 [1 (^ 2)] [1 (^ 5)] [1 (^ 1)])))

    (m@ [0 9 (melody)] 
      (rhythm .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 1)
      (seq (note a 0 5) (note g 1 5) (note a 0 5) (note b 0 5) (note a 0 5) (note f 1 5)
           (note e 0 5) (note d 0 5) (note e 0 5) (note c 0 5) (note g 0 5) (note f 1 5)
           (note g 0 5) (note d 0 5) (note b 0 4) (note d 0 5) (note g 0 4))
      (apply-rhythm))
      
    (m@ [0 9 (melody)] (instrument |Yamaha Grand Piano|) (octave 4))
    (m@ [0 9 (accomp)] (instrument |Yamaha Grand Piano|) (octave 4))

    (i@ [0 9] (tempo 120) (expand-repeat) (^->note) (note->midi))))

#;(set-output-device! 1)
#;(play result)
