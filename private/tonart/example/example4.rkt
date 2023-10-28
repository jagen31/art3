#lang racket

(require "../../common/core.rkt" "../../common/stdlib.rkt" 
         "../../common/coordinate/interval.rkt" "../../common/coordinate/subset.rkt" 
         "../stdlib.rkt" "../common-practice/lib.rkt"
         "../computer/lib.rkt" "../organ/hymn.rkt"
  rsound (for-syntax syntax/parse))
(set-output-device! 1)

#;(perform quote-performer 
  (i@ [0 8] 
    (repeat 2 (rhythm .25 .25 1 .5)))

  (i@ [0 8] 
    (expand-repeat)
    (seq (tone 440) (tone 550) (tone 660) (tone 550))
    (apply-rhythm)))

(define-simple-rewriter do-it expand-do-it (repeat 6 (i@ [0 6] (rhythm 2 2 2))))

(perform #;quote-performer music-pstream-performer
  (i@ [0 18] (do-it) (expand-do-it) (expand-repeat) (seq (note a 0 3) (note b 0 3) (note c 0 4)) (apply-rhythm))
  (i@ [0 18] (instrument |G.o. reeds 8-4|) (note->midi)))

#;(perform quote-performer
  (-- 0 [2 (! 0)] [2 (! 1)] [2 (! 2)])
  (i@ [0 8] (seq (tone 440) (tone 550) (tone 660)) (seq-ref)))

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
      
    (m@ [0 9 (melody)] (instrument |Tromp. en chamade|) (octave 4))
    (m@ [0 9 (accomp)] (instrument |Montre 8 Flute 4|) (octave 4))

    (i@ [0 9] (expand-repeat) (^->note) (note->midi))))

#;(perform quote-performer 
  (m@ [0 4 (v1)] (i@ [2 4] (tone 440)))
  (m@ [0 4 (v1)] (copy-to (v2))))

#;(play result)
#;(rs-write result "test.wav")
