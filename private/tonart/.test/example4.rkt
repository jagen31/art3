#lang racket

(require "../../common/core.rkt" "../../common/stdlib.rkt" 
         "../../common/coordinate/interval.rkt" "../../common/coordinate/subset.rkt" 
         "../../common/coordinate/switch.rkt" "../../common/coordinate/instant.rkt" 
         "../rewriter/stdlib.rkt" "../rewriter/common-practice/lib.rkt" 
         "../realizer/electronic/lib.rkt" 
         "../realizer/electronic/linuxsampler/lib.rkt"
         "../realizer/visual/musicxml/lib.rkt"
         "../realizer/electronic/rsound/lib.rkt"
         "../rewriter/church/hymn.rkt"
  rsound (for-syntax syntax/parse))

(define-simple-rewriter do-it expand-do-it (repeat 6 (i@ [0 6] (rhythm 2 2 2))))

(define-simple-rewriter the-music expand-the-music 

    (i@ [0 18] (instrument-map [strings . 069_Quintadena8Viola4]))

    (i@ [0 18] (do-it) (expand-do-it) (expand-repeat) (seq (note a 0 3) (note b 0 3) (note c 0 4)) (apply-rhythm))
    )

#;(define linuxsampler-string
  (perform linuxsampler-performer 
    (i@ [0 18] (the-music))
    (expand-the-music)
    (i@ [0 18] (tempo 120) (instrument strings) (note->midi) (d/dt)) ; to midi events
    ))

(define musicxml-string 
  (perform musicxml-performer
    (i@ [0 18] (the-music))
    (expand-the-music) ; leave as notes
    ))

#;(displayln linuxsampler-string)
(displayln musicxml-string)

#;(define result 
  (perform music-rsound-performer 

    ;; the keys
    (ss@ (accomp) (-- 0 [3 (key d 0 major)] [3 (key c 0 minor)] [3 (key g 0 major)]))

    ;; repeat this pattern
    (musi@ [0 9 (accomp)] (repeat 3 (-- 0 [1 (^ 2)] [1 (^ 5)] [1 (^ 1)])))

    (musi@ [0 9 (melody)] 
      (rhythm .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 1)
      (seq (note a 0 5) (note g 1 5) (note a 0 5) (note b 0 5) (note a 0 5) (note f 1 5)
           (note e 0 5) (note d 0 5) (note e 0 5) (note c 0 5) (note g 0 5) (note f 1 5)
           (note g 0 5) (note d 0 5) (note b 0 4) (note d 0 5) (note g 0 4))
      (apply-rhythm))
      
    (musi@ [0 9 (melody)] (instrument |Yamaha Grand Piano|) (octave 4))
    (musi@ [0 9 (accomp)] (instrument |Yamaha Grand Piano|) (octave 4))

    (i@ [0 9] (tempo 120) (expand-repeat) (^->note) (note->midi))))


#;(define result (perform music-rsound-performer (i@ [0 96] (hyfrydol) (hyfrydol->notes) (note->midi) (tempo 120) (instrument |Yamaha Grand Piano|))))

#;(set-output-device! 2)
#;(play result)