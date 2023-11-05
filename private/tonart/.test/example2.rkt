#lang racket

(require "../../common/core.rkt" "../../common/stdlib.rkt" 
         "../../common/coordinate/interval.rkt" "../../common/coordinate/subset.rkt" 
         "../rewriter/stdlib.rkt" "../rewriter/common-practice/lib.rkt" "../realizer/electronic/rsound/lib.rkt" "../rewriter/church/hymn.rkt"
  rsound (for-syntax syntax/parse))


;; this is the actual composition
(define-simple-rewriter the-music expand-the-music
  (ss@ (melody) 
    (-- 0 
      [16 (repeat 8 (rhythm .5 .5 .5 .5 1 1 .5 .5 .5 .5 2))] 
      ;; variety is the spice of life
      [16 (repeat 8 (rhythm 1 1 .5 .5 .5 .5 .25 .25 1 0.5 2))]))
  (musi@ [0 32 (bass)] 
    ;; ... but not in the bass
    (repeat 8 (rhythm 2 2 2 2))))


;; these are example sequences i have in mind. the sequences gotta have the same
;; number of items as the rhythm has (11 for melody, 4 for bass).  but this could be ignored,
;; if you decide that sequences are cycles and fill in the missing notes that way. 
(define-simple-rewriter cool-melody-seq expand-cool-melody-seq
  (seq (note a 0 3) (note c 0 4) (note d 0 4) (note f 0 4) (note g 0 4)
   (note f 0 4) (note e 0 4) (note f 0 4) (note e 0 4) (note c 0 4) (note d 0 4)))

(define-simple-rewriter punchy-4note-seq expand-punchy-4note-seq
  (seq (note a 0 2) (note b 0 2) (note e 0 3) (note d 0 2)))


;; just a logical organization
(define-simple-rewriter the-notes expand-the-notes
  ;; the notes to use
  (musi@ [0 32 (melody)] (cool-melody-seq))
  (musi@ [0 32 (bass)] (punchy-4note-seq)))

(define sound
  (perform 

  ;; capable of performing (@ [instrument] midi), tone
  ;; proof in the comments
  music-rsound-performer

  (i@ [0 32] (the-music) (the-notes))

  ;; we'll render these as midi, which requires an instrument specified.
  ;; the instruments to use:
  (musi@ [0 32 (melody)] (instrument |Clarinet|))
  (musi@ [0 32 (bass)] (instrument |Violin|))

  ;; render things
  (i@ [0 32] 
    ;; current context type: the-music, the-notes, instrument
    (expand-the-music) ; the-music -> (repeat rhythm)
    (expand-the-notes) ; the-notes -> cool-melody-seq, punchy-4note-seq
    ;; => (repeat rhythm), cool-melody-seq, punchy-4note-seq, instrument
    (expand-cool-melody-seq) ; cool-melody-seq -> (seq note)
    (expand-punchy-4note-seq) ; punchy-4note-seq -> (seq note)
    ;; => (repeat rhythm), (seq note), instrument
    (expand-repeat) ; (repeat A) -> A
    ;; => rhythm, (seq note), instrument
    (apply-rhythm) ; (@ [(seq A)] rhythm) -> A 
    ;; => note, (seq note), instrument
    (note->midi) ; note -> midi
    ;; => midi, (seq note), instrument
    ;; performer will ignore the seq. midi is present w/ instrument and will be performed QED
    (tempo 120)
    )))

(set-output-device! 1)
(play sound)
(rs-write sound "sound.wav")
