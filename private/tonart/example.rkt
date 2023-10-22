#lang racket

(require "../common/core.rkt" "../common/stdlib.rkt" "../common/interval.rkt" "music.rkt" "hymn-library.rkt"
  rsound (for-syntax syntax/parse))
(set-output-device! 1)

;; FIXME jagen make this a macro
(define-art-object (my-music []))

(define-rewriter my-music->objs
  (syntax-parser
    [_
     #:with (result ...) 
       (for/fold ([acc '()] #:result (reverse acc)) 
                 ([expr (current-ctxt)])
         (syntax-parse expr
           [({~datum my-music})
            (define st (syntax-parse (context-ref (get-id-ctxt expr) #'interval) [({~datum interval} ({~datum start} st*:number) _) (syntax-e #'st*)]))
            (cons
              (delete-expr this-syntax)
              (cons 
                (qq-art this-syntax
                    (@ [(interval (start #,st) (end #,(+ st 2)))]
                      (! 0) (! 1) (! 2)))
                acc))]
           [_ acc]))

     (qq-art this-syntax (@ () result ...))]))

#;(define test (perform music-rsound-performer 
  (i@ [0 4] (tone 440))
  (i@ [0 6] (repeat 2 (my-music))) ;; -> my-music => my-music
  (pocket-rewrite
    (i@ [0 12] (instrument Clarinet))
    (i@ [0 10] (midi 61))
    (translate 2))
  (my-music->objs) ;; my-music -> ! => !

  (-- 0 
    [2 (seq (note a 0 3) (note c 0 4) (tone 200))] ; -> [seq note] => !, seq]
    [2 (seq (note a 0 4) (note c 0 5) (tone 300))] ; -> [seq note] => !, seq
    [2 (seq (note f 0 4) (note d 0 5) (tone 555))]) ; -> [seq note] => !, seq
  (seq-ref) ; !@[seq A] -> A => note, seq

  (i@ [0 24] (st-flavian))
  (st-flavian->notes)

  (i@ [0 100] (instrument |Yamaha Grand Piano|))
  (pocket-rewrite
    (i@ [0 24] (stuttgart))
    (stuttgart->notes)
    (note->midi))

  (pocket-rewrite
    (i@ [6 32] (thaxted-a))
    (thaxted-a->notes)
    (note->midi))

  (i@ [0 100] (put (tuning 12tet))) ;; -> tuning => !, seq, tuning
  (note->tone) ; note@tuning -> tone => tone, seq, tuning
))

#;(perform music-pstream-performer 
  (m@ [2 32 (one)] (instrument Clarinet))
  (-- 2 [15 (repeat 6 (m@ [0 6 (one)] (rhythm 2 2 2)))] [15 (repeat 6 (m@ [0 6 (one)] (rhythm 1 1 1)))])
  (m@ [2 32 (one)] (expand-repeat))
  (m@ [2 32 (one)] (seq (midi 61) (midi 62) (midi 63)))
  (m@ [2 32 (one)] (apply-rhythm))
  )

#;(perform quote-performer 
  (i@ [0 8] 
    (repeat 2 (rhythm .25 .25 1 .5)) 
    (expand-repeat) 
    (seq (tone 440) (tone 550) (tone 660) (tone 550))
    (apply-rhythm)))

(define-simple-rewriter do-it expand-do-it (repeat 6 (i@ [0 2] (rhythm 2 2 2))))

#;(perform quote-performer
  (i@ [0 18] (do-it) (expand-do-it) (expand-repeat) (seq (note a 0 4) (note b 0 4) (note c 0 5)) (apply-rhythm))
  (i@ [0 18] (instrument |Yamaha Grand Piano|) (note->midi)))

(perform quote-performer
  (-- 0 [2 (! 0)] [2 (! 1)] [2 (! 2)])
  (i@ [0 8] (seq (tone 440) (tone 550) (tone 660)) (seq-ref)))

#;(play test)
#;(rs-write test "test.wav")
