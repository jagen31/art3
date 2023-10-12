#lang racket

(require "../common/core.rkt" "../common/stdlib.rkt" "../common/interval.rkt" "music.rkt" 
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
                      (put (! 0) (! 1) (! 2))))
                acc))]
           [_ acc]))

     (qq-art this-syntax (@ () result ...))]))

#;(define test (perform quote-performer 
  (i@ [0 4] (put (tone 440)))
  (i@ [0 6] (repeat 2 (put (my-music)))) ;; -> my-music => my-music
  (pocket-rewrite
    (i@ [0 12] (put (instrument Violin)))
    (i@ [0 10] (put (midi 61)))
    (translate 2))
  (my-music->objs) ;; my-music -> ! => !

  (-- 0 
    [2 (put (list-items (note a 0 3) (note c 0 4) (tone 200)))] ; -> [list-items note] => !, list-items]
    [2 (put (list-items (note a 0 4) (note c 0 5) (tone 300)))] ; -> [list-items note] => !, list-items
    [2 (put (list-items (note f 0 4) (note d 0 5) (tone 555)))]) ; -> [list-items note] => !, list-items
  (list-item-ref) ; !@[list-items A] -> A => note, list-items

  (i@ [0 100] (put (tuning 12tet))) ;; -> tuning => !, list-items, tuning
  (note->tone) ; note@tuning -> tone => tone, list-items, tuning
))

(perform music-pstream-performer (i@ [0 10] (put (instrument Clarinet))) (-- 0 [2 (put (midi 61))] [2 (put (midi 62))] [2 (put (midi 63))]))

(sleep 10)
