#lang racket
(require rsound "../common/core.rkt" "../common/stdlib.rkt" "../common/interval.rkt" "music.rkt" 
  (for-syntax racket syntax/parse))

(define-for-syntax the-exprs (box '()))
(define-for-syntax the-post-exprs (box '()))
(define the-sound (box (silence 1)))
(define the-length (box 8))
(define the-device (box 1))

(thread (λ()
  (let loop ()
    (set-output-device! (unbox the-device))
    (play (unbox the-sound))
    (sleep (unbox the-length))
    (loop))))

(define-syntax (#%top-interaction stx)
  (syntax-parse stx
    [(_ . ({~literal set-length} n:number))
     #'(set-box! the-length n)]
    [(_ . ({~literal export-sound} name:string))
     #'(rs-write (unbox the-sound) name)]
    [(_ . ({~literal export-source} name:string))
     (define file (open-output-file (syntax-e #'name)))
     (for ([expr (unbox the-exprs)]) (writeln (syntax->datum expr) file))
     (for ([expr (unbox the-post-exprs)]) (writeln (syntax->datum expr) file))
     (close-output-port file)]
    [(_ . ({~literal set-audio-device} dev:number))
     #'(set-box! the-device dev)]
    [(_ . ({~literal add} instr ...))
     ;; add the instruction to the instructions
     (set-box! the-exprs (append (unbox the-exprs) (syntax->list #'(instr ...))))
     ;; perform the instructions and set to the sound
     #`(set-box! the-sound (perform music-rsound-performer #,@(unbox the-exprs) #,@(unbox the-post-exprs)))]
    [(_ . ({~literal post} instr ...))
     ;; add the instruction to the post instructions
     (set-box! the-post-exprs (append (unbox the-post-exprs) (syntax->list #'(instr ...))))
     ;; perform the instructions and set to the sound
     #`(set-box! the-sound (perform music-rsound-performer #,@(unbox the-exprs) #,@(unbox the-post-exprs)))]
    [(_ . ({~literal show}))
     #`(perform quote-performer #,@(unbox the-exprs) #,@(unbox the-post-exprs))]))
