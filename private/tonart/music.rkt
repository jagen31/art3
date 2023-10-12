#lang racket

(require "../common/core.rkt" "../common/stdlib.rkt" "../common/interval.rkt" racket/runtime-path 
  (for-syntax syntax/parse racket/match racket/list) rsound rsound/envelope sf2-parser)
(provide (all-defined-out))

;;;;;;;;;; PERFORMER FAMILIES- use other performers to stream/make a sound.
;; stream directly to pstream
(define-syntax (define-composite-pstream-performer stx)
  (syntax-parse stx
    [(_ n:id {subperformer:id ...})
     #'(begin
         (define-composite-performer n {subperformer ...} [(define pstream (make-pstream))] 
           (λ(clauses) 
              #`(let ()
                 #,@(for/list ([expr clauses])
                      #`(let ([expr* #,expr])
                          (pstream-queue pstream (rs-scale 0.25 (cdr expr*)) (car expr*))))))))]))

;; create an rsound
(define-syntax (define-composite-rsound-performer stx)
  (syntax-parse stx
    [(_ n:id {subperformer:id ...})
     #'(begin
         (define-composite-performer n {subperformer ...} [(define pstream (make-pstream))] 
           (λ(clauses) 
              (define result #`(let ()
                 #,(for/fold ([acc #'(silence 1)])
                              ([expr clauses])
                      #`(let* ([expr* #,expr]
                               [sound (rs-scale 1 (cdr expr*))]
                               [silence+sound (if (zero? (car expr*)) sound (rs-append (silence (car expr*)) sound))])
                          (rs-overlay (rs-scale 0.1 silence+sound) #,acc)))))
         #`(rs-scale 4 #,result))))]))

;;;;;; COMMON OBJECTS



;;;;;;;;;; notes!  considered fairly fundamental...
(define-art-object (note [pitch accidental octave]))
(define-art-object (tuning [type]))

;; convert notes in a context to tones. requires a tuning
(begin-for-syntax
  (define (semitone->freq value octave)
    (define freq (vector-ref #(261.626 277.183 293.665 311.127 329.628 349.228 369.994 391.995 415.305 440.000 466.164 493.883) value))
    (* freq (expt 2 (- octave 4)))))
(define-rewriter note->tone
  (syntax-parser
    [_ 
     #:with (result ...)
       (for/fold ([acc '()] #:result (reverse acc)) 
                 ([expr (current-ctxt)])
         (syntax-parse expr
           [({~datum note} p a o)
            (define semis
              (match (syntax-e #'p)
                ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11]))
            (syntax-parse (context-ref/surrounding (current-ctxt) (get-id-ctxt expr) #'tuning)
              [({~datum tuning} {~datum 12tet})
               (cons (delete-expr expr) (cons (ctxt->@ (get-id-ctxt expr) (qq-art expr 
                 (put (tone #,(semitone->freq (modulo (+ semis (syntax-e #'a)) 12) (syntax-e #'o)))))) acc))])]
           [_ acc]))
     (qq-art this-syntax (@ () result ...))]))

(define-rewriter note->midi
  (syntax-parser
    [_ 
     #:with (result ...)
       (for/fold ([acc '()] #:result (reverse acc)) 
                 ([expr (current-ctxt)])
         (syntax-parse expr
           [({~datum note} p a o)
            (define semis
              (match (syntax-e #'p)
                ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11]))
            (with-syntax ([midi-stx (quasisyntax/loc expr (midi #,(+ 61 semis (syntax-e #'a) (* 12 (- (syntax-e #'o) 4)))))])
              (cons (delete-expr expr) (cons (ctxt->@ (get-id-ctxt expr) #'(put midi-stx)) acc)))]
           [_ acc]))
     (qq-art this-syntax (@ () result ...))]))


;;;;;;; TONES - these are pretty easy to have a computer perform.
(define-art-object (tone [freq]))
;; subperformer for performing tones from a context
(define (get-duration start end)
  (* (/ (- end start) 2) (default-sample-rate)))
(define-subperformer tone-subperformer
  (λ(ctxt)
    (for/foldr ([acc '()])
               ([stx ctxt])
      (syntax-parse stx
        [({~datum tone} freq) 
         (define-values (start* end*) (syntax-parse (context-ref (get-id-ctxt stx) #'interval) 
           [({~datum interval} ({~datum start} val:number) ({~datum end} val2:number)) (values (syntax-e #'val) (syntax-e #'val2))]))
         (cons #`(let ([duration (get-duration #,start* #,end*)]) 
             (cons (* #,start* (/ (default-sample-rate) 2))
                   (rs-scale 2 (rs-mult (sine-window duration (floor (/ duration 4))) (make-tone freq 0.1 duration))))) acc)]
        [_ acc]))))

;; MIDI- an alternative to sine waves
(define-art-object (midi [num]))
(define-art-object (instrument [name]))

(define-runtime-path soundfont-path "soundfont")
(define fluid
  (parse-soundfont
   (open-input-file
    (build-path soundfont-path "FluidR3_GM.sf2"))))

(define-subperformer midi-subperformer
  (λ(ctxt)
    (for/foldr ([acc '()])
               ([stx ctxt])
      (syntax-parse stx
        [({~datum midi} num:number) 
         (define-values (start* end*) (syntax-parse (context-ref (get-id-ctxt stx) #'interval) 
           [({~datum interval} ({~datum start} val:number) ({~datum end} val2:number)) (values (syntax-e #'val) (syntax-e #'val2))]))
         (define instrument (context-ref/surrounding ctxt (get-id-ctxt stx) #'instrument))
         (unless instrument (raise-syntax-error 'midi-subperformer "no instrument in context for midi" stx))
         (syntax-parse instrument
           [({~datum instrument} name:id)
            (cons #`(let ([duration (get-duration #,start* #,end*)]) 
              (cons (* #,start* (/ (default-sample-rate) 2))
                    (preset-midi->rsound (load-preset fluid (symbol->string (syntax->datum #'name))) (syntax-e #'num) duration))) acc)])]
        [_ acc]))))

(define-composite-pstream-performer music-pstream-performer {tone-subperformer midi-subperformer})
(define-composite-rsound-performer music-rsound-performer {tone-subperformer midi-subperformer})