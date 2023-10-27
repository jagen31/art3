#lang racket

(require "../../common/core.rkt" "../../common/stdlib.rkt" "../../common/coordinate/interval.rkt" "../../common/coordinate/subset.rkt" racket/runtime-path 
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




;;;;;;; TONES - these are pretty easy to have a computer perform.
(define-art-object (tone [freq]))
;; subperformer for performing tones from a context
(define (get-duration start end)
  (round (* (/ (- end start) 2) (default-sample-rate))))
(define-subperformer tone-subperformer
  (λ(ctxt)
    (for/foldr ([acc '()])
               ([stx ctxt])
      (syntax-parse stx
        [({~datum tone} freq) 

         (define-values (start* end*) (syntax-parse (context-ref (get-id-ctxt stx) #'interval) 
           [({~datum interval} ({~datum start} val:number) ({~datum end} val2:number)) (values (syntax-e #'val) (syntax-e #'val2))]))
         (cons #`(let ([duration (get-duration #,start* #,end*)]) 
             (cons (round (* #,start* (/ (default-sample-rate) 2)))
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

(define jeux
  (parse-soundfont
   (open-input-file
    (build-path soundfont-path "Jeux14.sf2"))))

#;(println (map preset-name (soundfont-presets jeux)))

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
              (cons (round (* #,start* (/ (default-sample-rate) 2)))
                    (preset-midi->rsound (load-preset jeux (symbol->string (syntax->datum #'name))) (syntax-e #'num) duration))) acc)])]
        [_ acc]))))


(define-composite-pstream-performer music-pstream-performer {tone-subperformer midi-subperformer})
(define-composite-rsound-performer music-rsound-performer {tone-subperformer midi-subperformer})
