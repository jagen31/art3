#lang racket

(require "../../../../common/core.rkt" "../../../../common/stdlib.rkt" 
         "../../../../common/coordinate/interval.rkt" "../../../../common/coordinate/subset.rkt" 
         "../../../../common/coordinate/instant.rkt" "../../../../common/coordinate/switch.rkt" 
         "../../../rewriter/stdlib.rkt" "../lib.rkt" racket/runtime-path
  (for-syntax syntax/parse racket/match racket/list racket/string racket/dict) rsound rsound/envelope sf2-parser)
(provide (all-defined-out))

;; load fluid by default
(define-runtime-path soundfont-path "../resources/sf2")
(define fluid
  (parse-soundfont
   (open-input-file
    (build-path soundfont-path "FluidR3_GM.sf2"))))

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
                          (rs-overlay (rs-scale 0.05 silence+sound) #,acc)))))
         #`(rs-scale 4 #,result))))]))

;; subperformer for performing tones from a context
(define (get-duration start end tempo)
  (round (* (/ (- end start) (/ tempo 60)) (default-sample-rate))))
(define-subperformer tone-subperformer
  (λ(ctxt)
    (for/foldr ([acc '()])
               ([stx ctxt])
      (syntax-parse stx
        [({~datum tone} freq) 

         (define-values (start* end*) (syntax-parse (context-ref (get-id-ctxt stx) #'interval) 
           [({~datum interval} ({~datum start} val:number) ({~datum end} val2:number)) (values (syntax-e #'val) (syntax-e #'val2))]))
         ;; FIXME jagen THIS ASSUMES UNIFORM TEMPO
         (define tempo (context-ref/surrounding ctxt (get-id-ctxt stx) #'tempo))
         (unless tempo 
           (begin
             (define msg (format "no tempo in context for tone. tone: ~a. candidates: ~a" (un-@ stx) (map un-@ (context-ref* ctxt #'tempo))))
             (raise-syntax-error 'tone-subperformer msg stx)))

         (syntax-parse tempo
           [({~datum tempo} tempo*:number)
         (cons #`(let ([duration (get-duration #,start* #,end* tempo*)]) 
             (cons (round (* #,start* (/ (default-sample-rate) (/ tempo* 60))))
                   (rs-scale 2 (rs-mult (sine-window duration (floor (/ duration 4))) (make-tone freq 0.1 duration))))) acc)])]
        [_ acc]))))

(define-subperformer midi-subperformer
  (λ(ctxt)
    (for/foldr ([acc '()])
               ([stx ctxt])
      (syntax-parse stx
        [({~datum midi} num:number) 
         (define iv (context-ref (get-id-ctxt stx) #'interval))
         (unless iv (raise-syntax-error 'midi-subperformer 
           (format "this performer requires beat intervals for all midis, got: ~s" (syntax->datum (un-@ stx))) stx))
         (define-values (start* end*) (syntax-parse iv
           [({~datum interval} ({~datum start} val:number) ({~datum end} val2:number)) (values (syntax-e #'val) (syntax-e #'val2))]))
         (define instrument (context-ref/surrounding ctxt (get-id-ctxt stx) #'instrument))
         ;; FIXME jagen THIS ASSUMES UNIFORM TEMPO
         (define tempo (context-ref/surrounding ctxt (get-id-ctxt stx) #'tempo))
         (unless instrument (raise-syntax-error 'midi-subperformer "no instrument in context for midi" stx))
         (unless tempo (raise-syntax-error 'midi-subperformer "no tempo in context for midi" stx))
         (syntax-parse #`(#,instrument #,tempo)
           [(({~datum instrument} name:id) ({~datum tempo} tempo*:number))
            (cons #`(let ([duration (get-duration #,start* #,end* tempo*)]) 
              (cons (round (* #,start* (/ (default-sample-rate) (/ tempo* 60))))
                    (preset-midi->rsound (load-preset fluid(symbol->string (syntax->datum #'name))) (syntax-e #'num) duration))) acc)])]
        [_ acc]))))

(define-composite-pstream-performer music-pstream-performer {tone-subperformer midi-subperformer})
(define-composite-rsound-performer music-rsound-performer {tone-subperformer midi-subperformer})
