#lang racket

(require "../../common/core.rkt" "../../common/stdlib.rkt" "../../common/coordinate/interval.rkt" 
         "../computer/lib.rkt"
         (for-syntax syntax/parse racket/match))
(provide (all-defined-out))

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


(define-art-object (key [pitch a* mode]))
(define-art-object (^ [degree]))
