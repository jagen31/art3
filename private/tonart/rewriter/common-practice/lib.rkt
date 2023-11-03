#lang racket

(require "../../../common/core.rkt" "../../../common/stdlib.rkt" "../../../common/coordinate/interval.rkt" 
         "../../realizer/electronic/lib.rkt"
         (for-syntax syntax/parse racket/match racket/list "tonality.rkt"))
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
       (begin
       (define-values (exprs deletes)
         (for/fold ([acc1 '()] [acc2 '()] #:result (values (reverse acc1) (reverse acc2)))
                   ([expr (current-ctxt)])
           (syntax-parse expr
             [({~datum note} p a o)
              (define semis
                (match (syntax-e #'p)
                  ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11]))
              (with-syntax ([midi-stx (quasisyntax/loc expr (midi #,(+ 61 semis (syntax-e #'a) (* 12 (- (syntax-e #'o) 4)))))])
                (values (cons (qq-art expr (put midi-stx)) acc1) (cons (delete-expr expr) acc2)))]
             [_ (values acc1 acc2)])))
         (append deletes exprs))
     #'(@ () result ...)]))


(define-art-object (key [pitch accidental mode]))
(define-art-object (^ [degree]))
(define-art-object (octave [o]))

(define-rewriter ^->note
  (syntax-parser
    [_ 
     #:with (result ...)
       (begin
         (define-values (exprs deletes)
           (for/fold ([acc1 '()] [acc2 '()] #:result (values (reverse acc1) (reverse acc2)))
                     ([expr (current-ctxt)])
             (syntax-parse expr
               [({~datum ^} ix:number)
                (syntax-parse (context-ref/surrounding (current-ctxt) (get-id-ctxt expr) #'key)
                  [({~datum key} pitch:id accidental:number mode:id)
                   (define octave 
                     (syntax-parse (context-ref/surrounding (current-ctxt) (get-id-ctxt expr) #'octave)
                       [(octave o:number) (syntax-e #'o)]))

                   (define scale (generate-scale (syntax->datum #'pitch) (syntax->datum #'accidental) (syntax->datum #'mode)))
                   (define ix* (sub1 (syntax-e #'ix)))
                   (match-define (list p a) (list-ref scale (modulo ix* 7)))

                   (define c (index-where scale (λ (x) (eq? (car x) 'c))))
                   (define o (+ octave (floor (/ (- ix* c) 7))))

                   (values (cons (qq-art expr (put (note #,p #,a #,o))) acc1) (cons (delete-expr expr) acc2))])]
               [_ (values acc1 acc2)])))
         (append deletes exprs))
     #'(@ () result ...)]))

(define-art-object (transpose-diatonic []))
(define-mapping-rewriter (run-transpose-diatonic [(: transposes transpose-diatonic)])
  (syntax-parser
    [(_ val:number) 
     #:with (result ...)
       (begin
         (define-values (exprs deletes)
           (for/fold ([acc1 '()] [acc2 '()] #:result (values (reverse acc1) (reverse acc2)))
                     ([expr (current-ctxt)])
             (syntax-parse expr
               [({~datum ^} ix:number)
                (values (cons (qq-art expr (put (^ #,(add1 (modulo (sub1 (+ (syntax-e #'val) (syntax-e #'ix))) 8))))) acc1) (cons (delete-expr expr) acc2))]
               [_ (values acc1 acc2)])))
         (append deletes exprs))
     #'(@ () result ...)]))

(define-art-object (time-sig [n d]))
(define-art-object (dynamic [level]))
