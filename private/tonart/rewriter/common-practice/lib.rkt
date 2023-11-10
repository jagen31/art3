#lang racket

(require "../../../common/core.rkt" "../../../common/stdlib.rkt" "../../../common/coordinate/interval.rkt" 
         "../../realizer/electronic/lib.rkt"
         "coordinate/metric-interval.rkt"
         (for-syntax syntax/parse racket/match racket/list "tonality.rkt"))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(module+ test (require rackunit (for-syntax rackunit)))

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
            (define tuning (context-ref/surrounding (current-ctxt) (get-id-ctxt expr) #'tuning))
            (unless tuning (raise-syntax-error 'note->tone "no tuning in context for note" expr))
            (syntax-parse tuning
              [({~datum tuning} {~datum 12tet})
               (with-syntax ([tone-stx (quasisyntax/loc expr (tone #,(semitone->freq (modulo (+ semis (syntax-e #'a)) 12) (syntax-e #'o))))])
                 (values (cons (qq-art expr (put tone-stx)) (cons (delete-expr expr) acc))))])]
           [_ acc]))
     #'(@ () result ...)]))

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

(define-art-object (pitch [p a]))

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
  (λ (stx expr)
  (syntax-parse expr
    [(_ val:number) 
     #:with (result ...)
       (begin
         (define-values (exprs deletes)
           (for/fold ([acc1 '()] [acc2 '()] #:result (values (reverse acc1) (reverse acc2)))
                     ;; FIXME jagen
                     ([expr (filter (λ (e) (context-within? (get-id-ctxt e) (get-id-ctxt expr))) (current-ctxt))])
             (syntax-parse expr
               [({~datum ^} ix:number)
                (values (cons (qq-art expr (put (^ #,(+ (syntax-e #'val) (syntax-e #'ix))))) acc1) (cons (delete-expr expr) acc2))]
               [_ (values acc1 acc2)])))
         (append deletes exprs))
     #'(@ () result ...)])))

(define-art-object (time-sig [n d]))
(define-art-object (dynamic [level]))

(define-rewriter mi@
  (λ(stx)
    (syntax-parse stx
      [(_ [(mstart* bstart*)] expr ...)
       ;; FIXME jagen fix this!!
       (qq-art stx (mi@ [(mstart* bstart*) (+inf.0 4)] expr ...))]
      [(_ [(mstart* bstart*) (mend* bend*)] expr ...)
       (qq-art stx (@ [(metric-interval (start mstart* bstart*) (end mend* bend*))] expr ...))])))

(define-rewriter measure@
  (λ(stx)
    (syntax-parse stx
      [(_ [start:number end:number] expr ...) (qq-art stx (mi@ [(start 1) (end 4)] expr ...))])))

(define-rewriter music@
  (λ(stx)
    (syntax-parse stx
      [(_ [(measure:number beat:number) (voice:id ...)] expr ...) 
       (qq-art stx (music@ [(measure beat) (+inf.0 4) (voice ...)] expr ...))]
      [(_ [(mstart:number bstart:number) (mend:number bend:number) (voice:id ...)] expr ...)
       (qq-art stx (mi@ [(mstart bstart) (mend bend)] (ss@ (voice ...) expr ...)))])))

(define-for-syntax (do-metric-interval->interval stx ctxt)
  (syntax-parse stx 
    [(_ (_ ms*:number bs*:number) (_ me*:number be*:number))
     (qq-art stx
       (interval 
         (start #,(+ (* 4 (sub1 (syntax-e #'ms*))) (sub1 (syntax-e #'bs*))))
         (end #,(+ (* 4 (sub1 (syntax-e #'me*))) (sub1 (syntax-e #'be*))))))]
    [_ #f]))

(define-rewriter metric-interval->interval
  (λ (stx)
    (syntax-parse stx
      [_
       (define exprs
         (flatten
           (for/list ([expr (current-ctxt)])
             (define the-minterval (context-ref (get-id-ctxt expr) #'metric-interval))
             (if the-minterval
               (list (delete-expr expr) 
                     (put-in-id-ctxt 
                       (remove-from-id-ctxt expr #'metric-interval) 
                       (do-metric-interval->interval the-minterval (current-ctxt))))
               '()))))
       #`(@ () #,@exprs)])))

(define-for-syntax (do-interval->metric-interval stx ctxt)
  (syntax-parse stx 
    [(_ (_ start*:number) (_ end*:number))
     (qq-art stx
       (metric-interval 
         (start #,(add1 (floor (/ (syntax-e #'start*) 4))) #,(add1 (remainder (syntax-e #'start*) 4)))
         (end #,(add1 (floor (/ (syntax-e #'end*) 4))) #,(add1 (remainder (syntax-e #'end*) 4)))))]
    [_ #f]))

(module+ test
  (begin-for-syntax
  (check-equal? 
    (syntax->datum 
      (do-interval->metric-interval #'(interval (start 3) (end 20))
        (list (set-id-ctxt #'(time-sig 4 4) (list #'(interval (start 0) (end 100)))))))
    '(metric-interval (start 1 4) (end 6 1)))))

(define-rewriter interval->metric-interval
  (λ (stx)
    (syntax-parse stx
      [_
       (define exprs
         (flatten
           (for/list ([expr (current-ctxt)])
             (define the-interval (context-ref (get-id-ctxt expr) #'interval))
             (if the-interval
               (list (delete-expr expr) 
                     (put-in-id-ctxt 
                       (remove-from-id-ctxt expr #'interval) 
                       (do-interval->metric-interval the-interval (current-ctxt))))
               '()))))
       #`(@ () #,@exprs)])))

(module+ test
  (begin-for-syntax
  (check-equal? 
    (syntax->datum 
      (do-metric-interval->interval #'(metric-interval (start 1 4) (end 6 1))
        (list (set-id-ctxt #'(time-sig 4 4) (list #'(interval (start 0) (end 100)))))))
    '(interval (start 3) (end 20)))))

(define-nonhom-merge-rule metric-interval interval #:keep-right
  (λ (l r l* _ ctxt)
    (do-merge-interval (do-metric-interval->interval l ctxt) r)))

(define-nonhom-merge-rule interval metric-interval #:keep-right
  (λ (l r l* _ ctxt)
    (do-merge-metric-interval (do-interval->metric-interval l ctxt) r)))

(define-art-object (chord [pitch accidental mode]))

(define-art-object (relative-harmony [chords]))

(define-for-syntax (odd-even-list li) 
  (define (odd-even-list li lacc racc)
    (cond 
      [(null? li) (values (reverse lacc) (reverse racc))]
      [(null? (cdr li)) (values (cons (car li) (reverse lacc)) (reverse racc))]
      [else (odd-even-list (cddr li) (cons (car li) lacc) (cons (cadr li) racc))]))
  (odd-even-list li '() '()))

(define-mapping-rewriter (relative-harmony->chord-seq [(: harm relative-harmony)])
  (λ (stx harm)
    (syntax-parse harm
      [(_ harmony ...)
       (define start-pitch (context-ref/surrounding (current-ctxt) (get-id-ctxt harm) #'pitch))
       (unless start-pitch (raise-syntax-error 'relative-harmony->chord-seq "no pitch in context for relative harmony" harm)) 
       (syntax-parse start-pitch
         [(_ p*:id a*:number)
          #:do
            [(define pitch (map syntax-e (list #'p* #'a*))) 
             (define-values (chord-types transitions) (odd-even-list (syntax->datum #'(harmony ...))))]
          #:with (chords ...)
            (for/fold ([chords '()] [pitch pitch] #:result (reverse chords)) 
                      ([chord-type chord-types] [transition (cons '(P 1) transitions)])
              (define new-pitch (transpose-by-interval (first pitch) (second pitch) (second transition) (first transition)))
              (values (cons #`(chord #,(first new-pitch) #,(second new-pitch) #,chord-type) chords) new-pitch))
          (qq-art harm (seq chords ...))])])))
