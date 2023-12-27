#lang racket

(require art/private/core art/coordinate/interval art/coordinate/instant art/coordinate/switch 
         art/private/lib art/sequence 2htdp/image 
         (for-syntax syntax/parse racket/list))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-interval-coordinate interval)

;;;;;;;;;; INTERVAL

(define-art-rewriter i@
  (λ(stx)
    (syntax-parse stx
      [(_ start*:number expr ...)
       (qq-art stx (i@ [start* +inf.0] expr ...))]
      [(_ [start* end*] expr ...)
       (qq-art stx (@ [(interval (start start*) (end end*))] expr ...))])))

(define-art-rewriter --
  (λ(stx)
    (syntax-parse stx
      [(_ start*:number {~and box [len:number expr ...]} ...)
       #:with (result ...)
         (for/fold ([acc '()] [t (syntax-e #'start*)] #:result (reverse acc))
                   ([box (syntax->list #'(box ...))] [l (syntax->list #'(len ...))] [e (syntax->list #'((expr ...) ...))])
           (values (cons #`(i@ [#,t #,(+ t (syntax-e l))] #,@e) acc) (+ t (syntax-e l))))
       (qq-art this-syntax (@ () result ...))]
      [(_ expr ...)
       (qq-art this-syntax (-- 0 expr ...))])))


;; repeats are reified since that's something you might want to realize directly (the alternative is
;; to have it be a rewrite.  In that case, a realizer will never encounter a repeat directly)
(define-art-object (loop []))

(define-mapping-rewriter (expand-loop [(: repeats loop)])
  (λ (stx repeat)
    (syntax-parse repeat
      [(_ size*:number expr ...)
       #:do [
        (define size (syntax-e #'size*))
        (define iv (context-ref (get-id-ctxt repeat) #'interval))
        (unless iv (raise-syntax-error 'expand-loop
          (format "repeat requires a beat interval, got: ~s" (syntax->datum (un-@ repeat))) repeat))
        (define-values (the-start the-end) (syntax-parse iv
          [({~literal interval} ({~datum start} s) ({~datum end} e)) (values (syntax-e #'s) (syntax-e #'e))]))
       ]
       #:with (result ...)
         (for/list ([i (in-range 0 (- the-end the-start) size)])
           #`[#,size expr ...])
       (qq-art this-syntax (-- 0 result ...))]
      [_ (error 'expand-loop "oops")])))


(define-art-rewriter translate
  (syntax-parser
    [(_ value:number)
     #:with (result ...) (for/foldr ([acc '()]) 
                ([expr (current-ctxt)])
       (define-values (the-start the-end) (syntax-parse (context-ref (get-id-ctxt expr) #'interval) [({~literal interval} ({~datum start} s) ({~datum end} e)) (values #'s #'e)]))
       (cons (delete-expr expr)
         (cons (qq-art expr 
           ;; FIXME jagen
           (@ [(interval (start value) (end +inf.0))] 
             (@ [#,@(get-id-ctxt expr)] (put #,expr)))) acc)))
     (qq-art this-syntax (@ () result ...))]))


;; rhythms combine with seqs to place objects in intervals.  This could probably be generalized to
;; applying a list of coordinates to a list of objects, but the rhythm syntax for intervals is so
;; nice & quick.
(define-art-object (rhythm []))

(define-art-rewriter uniform-rhythm
  (λ (stx)
    (syntax-parse (context-ref (get-id-ctxt stx) #'interval)
      [(_ (start s) (end e))
       (syntax-parse stx
        [(_ val:number)
         (define num (inexact->exact (floor (/ (- (syntax-e #'e) (syntax-e #'s)) (syntax-e #'val)))))
         (qq-art stx (rhythm #,@(build-list num (λ (_) #'val))))])])))

(define-for-syntax (do-apply-rhythm stx r exprs)

  (define seq (context-ref/surrounding (current-ctxt) (get-id-ctxt r) #'seq))
  (define items 
    (syntax-parse seq [(_ seq-expr ...) (syntax->list #'(seq-expr ...))]))

  (with-syntax ([(result ...) 
    (for/list ([e exprs] [i (in-naturals)])
      #`[#,e #,(quasisyntax/loc stx #,(remove-from-id-ctxt (list-ref items i) #'index))])])

    (qq-art r (context #,(delete-expr seq) (-- 0 result ...)))))

;; holes indicate spaces where objects should go.  They work well with rhythms.
(define-art-object (hole []))

(define-mapping-rewriter (fill-holes [(: h hole)])
  (λ (stx h)
    (syntax-parse stx
      [(_ head:id)
       (define its (require-context (current-ctxt) h #'head))
       (qq-art h (@ () #,@(map delete-expr its) #,(delete-expr h) #,@its))])))

(define-mapping-rewriter (rhythm->holes [(: r rhythm)])
  (λ (stx r)
    ;; cheeky implementation
    (syntax-parse r
      [(_ num:number ...)
       #:with r* r
       #:with (hole* ...) 
         (build-list (length (syntax->list #'(num ...))) (λ (_) #'(hole)))
       #:with hole-seq #'(seq (ix-- hole* ...))
       #:with app #'(apply-rhythm)
       (qq-art r (pocket-rewrite hole-seq r* app))])))

(define-art-object (divisions [n]))

(define-art-rewriter exact-subdivide
  (λ (stx)
    (syntax-parse stx
      [(_ division* ε*:number)
       (define division (syntax-e #'division*))
       (define ε (syntax-e #'ε*))
       (define exprs
         (flatten
           (for/list ([expr (current-ctxt)])
             (define-values (s e) 
               (syntax-parse (context-ref (get-id-ctxt expr) #'interval)
                 [(_ (_ s:number) (_ e:number))
                  (values (syntax-e #'s) (syntax-e #'e))]))
             (define s* (* s division))
             (define e* (* e division))
             (define (round+ensure-whole n)
               (define rounded (round n))
               (and (< (- rounded n) ε) (> (- rounded n) (- ε)) (inexact->exact rounded)))
              
             (define s** (or (round+ensure-whole s*) (raise-syntax-error 'exact-subdivide "score does not subdivide into the given number of divisions." expr)))
             (define e** (or (round+ensure-whole e*) (raise-syntax-error 'exact-subdivide "score does not subdivide into the given number of divisions." expr)))
             (list (delete-expr expr) (put-in-id-ctxt expr #'(interval (start #,s**) (end #,e**)))))))
       #`(@ () #,@(cons (qq-art #'division* (divisions division*)) exprs))])))

(define-art-rewriter apply-rhythm*
  (λ (stx)
    (syntax-parse stx
      [(apply-rhythm* expr:number ...)
       (do-apply-rhythm stx stx (syntax->list #'(expr ...)))])))

(define-mapping-rewriter (apply-rhythm [(: rhythms rhythm)])
  (λ (stx r)
    (syntax-parse r
      [(_ expr:number ...)
       (do-apply-rhythm stx r (syntax->list #'(expr ...)))])))

(realize (quote-realizer)
  (seq (ix-- (dumb) (dumber) (dumbest)))
  (rhythm 3 4 5)
  (apply-rhythm))

(define-art-rewriter unapply-rhythm
  (λ (stx)
    (syntax-parse stx
      [(_ name:id)
       #:do [(define exprs (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'name))
             (define sorted (sort exprs < #:key expr-interval-start))
             (define r
               (for/fold ([rs '()] [t (expr-interval-start stx)] #:result (reverse rs))
                         ([e exprs])
                 (define end* (expr-interval-end e))
                 (values (cons (- end* t) rs) end*)))]
      #:with (sorted* ...) 
        (for/list ([expr sorted]) (remove-from-id-ctxt expr #'interval))
      #:with (r* ...) r
      #`(@ () #,@(map delete-expr sorted) #,(qq-art stx (seq (ix-- sorted* ...))) #,(qq-art stx (rhythm r* ...)))])))


(define-art-rewriter coalesce-seq
  (λ (stx)
    (syntax-parse stx
      [_
       #:do [(define exprs (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'seq))
             (define sorted (sort exprs < #:key expr-interval-start))]
       #:with (result ...)        
         (flatten 
           (for/list ([e sorted])
             (syntax-parse e
               [({~literal seq} expr ...)
                (map (λ (e) (remove-from-id-ctxt e #'index)) (syntax->list #'(expr ...)))])))
      #`(@ () #,@(map delete-expr sorted) #,(qq-art stx (seq (ix-- result ...))))])))

;; FIXME jagen WHAT to do with this...
;;;;;;; INSTANT/SWITCH
;; interval -> instant + switch
(define-art-rewriter d/dt
  (λ (stx)
    (syntax-parse stx
      [_
       (define target
         (filter 
           (λ (expr) 
             (and (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (current-ctxt))))
           (current-ctxt)))
       (with-syntax ([(target* ...)
         (flatten
           (for/list ([item target])
             (define current-interval (context-ref (get-id-ctxt item) #'interval))
             (if current-interval
              (syntax-parse current-interval
                [(_ (_ s:number) (_ e:number))
                  (define new-item (remove-from-id-ctxt item #'interval))
                  (list (delete-expr item)
                        (put-in-id-ctxt (put-in-id-ctxt new-item #'(switch #t)) #'(instant s))
                        (put-in-id-ctxt (put-in-id-ctxt new-item #'(switch #f)) #'(instant e)))])
              item)))])
         #`(@ () target* ...))])))
