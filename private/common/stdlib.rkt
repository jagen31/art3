#lang racket

(require "core.rkt" syntax-spec 
         "coordinate/instant.rkt" "coordinate/interval.rkt" "coordinate/subset.rkt" "coordinate/switch.rkt"
         (for-syntax syntax/parse racket/list (except-in ee-lib racket-var)))
(provide (all-defined-out) (for-syntax (all-defined-out)))

;;;;;;;;;;;; SOME GENERIC OBJECTS that may come in handy.

;;;;;;; LISTS of ITEMS in a context.
(define-art-object (seq [items]))
(define-art-object (! [ix]))

;; index into the `seq` (convert `!`s to their corresponding objects)
(define-rewriter seq-ref
  (syntax-parser
    [_ 
     #:with (result ...)
     (begin
       (define-values (exprs deletes)
         (for/fold ([acc1 '()] [acc2 '()] #:result (values (reverse acc1) (reverse acc2))) 
                   ([expr (current-ctxt)])
           (syntax-parse expr
             [({~datum !} value:number)
               (define items (context-ref/surrounding (current-ctxt) (get-id-ctxt expr) #'seq))
               (unless items 
                 (define msg (format "no list items in context for ref. context: ~a. candidates: ~a" (un-@ expr) (map un-@ (context-ref* (current-ctxt) #'seq))))
                 (raise-syntax-error 'seq-ref msg expr))
               (syntax-parse items
                 [(_ the-items ...) 
                  (values (cons (qq-art expr (put #,(list-ref (syntax->list #'(the-items ...)) (syntax-e #'value)))) acc1)
                          (cons (delete-expr expr) acc2))])]
             [_ (values acc1 acc2)])))
        (append deletes exprs))

     #'(@ () result ...)]))

(define-performer quote-performer 
  (λ(stx)
    (syntax-parse stx
      [(_ exprs ...)
       #`'(#,@(for/list ([e (syntax->list #'(exprs ...))]) (un-@ e)))])))

(begin-for-syntax 
  (struct art-subperformer/s [body]))

;; easy way to decompose a performer into parts.  The values produced by the subperformers will all be
;; appended together and handed to `combiner`.
(syntax-spec

  (binding-class art-subperformer)

  (host-interface/definitions (define-subperformer name:art-subperformer body:expr)
    #:binding (export name)
    #'(define-syntax name (art-subperformer/s body)))

  (host-interface/definitions (define-composite-performer name:art-performer 
    {sub-name:art-subperformer ...} [init-statement:expr ...] combiner:expr)
    #:binding (export name)
    #'(define-syntax name
        ;; phase 1 code
        (λ (stx) 
          (syntax-parse stx
            [(_ exprs (... ...))
             (define clauses 
               (flatten
                 (for/list ([subn (syntax->list #'(sub-name ...))])
                   (define subperf (lookup subn))
                   ((art-subperformer/s-body subperf) (syntax->list #'(exprs (... ...)))))))
             #`(let () init-statement ... #,(combiner clauses))])))))

;; a classier rewriter that uses "type-clauses" to pattern match against the context and deliver /
;; delete the right expressions automatically
(syntax-spec
  
  (nonterminal type-clause
    (: name:id obj:art-object)
    (@@ [obj:art-object] t:type-clause))

    
  ;; FIXME jagen use the binding stuff
  (host-interface/definitions (define-standard-rewriter (name:rewriter [clause:type-clause ...]) body:expr)
    #:binding (export name)
    #:with ([head-name binding-clause] ...)
      (for/list ([clause (syntax->list #'(clause ...))])
        (syntax-parse clause
          [({~literal :} binding:id head:id)
           #'[binding
              (filter (λ (expr) 
                (syntax-parse expr
                  [(head*:id _ (... ...))
                   (free-identifier=? #'head* #'head)]))
                (current-ctxt))]]))
    #'(define-syntax name
        (rewriter/s 
          (λ (stx)
            (define head-name binding-clause) ...
            (body head-name ...))))))
          
(define-syntax (define-mapping-rewriter stx)
  (syntax-parse stx 
    [(_ (name:id [clause ...]) body)
    #'(define-standard-rewriter (name [clause ...]) 
        (λ (melodies)
          (with-syntax ([(result (... ...))
            (append
              (for/list ([melody melodies]) (delete-expr melody))
              (for/list ([melody melodies]) 
                (parameterize ([current-ctxt (filter (λ(expr) (context-within? (get-id-ctxt expr) (get-id-ctxt melody))) (current-ctxt))])
                  (body melody))))])
            #'(@ () result (... ...)))))]))

(define-syntax (define-simple-rewriter stx)
  (syntax-parse stx 
    [(_ name:id rewriter-name:id body ...)
    #'(begin
        (define-art-object (name []))
        (define-mapping-rewriter (rewriter-name [(: _ name)]) 
        (λ (obj)
          (syntax-parse obj
            [_
             (qq-art obj (@ () body ...))]))))]))

;;;;;;;;;;;;;;;;;;

;; delete by name
(define-rewriter delete
  (λ (stx)
    (syntax-parse stx
      [(_ name:id)
       (define target
         (filter 
           (λ (expr) 
             (and (context-within? (get-id-ctxt expr) (get-id-ctxt stx))
                  (syntax-parse expr
                    [(head:id _ ...) (free-identifier=? (compiled-from #'head) #'name)])))
           (current-ctxt)))
       (with-syntax ([(target* ...)
         (for/list ([item target])
           ;; FIXME jagen preserve orthogonality?
           (delete-expr item))])
         #`(@ () target* ...))])))


;;;;;;;;; SUBSET 
(define-rewriter ss@
  (λ(stx)
    (syntax-parse stx
      [(_ [item ...] expr ...)
       (qq-art stx (@ [(subset item ...)] expr ...))])))

(define-rewriter copy-to
  (λ (stx)
    (syntax-parse stx
      [(_ (ss* ...))
       (define coords (syntax->list #'(ss* ...)))
       (define target
         (filter 
           (λ(expr) (context-within? (get-id-ctxt expr) (get-id-ctxt stx)))
           (current-ctxt)))
       (with-syntax ([(target* ...)
         (for/list ([item target])
           ;; FIXME jagen preserve orthogonality?
           #`(put #,(put-in-id-ctxt item #'subset #'(ss* ...))))])
         #`(@ () target* ...))])))







;;;;;;;;;; INTERVAL

(define-rewriter i@
  (λ(stx)
    (syntax-parse stx
      [(_ [start* end*] expr ...)
       (qq-art stx (@ [(interval (start start*) (end end*))] expr ...))])))

(define-rewriter --
  (λ(stx)
    (syntax-parse stx
      [(_ start* {~and box [len:number expr ...]} ...)
       #:with (result ...)
         (for/fold ([acc '()] [t (syntax-e #'start*)] #:result (reverse acc))
                   ([box (syntax->list #'(box ...))] [l (syntax->list #'(len ...))] [e (syntax->list #'((expr ...) ...))])
           (values (cons #`(i@ [#,t #,(+ t (syntax-e l))] #,@e) acc) (+ t (syntax-e l))))
       (qq-art this-syntax (@ () result ...))])))


;; repeats are reified since that's something you might want to realize directly (the alternative is
;; to have it be a rewrite.  In that case, a realizer will never encounter a repeat directly)
(define-art-object (repeat []))

(define-mapping-rewriter (expand-repeat [(: repeats repeat)])
  (λ (repeat)
    (syntax-parse repeat
      [(_ size*:number expr ...)
       #:do [
        (define size (syntax-e #'size*))
        (define-values (the-start the-end) (syntax-parse (context-ref (get-id-ctxt repeat) #'interval) 
          [({~datum interval} ({~datum start} s) ({~datum end} e)) (values (syntax-e #'s) (syntax-e #'e))]))
       ]
       #:with (result ...)
         (for/list ([i (in-range 0 (- the-end the-start) size)])
           #`[#,size expr ...])
       (qq-art this-syntax (-- 0 result ...))]
      [_ (error 'expand-repeat "oops")])))
  

(define-rewriter translate
  (syntax-parser
    [(_ value:number)
     #:with (result ...) (for/foldr ([acc '()]) 
                ([expr (current-ctxt)])
       (define-values (the-start the-end) (syntax-parse (context-ref (get-id-ctxt expr) #'interval) [({~datum interval} ({~datum start} s) ({~datum end} e)) (values #'s #'e)]))
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

(define-mapping-rewriter (apply-rhythm [(: rhythms rhythm)])
  (λ (r)
    (syntax-parse r
      [(_ expr:number ...)
       ;; FIXME copy id ctxt
       #:with (result ...)
         (for/list ([e (syntax->list #'(expr ...))] [i (in-naturals)])
           #`[#,e (! #,i)])

       (qq-art this-syntax
          (@ ()
            (-- 0 result ...)
            ;; FIXME jagen TOTALLY UNSAFE (this will seq-ref in the surrounding context :'( )
            (seq-ref)))])))

(define-art-object (divisions [n]))

(define-rewriter exact-subdivide
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
             (list (delete-expr expr) (put-in-id-ctxt expr #'interval #`((start #,s**) (end #,e**)))))))
       #`(@ () #,@(cons (qq-art #'division* (divisions division*)) exprs))])))

;;;;;;; INSTANT/SWITCH
;; interval -> instant + switch
(define-rewriter d/dt
  (λ (stx)
    (syntax-parse stx
      [_
       (define target
         (filter 
           (λ (expr) 
             (and (context-within? (get-id-ctxt expr) (get-id-ctxt stx))
                  (context-ref (get-id-ctxt expr) #'interval)))
           (current-ctxt)))
       (with-syntax ([(target* ...)
         (flatten
           (for/list ([item target])
             (syntax-parse (context-ref (get-id-ctxt item) #'interval)
               [(_ (_ s:number) (_ e:number))
                (define new-item (remove-from-id-ctxt item #'interval))
                (list (delete-expr item)
                      (put-in-id-ctxt (put-in-id-ctxt new-item #'switch #'(#t)) #'instant #'(s))
                      (put-in-id-ctxt (put-in-id-ctxt new-item #'switch #'(#f)) #'instant #'(e)))])))])
         #`(@ () target* ...))])))
