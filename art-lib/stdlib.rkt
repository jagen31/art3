#lang racket

(require "core.rkt" syntax-spec racket/class
         "coordinate/instant.rkt" "coordinate/index.rkt" "coordinate/interval.rkt" "coordinate/subset.rkt" "coordinate/switch.rkt"
         2htdp/image
         (for-syntax syntax/parse racket/list racket/match (except-in ee-lib racket-var) syntax/id-table syntax/id-set))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art-object (number [n]))

(define-for-syntax (number-value stx)
  (syntax-parse stx
    [(_ val:number) (syntax-e #'val)]))

(begin-for-syntax (struct drawer/s [body]))

(define-syntax number-drawer 
  (drawer/s
    (λ (e width height)
      (syntax-parse e
        [({~literal number} n:number)
         #`(overlay (text (~s n) 24 'blue) (rectangle #,width #,height 'solid 'transparent))]))))

;; a classier rewriter that uses "type-clauses" to pattern match against the context and deliver /
;; delete the right expressions automatically
;; FIXME jagen use the binding stuff
(define-syntax (define-standard-rewriter stx)
  (syntax-parse stx
    [(_ (name:id [clause ...]) body:expr)
     #:with ([head-name binding-clause] ...)
       (for/list ([clause (syntax->list #'(clause ...))])
         (syntax-parse clause
           [({~literal :} binding:id head:id)
            #'[binding
               (filter (λ (expr) 
                 (syntax-parse expr
                   [(head*:id _ (... ...))
                    (and (free-identifier=? #'head* #'head) (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (current-ctxt)))]))
                 (current-ctxt))]]))
     #'(define-syntax name
         (rewriter/s 
           (λ (stx)
             (define head-name binding-clause) ...
             #`(@ () #,(body stx head-name ...)))))]))
          
(define-for-syntax mapping-rewriter-index (make-parameter #f))

(define-syntax (define-mapping-rewriter stx)
  (syntax-parse stx 
    [(_ (name:id [clause ...]) body)
    #'(define-standard-rewriter (name [clause ...]) 
        (λ (stx melodies)
          (with-syntax ([(result (... ...))
            (append
              (for/list ([melody melodies]) (delete-expr melody))
              (for/list ([melody melodies] [i (in-naturals)]) (parameterize ([mapping-rewriter-index i]) (body stx melody))))])
            #`(@ () result (... ...)))))]))

(define-syntax (define-simple-rewriter stx)
  (syntax-parse stx 
    [(_ name:id rewriter-name:id body ...)
    #'(begin
        (define-art-object (name []))
        (define-mapping-rewriter (rewriter-name [(: _ name)]) 
        (λ (stx obj)
          (syntax-parse obj
            [_
             (qq-art obj (@ () body ...))]))))]))


;;;;;;;;;;;; SOME GENERIC OBJECTS that may come in handy.


(define-art-rewriter &&&
  (λ (stx)
    (syntax-parse stx
      [(_ {~and left-expr (left-rw:id _ ...)} {~and right-expr (right-rw:id _ ...)})
       
       (define rw1 (lookup #'left-rw rewriter/s?))
       (define rw2 (lookup #'right-rw rewriter/s?))

       (define set1 ((rewriter/s-body rw1) (qq-art stx left-expr)))
       (define set2 ((rewriter/s-body rw2) (qq-art stx right-expr)))

       #`(@ () #,@(compile-rewrite-exprs (list set1 set2) (current-ctxt)))])))

(define-mapping-rewriter (rewrite-in-seq [(: s seq)])
  (λ (stx s)
    (syntax-parse stx
      [(_ exprs ...)
       (syntax-parse s
         [(_ exprs* ...)
           #:with (result ...) 
             (compile-rewrite-exprs 
               (syntax->list #'(exprs ...)) (syntax->list #'(exprs* ...)))
           #`(@ () #,(delete-expr s) #,(qq-art s (seq result ...)))])])))

(define-art-realizer quote-performer (λ(stx) #`'(#,@(for/list ([e (current-ctxt)]) (un-@ e)))))

(begin-for-syntax 
  (struct art-subperformer/s [body]))

(define-syntax (define-composite-performer stx)
  (syntax-parse stx
    [(_ name {sub-name ...} [init-statement ...] combiner)
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
             #`(let () init-statement ... #,(combiner clauses))])))]))

;;;;;;;;;;;;;;;;;;

;; delete by name
(define-art-rewriter delete
  (λ (stx)
    (syntax-parse stx
      [(_ name:id ...)
       (define names (immutable-free-id-set (syntax->list #'(name ...))))
       (define target
         (filter 
           (λ (expr) 
             (and (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (current-ctxt))
                  (syntax-parse expr
                    [(head:id _ ...) (free-id-set-member? names #'head)])))
           (current-ctxt)))
       (with-syntax ([(target* ...)
         (for/list ([item target])
           ;; FIXME jagen preserve orthogonality?
           (delete-expr item))])
         #`(@ () target* ...))])))


;;;;;;;;; SUBSET 
(define-art-rewriter ss@
  (λ(stx)
    (syntax-parse stx
      [(_ [item ...] expr ...)
       (qq-art stx (@ [(subset item ...)] expr ...))])))

(define-art-rewriter copy-to
  (λ (stx)
    (syntax-parse stx
      [(_ (ss* ...))
       (define coords (syntax->list #'(ss* ...)))
       (define target
         (filter 
           (λ(expr) (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (current-ctxt)))
           (current-ctxt)))
       (with-syntax ([(target* ...)
         (for/list ([item target])
           ;; FIXME jagen preserve orthogonality?
           #`(put #,(put-in-id-ctxt item #'(subset ss* ...))))])
         #`(@ () target* ...))])))

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

#;(define-for-syntax (do-apply-rhythm stx r exprs)

  (define seq (context-ref/surrounding (current-ctxt) (get-id-ctxt r) #'seq))
  (define items 
    (syntax-parse seq [(_ seq-expr ...) (syntax->list #'(seq-expr ...))]))

  (with-syntax ([(result ...) 
    (for/list ([e exprs] [i (in-naturals)])
      #`[#,e #,(quasisyntax/loc stx #,(remove-from-id-ctxt (list-ref items i) #'index))])])

    (qq-art r (@ () #,(delete-expr seq) (-- 0 result ...)))))

#;(define-art-rewriter apply-rhythm*
  (λ (stx)
    (syntax-parse stx
      [(apply-rhythm* expr:number ...)
       (do-apply-rhythm stx stx (syntax->list #'(expr ...)))])))

#;(define-mapping-rewriter (apply-rhythm [(: rhythms rhythm)])
  (λ (stx r)
    (syntax-parse r
      [(_ expr:number ...)
       (do-apply-rhythm stx r (syntax->list #'(expr ...)))])))

#;(define-art-rewriter unapply-rhythm
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
      #:with (sorted* ...) sorted
      #:with (r* ...) r
      #`(@ () #,@(map delete-expr sorted) #,(qq-art stx (seq (ix-- sorted* ...))) #,(qq-art stx (rhythm r* ...)))])))

;; holes indicate spaces where objects should go.  They work well with rhythms.
(define-art-object (hole []))

(define-mapping-rewriter (fill-holes [(: h hole)])
  (λ (stx h)
    (syntax-parse stx
      [(_ head:id)
       #:do [(define its (context-ref*/surrounding (current-ctxt) (get-id-ctxt h) #'head))
             (when (null? its) (raise-syntax-error 'fill-holes (format "could not fill hole: ~s.  No ~s exists.  candidates: ~s" (un-@ h) #'head (map un-@ (context-ref* (current-ctxt) #'head)))))]
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

(begin-for-syntax
  (struct interp/s object/s [parent body]))

(define-syntax (define-interpretation stx)
  (syntax-parse stx
    [(_ name)
     #'(define-syntax name 42)]))

(define-syntax (interpretation+ stx)
  (syntax-parse stx
    [(_ name [iname* body* ...] ...)
     #'(begin
         (define-syntax iname* (interp/s #'name #'(@ () body* ...))) ...)]))

(define-art-rewriter interpret
  (λ (stx)
    (syntax-parse stx
      [(_ interp*:id)
       #:with (result ...) 
         (for/fold ([acc '()] #:result (reverse acc)) 
                   ([expr (filter (λ (expr) (context-within? (get-id-ctxt expr) (or (get-id-ctxt stx) '()) (current-ctxt))) (current-ctxt))])
           (syntax-parse expr
             [(head:id arg ...)
              #:do [(define interp-struct (lookup #'head interp/s?))]
              #:when (and interp-struct (free-identifier=? (interp/s-parent interp-struct) #'interp*))
              #:do [(define new-expr (interp/s-body interp-struct))] 
              (cons (qq-art expr #,new-expr) (cons (delete-expr expr) acc))]
            [_ acc]))
       #`(@ () result ...)])))

(define-for-syntax (float-modulo n m)
  (- n (* (floor (/ n m)) m)))

(define-interval-coordinate interval)

(define-art-object (dumb []))
(define-art-object (dumber []))
(define-art-object (dumbest []))

(define-mapping-rewriter (dumb->dumber [(: d dumb)])
  (λ (stx d) #`(@ () #,(delete-expr d) #,(qq-art d (dumber)))))

(define-mapping-rewriter (dumber->dumbest [(: d dumber)])
  (λ (stx d) #`(@ () #,(delete-expr d) #,(qq-art d (dumbest)))))
