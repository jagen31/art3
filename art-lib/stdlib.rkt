#lang racket

(require "core.rkt" syntax-spec racket/class
         "coordinate/instant.rkt" "coordinate/index.rkt" "coordinate/interval.rkt" "coordinate/subset.rkt" "coordinate/switch.rkt"
         2htdp/image
         (for-syntax syntax/parse racket/list racket/match (except-in ee-lib racket-var) syntax/id-table))
(provide (all-defined-out) (for-syntax (all-defined-out)))

;; a classier rewriter that uses "type-clauses" to pattern match against the context and deliver /
;; delete the right expressions automatically
(syntax-spec
  
  (nonterminal type-clause
    (: name:id obj:art-object)
    (@@ [obj:art-object] t:type-clause))

    
  ;; FIXME jagen use the binding stuff
  (host-interface/definitions (define-standard-rewriter (name:art-object [clause:type-clause ...]) body:expr)
    #:binding (export name)
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
            #`(@ () #,(body stx head-name ...)))))))
          
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

(define-art-rewriter ix@
  (λ (stx)
    (syntax-parse stx
      [(_ n:number expr ...) (qq-art stx (ix@ [n] expr ...))]
      [(_ (n:number ...) expr ...) 
       (qq-art stx (@ [(index n ...)] expr ...))])))

(define-art-rewriter ix--
  (λ(stx)
    (syntax-parse stx
      [(_ start*:number expr ...)
       #:with (result ...)
         (for/fold ([acc '()] [n (syntax-e #'start*)] #:result (reverse acc))
                   ([e (syntax->list #'(expr ...))])
           (values (cons #`(ix@ #,n #,e) acc) (add1 n)))
       (qq-art this-syntax (@ () result ...))]
      [(_ expr ...)
       (qq-art this-syntax (ix-- 0 expr ...))])))


;; repeats are reified since that's something you might want to realize directly (the alternative is
;; to have it be a rewrite.  In that case, a realizer will never encounter a repeat directly)
(define-art-object (ix-loop []))

(define-for-syntax (do-expand-ix-loop stx s e exprs)
  (with-syntax ([((expr ...) ...) (for/list ([i (in-range s e)]) exprs)])
    (qq-art stx (ix-- #,s (@ () expr ...) ...))))

(define-mapping-rewriter (expand-ix-loop [(: repeats ix-loop)])
  (λ (stx repeat)
    (syntax-parse repeat
      [(_ [s:number e:number] expr ...)
       (do-expand-ix-loop repeat (syntax-e #'s) (syntax-e #'e) (syntax->list #'(expr ...)))]
      [(_ n:number expr ...) (do-expand-ix-loop repeat 0 (syntax-e #'n) (syntax->list #'(expr ...)))]
      [_ (error 'expand-loop "oops")])))

;;;;;;; LISTS of ITEMS in a context.
(define-art-embedding (seq [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (compile-rewrite-exprs (list (qq-art/no-context stx (@ () expr ...))) '())])))
(define-art-object (! [ix]))



;; index into the `seq` (convert `!`s to their corresponding objects)
(define-art-rewriter seq-ref
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
                (define msg (format "no seq in context for ref. ref: ~a. candidates: ~a" (un-@ expr) (map un-@ (context-ref* (current-ctxt) #'seq))))
                (raise-syntax-error 'seq-ref msg expr))
              (syntax-parse items
                [(_ the-items ...) 
                 #:with the-item (or (findf (λ (expr) 
                   (syntax-parse (or (context-ref (get-id-ctxt expr) #'index) #'(index -1))
                     [(_ ix*:number) (= (syntax-e #'ix*) (syntax-e #'value))]))
                   (syntax->list #'(the-items ...)))
                  (raise-syntax-error 'seq-ref (format "No item with given index in sequence: ~s" (un-@ items)) expr))
                 (values (cons (qq-art expr (put the-item)) acc1)
                         (cons (delete-expr expr) acc2))])]
             [_ (values acc1 acc2)])))
        (append deletes exprs))

     #'(@ () result ...)]))

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
               [({~datum seq} expr ...)
                (map (λ (e) (remove-from-id-ctxt e #'index)) (syntax->list #'(expr ...)))])))
      #`(@ () #,@(map delete-expr sorted) #,(qq-art stx (seq (ix-- result ...))))])))

(define-art-rewriter split
  (λ (stx)
    (define ctxt (filter (λ (expr) (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (current-ctxt))) (current-ctxt)))
    (syntax-parse stx
      [(_ subctxt:id)
       #:do [
         (define new-exprs
           (for/fold ([acc (hash)]) 
                     ([expr ctxt])
             (match (expr-indices expr)
               [(list ixs ... last)
                (hash-update acc ixs (λ (cur) (cons (put-in-id-ctxt expr #`(index #,last)) cur)) (λ () '()))])))
         (define-values (result deletes)
           (for/fold ([acc '()] [deletes '()])
                     ([(new-ix inner-exprs) (in-hash new-exprs)])
             (values (cons (qq-art/no-context stx
               (ix@ [#,@new-ix] (subctxt #,@(reverse inner-exprs)))) acc)
               (append (map delete-expr inner-exprs) deletes))))]
       #:with (result* ...) (reverse result)
       #:with (deletes* ...) (reverse deletes)
       #'(@ () deletes* ... result* ...)])))

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
               (syntax->list (compile-art-references #'(exprs ...))) (syntax->list #'(exprs* ...)))
           #`(@ () #,(delete-expr s) #,(qq-art s (seq result ...)))])])))

(define-art-realizer quote-performer (λ(stx) #`'(#,@(for/list ([e (current-ctxt)]) (un-@ e)))))

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

;;;;;;;;;;;;;;;;;;

;; delete by name
(define-art-rewriter delete
  (λ (stx)
    (syntax-parse stx
      [(_ name:id)
       (define target
         (filter 
           (λ (expr) 
             (and (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (current-ctxt))
                  (syntax-parse expr
                    [(head:id _ ...) (free-identifier=? (compiled-from #'head) (decompile-reference #'name))])))
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
          [({~datum interval} ({~datum start} s) ({~datum end} e)) (values (syntax-e #'s) (syntax-e #'e))]))
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

    (qq-art r (@ () #,(delete-expr seq) (-- 0 result ...)))))

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
      #:with (sorted* ...) sorted
      #:with (r* ...) r
      #`(@ () #,@(map delete-expr sorted) #,(qq-art stx (seq (ix-- sorted* ...))) #,(qq-art stx (rhythm r* ...)))])))

;; holes indicate spaces where objects should go.  They work well with rhythms.
(define-art-object (hole []))

(define-mapping-rewriter (fill-holes [(: h hole)])
  (λ (stx h)
    (syntax-parse stx
      [(_ head:id)
       #:do [(define its (context-ref*/surrounding (current-ctxt) (get-id-ctxt h) (decompile-reference #'head)))
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

(syntax-spec
  (binding-class interp-binding)
  
  (host-interface/definitions (define-interpretation name:interp-binding)
    #:binding (export name)
    #'(define-syntax name 42))

  (host-interface/definitions (interpretation+ name:interp-binding [iname*:art-object body*:expr ...] ...)
     #:binding (export iname*)
     #'(begin
         (define-syntax iname* (interp/s #'name #'(@ () body* ...))) ...)))

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
              #:when (and interp-struct (free-identifier=? (interp/s-parent interp-struct) (compile-reference2 #'interp*)))
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

#;(perform (quote-performer) (put (seq 42)) (rewrite-in-seq (delete !)))
#;(perform (quote-performer) (put (seq (i@ [0 8] (loop 2 (dummy))) (expand-loop)) (! 0)) (seq-ref))

#;(perform (quote-performer)
  (seq (ix-- (dumb) (dumber))) (! 0) (! 1)
  (seq-ref)    
  (-- [2 (hole)] [2 (hole)])
  (fill-holes dumb))

#;(perform (quote-performer)
  (@ [(instant 1)] (seq (ix-- (dumb))))
  (@ [(instant 1)] (! 0))
  (seq-ref))

(perform (quote-performer)
  (-- [4 (dumb)] [2 (dumb)] [3 (dumb)])
  (unapply-rhythm dumb))

#;(perform (quote-performer)
  (seq (ix-- (dumb) (dumber) (dumbest)))
  (rhythm 3 4 5)
  (apply-rhythm))

#;(perform (quote-performer)
  (ix-- (seq (ix-- (dumb) (dumber))) (seq (ix-- (dumbest))))
  (coalesce-seq))

#;(perform (quote-performer)
  (ix-- (ix-- (dumb) (dumber) (dumbest)) (ix-- (dumbest) (dumber) (dumb)))
  (split seq))

#;(perform (quote-performer)
  (ix@ 1 (ix@ 2 (put (seq (ix@ [2] (seq (ix-- (ix-- (dumb) (dumber) (dumbest))))))))))

#;(perform (quote-performer)
  (debug-perform (quote-performer)))

#;(define-interpretation test-interp)

#;(interpretation+ test-interp
  [foo (dumb)])

#;(perform quote-performer
  (foo)
  (interpret test-interp))
  
(define-for-syntax (do-draw-seq ctxt width height drawers)
  (define max-ix
    (for/fold ([acc #f])
              ([expr ctxt])
      (define ix (expr-indices expr))
      (if acc (max-index acc ix) ix)))
  (when (> (length max-ix) 2) (raise-syntax-error 'draw-seq "cannot draw indexes greater than 2 yet" #f))

  (define-values (each-width each-height)
    (cond 
      [(= (length max-ix) 2)
       (values (floor (/ width (add1 (cadr max-ix)))) (floor (/ height (add1 (car max-ix)))))]
      [(= (length max-ix) 1)
       (values (floor (/ width (add1 (car max-ix)))) (floor height))]
      [(= (length max-ix) 0)
      (values (floor width) (floor height))]))

  (define get-expr-index 
    (cond 
      [(= (length max-ix) 2) expr-indices]
      [(= (length max-ix) 1)
       (λ (e) (list 0 (expr-index e)))]
      [(= (length max-ix) 0)
       (λ (e) (list 0 0))]))

  (define result
    (for/fold ([acc (hash)]) 
              ([e ctxt])
       (hash-set acc (get-expr-index e)
         (syntax-parse e
           [({~datum seq} expr ...)
            (define sub-result (do-draw-seq (syntax->list #'(expr ...)) (- each-width 20) (- each-height 20) drawers))
            ;; draw a box for nested sequences, apl style
            #`(overlay (rectangle #,each-width #,each-height 'outline 'blue) #,sub-result)]
           [_ (or (for/or ([drawer drawers]) (drawer e (- each-width 20) (- each-height 20))) (raise-syntax-error 'draw-seq "no drawer for expression." e)) 
             ]))))

  (define result2 
    (for/fold ([im #'empty-image])
              ([(ix expr) (in-hash result)])
      #`(overlay/xy #,im (* #,each-width #,(cadr ix)) (* #,each-height #,(car ix)) #,expr)))
  result2)

(begin-for-syntax (struct drawer/s [body]))

(define-syntax number-drawer 
  (drawer/s
    (λ (e width height)
      (syntax-parse e
        [({~datum number} n:number)
         #`(overlay (text (~s n) 24 'blue) (rectangle #,width #,height 'solid 'transparent))]))))

#;(define-for-syntax (do-draw ctxt width height recur))

#;(define-art-realizer draw-performer
  (λ (stx)
    (syntax-parse stx
      [(_ [width:number height:number] { [object . subperf] ... })
       (for/list ([(k v) (in-dict (syntax->datum #'((object . subperf) ...)))])
         )
       (do-draw (current-ctxt) (syntax-e #'width) (syntax-e #'height))])))

(define-art-realizer draw-seq-performer
  (λ (stx)
    (syntax-parse stx
      [(_ [width:number height:number] [drawers ...])
       (do-draw-seq (current-ctxt) (syntax-e #'width) (syntax-e #'height)
         (map (compose drawer/s-body lookup) (syntax->list #'(drawers ...))))])))

(begin-for-syntax
  (struct apl-value/s [thunk])
  (struct apl-monad/s [body])
  (struct apl-dyad/s [body]))

(define-syntax *ctxt* (apl-value/s (λ () (current-ctxt))))

(define-syntax each
  (apl-monad/s
    (λ (stx f)
      (apl-monad/s
        (λ (stx w)
          
          (define new-exprs
            (map float-scalars
              (flatten
                (for/list ([expr ((apl-value/s-thunk w))])
                  (syntax-parse expr
                    [({~datum seq} inner-expr ...)
                     (define val
                       ((apl-monad/s-body f) #f (apl-value/s (λ () (syntax->list #'(inner-expr ...))))))
                     (qq-art expr (seq #,@((apl-value/s-thunk val))))])))))
          (apl-value/s (λ () new-exprs)))))))

(define-syntax mix
  (apl-monad/s
    (λ (stx w)
      
      (define result
        (for/list ([expr ((apl-value/s-thunk w))])
          (syntax-parse expr
            [({~datum seq} inner-expr ...)
             (compile-rewrite-exprs 
               (list (qq-art expr (put inner-expr ...)))
               '())])))
      (define result2 (flatten result))
      (apl-value/s (λ () result2)))))
  
(define-art-object (number [n]))
(define-for-syntax (number-value stx)
  (syntax-parse stx
    [(_ val:number) (syntax-e #'val)]))

(define-art-rewriter numbers
  (λ (stx)
    (syntax-parse stx
      [(_ the-number ...)
       #:with (the-number* ...)
        (for/list ([n (syntax->list #'(the-number ...))])
         (syntax-parse n
          [n:number (qq-art #'n (number n))]))
       (qq-art stx (ix-- the-number* ...))])))

(define-for-syntax (scalar-extend-left a w)
  (define aix (max-index* (map expr-indices a)))
  (define wix (max-index* (map expr-indices w)))
  (cond 
    [(and (null? aix) (not (null? wix)))
     (define a* (car a))
     (define wix (max-index* (map expr-indices w)))
     (for/list ([ix (get-index-range wix)]) (put-in-id-ctxt a* #`(index #,@ix)))]
    [else a]))

(define-syntax rho
  (apl-monad/s
    (λ (stx w)
      
      (define w-values ((apl-value/s-thunk w)))
      
      (define max-ix
        (for/fold ([acc #f])
                  ([expr w-values])
          (define ix (expr-indices expr))
          (if acc (max-index acc ix) ix)))
      (define result
        (compile-rewrite-exprs 
          (list (qq-art stx (ix-- #,@(for/list ([ix max-ix]) (qq-art stx (number #,(add1 ix))))))) '()))
      (apl-value/s (λ () result)))))

(define-for-syntax (float-scalars stx)
  (syntax-parse stx
    [({~datum seq} a)
     #:when (null? (expr-indices #'a))
     (float-scalars (put-in-id-ctxt #'a (context-ref (get-id-ctxt stx) #'index)))]
    [_ stx]))

(define-syntax enclose
  (apl-monad/s
    (λ (stx w)
      
      (define result (map float-scalars (compile-rewrite-exprs (list (qq-art stx (ix@ 0 (seq #,@((apl-value/s-thunk w)))))) '())))
      (apl-value/s (λ () result)))))

(define-syntax iota
  (apl-monad/s
    (λ (stx w)
      (define w-values ((apl-value/s-thunk w)))
      (unless (= (length w-values) 1) (raise-syntax-error 'iota "expected scalar on right of iota"))
      (define nums (for/list ([i (in-range (number-value (car w-values)))]) #`(number #,i)))
      (define result (compile-rewrite-exprs (list (qq-art stx (ix-- #,@nums))) '()))
      (apl-value/s (λ () result)))))

(define-for-syntax (apl-apply-function-dyadically f stx a w)

      (define a-values- ((apl-value/s-thunk a)))
      (define w-values- ((apl-value/s-thunk w)))
      (define a-values (scalar-extend-left a-values- w-values-))
      (define w-values (scalar-extend-left w-values- a-values-))
      (define aix (max-index* (map expr-indices a-values)))
      (define wix (max-index* (map expr-indices w-values)))
      ;; FIXME jagen figure out how APL does these operators
      (unless (equal? aix wix) (raise-syntax-error 'apl:max "cant do unequal indexes yet" stx))

      (define (do-op l r ix)
        (syntax-parse #`(#,l #,r)
          [(({~datum number} l:number) ({~datum number} r:number))
           (qq-art stx (ix@ #,ix (number #,(f (syntax-e #'l) (syntax-e #'r)))))]
          [(({~datum number} l:number) ({~datum seq} expr ...))
           #:do [(define l* (syntax-e #'l))]
           #:with (expr* ...) 
             (for/list ([e (syntax->list #'(expr ...))])
               (qq-art e (number #,(* l* (number-value e)))))
           (qq-art stx (ix@ #,ix (seq expr* ...)))]
          [(({~datum seq} expr ...) ({~datum number} r:number))
           #:do [(define r* (syntax-e #'r))]
           #:with (expr* ...) 
             (for/list ([e (syntax->list #'(expr ...))])
               (qq-art e (number #,(* r* (number-value e)))))
           (qq-art stx (ix@ #,ix (seq expr* ...)))]
          [(({~datum seq} exprl ...) ({~datum seq} exprr ...))
           #:with (expr* ...) 
             (for/list ([l (syntax->list #'(exprl ...))] [r (syntax->list #'(exprr ...))])
               (qq-art l (number #,(* (number-value l) (number-value r)))))
           (qq-art stx (ix@ #,ix (seq expr* ...)))]))

      (define maxs 
        (for/list ([ix (get-index-range aix)]) (do-op (context-ref/index a-values ix) (context-ref/index w-values ix) ix)))
      
      (define result (compile-rewrite-exprs maxs '()))
      (apl-value/s (λ () result)))

(define-syntax apl:max (apl-dyad/s (λ (stx a w) (apl-apply-function-dyadically max stx a w))))
(define-syntax apl:min (apl-dyad/s (λ (stx a w) (apl-apply-function-dyadically min stx a w))))
(define-syntax apl:and (apl-dyad/s (λ (stx a w) (apl-apply-function-dyadically lcm stx a w))))
(define-syntax apl:+ (apl-dyad/s (λ (stx a w) (apl-apply-function-dyadically + stx a w))))
(define-syntax apl:- (apl-dyad/s (λ (stx a w) (apl-apply-function-dyadically - stx a w))))
(define-syntax apl:* (apl-dyad/s (λ (stx a w) (apl-apply-function-dyadically * stx a w))))
(define-syntax apl:>= (apl-dyad/s (λ (stx a w) (apl-apply-function-dyadically (λ (a w) (if (>= a w) 1 0)) stx a w))))

(define-syntax reduce
  (apl-dyad/s
    (λ (stx a w)
      (define a-fun (apl-dyad/s-body a))
      (define w-values ((apl-value/s-thunk w)))
      (define max-ix (max-index* (map expr-indices w-values)))
      (define axis (get-index-axis max-ix (sub1 (length max-ix))))
      (define init (for/list ([the-ix (car axis)]) (put-in-id-ctxt (context-ref/index w-values the-ix) #'(index))))
      (define result
        (for/fold ([acc init])
                  ([the-ixs (cdr axis)])
          (define the-ctxt (for/list ([the-ix the-ixs]) (put-in-id-ctxt (context-ref/index w-values the-ix) #'(index))))
          ((apl-value/s-thunk (a-fun stx (apl-value/s (λ () acc)) (apl-value/s (λ () the-ctxt)))))))
      (apl-value/s (λ () result)))))

(define-for-syntax (do-ravel w)
  (define max-ix (max-index* (map expr-indices w)))
  (for/list ([ix (get-index-range max-ix)] [i (in-naturals)])
    (define expr (context-ref/index w ix))
    (define expr* 
      (syntax-parse expr
        [({~datum seq} inner-expr ...)
         (qq-art expr (seq #,@(do-ravel (syntax->list #'(inner-expr ...)))))]
        [_ expr]))
    (put-in-id-ctxt expr* #`(index #,i))))

(define-syntax ravel
  (apl-monad/s
    (λ (stx w)
      (define w-values ((apl-value/s-thunk w)))
      (define result (do-ravel w-values))
      (apl-value/s (λ () result)))))

(define-syntax apl:first
  (apl-monad/s
    (λ (stx w)
      (define w-values ((apl-value/s-thunk w)))
      (define result (list (context-ref/index w-values (zero-index (length (expr-indices (car w-values)))))))
      (apl-value/s (λ () result)))))

(define-syntax replicate
  (apl-dyad/s
    (λ (stx a w)
      (define a-values- ((apl-value/s-thunk a)))
      (define w-values- ((apl-value/s-thunk w)))
      (define a-values (scalar-extend-left a-values- w-values-))
      (define w-values (scalar-extend-left w-values- a-values-))
      (unless (= (length a-values) (length w-values)) 
        (raise-syntax-error 'replicate "can only replicate at exact same shape for now" stx))
      (define max-ix (max-index* (map expr-indices a-values)))

      (define repeated (flatten 
        (for/list ([ix (get-index-range max-ix)])
          (define reps (context-ref/index a-values ix))
          (define expr (context-ref/index w-values ix))
          (for/list ([i (in-range (number-value reps))]) expr))))
      (define result
        (for/list ([i (in-naturals)] [expr repeated])
          (put-in-id-ctxt expr #`(index #,i))))
      (apl-value/s (λ () result)))))

(define-for-syntax (eval-apl-expr expr ctxt)
  (syntax-parse expr
    [({~datum lit} num:number ...)
     (define num* (syntax->list #'(num ...)))
     (define arr 
       (compile-rewrite-exprs 
         (list (if (= (length num*) 1) 
                   (qq-art expr (ix@ () (number #,(car num*))))
                   (qq-art expr (ix-- #,@(map (λ (n) (qq-art n (number #,n))) num*))))) '()))
     (apl-value/s (λ () arr))]
    [({~datum monad-dfn} expr)
     (apl-monad/s
       (λ (stx w)
         (eval-apl-expr #'expr (free-id-table-set ctxt #'ω w))))]
    [(head arg)
     (define f (eval-apl-expr #'head ctxt))
     (define arg* (eval-apl-expr #'arg ctxt))
     (displayln (format "rewriting with ~a" (syntax->datum #'head)))
     ((apl-monad/s-body f) expr arg*)]
    [(head left right)
     (define f (eval-apl-expr #'head ctxt))
     (define left* (eval-apl-expr #'left ctxt))
     (define right* (eval-apl-expr #'right ctxt))
     (displayln (format "rewriting with ~a" (syntax->datum #'head)))
     ((apl-dyad/s-body f) expr left* right*)]
    [name:id 
     (define it (lookup #'name))
     (cond
       [it it]
       [else (free-id-table-ref ctxt #'name)])]))

(define-art-rewriter run-apl
  (λ (stx)
    (syntax-parse stx
      [(_ expr) 
       (define result (eval-apl-expr #'expr (make-immutable-free-id-table)))
       #`(replace-full-context #,@((apl-value/s-thunk result)))])))


#;(println "THE BIG MONEY")
#;(perform (quote-performer) #;(draw-seq-performer [800 400] [number-drawer])
  (ix-- (seq (ix-- (numbers 1 2 3) (numbers 4 5 6))) (seq (ix-- (numbers 7 8 9))) (seq (ix-- (numbers 10 11 20))))
  (run-apl
    (reduce apl:+ 
      (apl:* 
        (apl:+ (lit 1) (iota (rho *ctxt*)))
        ((each (monad-dfn (reduce apl:and (ravel (apl:>= (mix (replicate (apl:first (rho ω)) (enclose (lit 12 13 14)))) ω))))) *ctxt*)))))

#;(println "THE BIG MONEY")
#;(perform #;(draw-seq-performer [800 400] [number-drawer]) (quote-performer)
  (ix-- (seq (ix-- (numbers 1 2 3) (numbers 4 5 6))) (seq (ix-- (numbers 7 8 9))) (seq (ix-- (numbers 10 11 20))))
  (run-apl
    ((each (monad-dfn (reduce apl:and (ravel (apl:>= (mix (replicate (apl:first (rho ω)) (enclose (lit 12 13 14)))) ω))))) *ctxt*)))

#;(define-interpretation testapl)

#;(interpretation+ testapl
  [sample
   (ix-- (seq (ix-- (seq (ix-- (number 1) (number 2))) (seq (ix-- (number 3) (number 4)))))
         (seq (ix-- (seq (ix-- (number 5) (number 6))) (seq (ix-- (number 7) (number 8))))))])

#;(perform (quote-performer)
  (ix-- (ix-- (dumb) (dumber) (dumbest))
        (ix-- (dumber) (dumbest) (dumb))
  (run-apl (rho *ctxt*))))

#;(perform (quote-performer)
  (seq
    (sample) (interpret testapl)
    (run-apl (rho (mix *ctxt*)))))
#;(println "nuttin")

#;(perform (draw-seq-performer [400 200])
  (sample) (interpret testapl))

#;(println "mix")

#;(perform (draw-seq-performer [400 200])
  (sample) (interpret testapl)
  (run-apl (mix *ctxt*)))

#;(println "mix each")

#;(perform (draw-seq-performer [400 200] [number-drawer])
  (sample) (interpret testapl)
  (run-apl ((each mix) *ctxt*)))

#;(perform (quote-performer)
  (sample) (interpret testapl)
  (run-apl (enclose *ctxt*)))

#;(perform (quote-performer)
  (run-apl (apl:max (lit 1 3 5) (lit 6 4 2))))

#;(perform (quote-performer)
  (run-apl (reduce apl:min (lit 1 2 3 4 5 6 7))))


#;(println "replicate!")
#;(perform (draw-seq-performer [400 200] [number-drawer])
  (numbers 1 2 3)
  (run-apl (replicate (lit 2 3 4) *ctxt*)))


#;(println "first!")
#;(perform (draw-seq-performer [400 200] [number-drawer])
  (ix-- (numbers 1 2 3) (numbers 4 5 6) (numbers 7 8 9))
  (run-apl (apl:first *ctxt*)))

#;(println "iota!")
#;(perform (draw-seq-performer [400 200] [number-drawer])
  (run-apl (iota (lit 6))))



#;(println "ravel")
#;(perform (draw-seq-performer [400 200] [number-drawer])
  (ix-- (numbers 1 2 3) (numbers 4 5 6) (numbers 7 8 9))
  (run-apl (ravel *ctxt*)))