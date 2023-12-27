#lang racket

(require art art/coordinate/index art/private/draw 2htdp/image (for-syntax syntax/parse racket/list syntax/id-set))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-index-coordinate index)

;;;;;;; array context.
(define-art-embedding (seq [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-mapping-rewriter (rewrite-in-seq [(: s seq)])
  (let ()
    (define (go stx s)
      (syntax-parse stx
        [(_ expr ... {~seq #:capture [name:id ...]})
         (syntax-parse s
           [(_ expr* ...)
             #:do [
              (define names (immutable-free-id-set (syntax->list #'(name ...))))
              (define captures 
                (if (attribute name)
                  (filter 
                    (λ (e) (syntax-parse e 
                      [(head:id _ ...) (free-id-set-member? names #'head)]))
                    (current-ctxt))
                  '()))]
             #:with (result ...) 
               (rewrite-in (append captures (syntax->list #'(expr* ...))) #`(context expr ... #,@(map delete-expr captures)))
             #`(context #,(delete-expr s) #,(qq-art s (seq result ...)))])]
       ;; FIXME jagen
       [(head expr ...) (go #'(head expr ... #:capture []) s)]))
    go))


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

(define-art-rewriter numbers
  (λ (stx)
    (syntax-parse stx
      [(_ the-number ...)
       #:with (the-number* ...)
        (for/list ([n (syntax->list #'(the-number ...))]) (qq-art n (number #,n)))
       (qq-art stx (ix-- the-number* ...))])))

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
             [({~literal !} value:number)
              (define items (require-context (current-ctxt) expr #'seq))
              (syntax-parse items
                [(_ the-items ...) 
                 #:with the-item (or (findf (λ (expr) 
                   (syntax-parse (or (context-ref (get-id-ctxt expr) #'index) #'(index -1))
                     [(_ ix*:number) (= (syntax-e #'ix*) (syntax-e #'value))]))
                   (syntax->list #'(the-items ...)))
                  (raise-syntax-error 'seq-ref (format "No item with given index in sequence: ~s" (un-@ items)) expr))
                 (values (cons (qq-art expr (context the-item)) acc1)
                         (cons (delete-expr expr) acc2))])]
             [_ (values acc1 acc2)])))
        (append deletes exprs))

     #'(@ () result ...)]))

(module+ test
  #;(realize (quote-realizer) (put (seq 42)) (rewrite-in-seq (delete !)))
  #;(realize (quote-realizer) (put (seq (i@ [0 8] (loop 2 (dummy))) (expand-loop)) (! 0)) (seq-ref))
  
  #;(realize (quote-realizer)
    (seq (ix-- (dumb) (dumber))) (! 0) (! 1)
    (seq-ref)    
    (-- [2 (hole)] [2 (hole)])
    (fill-holes dumb))
  
  #;(realize (quote-realizer)
    (@ [(instant 1)] (seq (ix-- (dumb))))
    (@ [(instant 1)] (! 0))
    (seq-ref))
  
  #;(realize (quote-realizer)
    (-- [4 (dumb)] [2 (dumb)] [3 (dumb)])
    (unapply-rhythm dumb))
  
 
  
  #;(realize (quote-realizer)
    (ix-- (seq (ix-- (dumb) (dumber))) (seq (ix-- (dumbest))))
    (coalesce-seq))
  
  #;(realize (quote-realizer)
    (ix-- (ix-- (dumb) (dumber) (dumbest)) (ix-- (dumbest) (dumber) (dumb)))
    (split seq))
  
  #;(realize (quote-realizer)
    (ix@ 1 (ix@ 2 (seq (ix@ [2] (seq (ix-- (ix-- (dumb) (dumber) (dumbest)))))))))
  
  #;(realize (quote-realizer)
    (debug-perform (quote-realizer)))
  
  #;(define-interpretation test-interp)
  
  (interpretation+ test-interp
    [foo (dumb)])
  
  #;(realize (quote-realizer)
    (foo)
    (interpret test-interp)))
  

(define-for-syntax (do-draw-seq ctxt width height)
  (define max-ix
    (for/fold ([acc '()])
              ([expr ctxt])
      (define ix (expr-index expr))
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

  (define get-expr-single-index 
    (cond 
      [(= (length max-ix) 2) expr-index]
      [(= (length max-ix) 1)
       (λ (e) (list 0 (expr-single-index e)))]
      [(= (length max-ix) 0)
       (λ (e) (list 0 0))]))

  (define result
    (for/fold ([acc (hash)]) 
              ([e ctxt])
       (hash-set acc (get-expr-single-index e)
         (syntax-parse e
           [({~literal seq} expr ...)
            (define sub-result (do-draw-seq (syntax->list #'(expr ...)) (- each-width 20) (- each-height 20)))
            ;; draw a box for nested sequences, apl style
            #`(overlay (rectangle #,each-width #,each-height 'outline 'blue) #,sub-result)]
           [_ (drawer-recur e)]))))

  (define result2 
    (for/fold ([im #'empty-image])
              ([(ix expr) (in-hash result)])
      #`(overlay/xy #,im (* #,each-width #,(cadr ix)) (* #,each-height #,(car ix)) #,expr)))
  result2)

(define-drawer draw-seq
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (do-draw-seq (syntax->list #'(expr ...)) (drawer-width) (drawer-height))])))

(register-drawer! seq draw-seq)