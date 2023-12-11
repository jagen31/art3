#lang racket

(require "../core.rkt" "../coordinate/index.rkt" "../stdlib.rkt" 2htdp/image 
         (for-syntax syntax/parse racket/list))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-index-coordinate index)

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

;;;;;;; LISTS of ITEMS in a context.
(define-art-embedding (seq [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (compile-rewrite-exprs (list (quasisyntax/loc stx (@ () expr ...))) '())])))
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
               [({~literal seq} expr ...)
                (map (λ (e) (remove-from-id-ctxt e #'index)) (syntax->list #'(expr ...)))])))
      #`(@ () #,@(map delete-expr sorted) #,(qq-art stx (seq (ix-- result ...))))])))



(define-for-syntax (do-draw-seq ctxt width height drawers)
  (define max-ix
    (for/fold ([acc #f])
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

(define-art-realizer draw-seq-performer
  (λ (stx)
    (syntax-parse stx
      [(_ [width:number height:number] [drawers ...])
       (do-draw-seq (current-ctxt) (syntax-e #'width) (syntax-e #'height)
         (map (compose drawer/s-body syntax-local-value) (syntax->list #'(drawers ...))))])))


(module+ test
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
  
  #;(perform (quote-performer)
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
    (interpret test-interp)))
  