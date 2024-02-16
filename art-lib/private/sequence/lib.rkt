#lang racket

(require art/base art/coordinate/index 2htdp/image (for-syntax syntax/parse racket/list syntax/id-set))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-index-coordinate index)

;;;;;;; array context.
(define-art-embedding (seq [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-mapping-rewriter (rewrite-in-seq [(: s seq)])
  (λ (stx s)
    (syntax-parse stx
      [(_ expr ...)
       (syntax-parse s
         [(_ expr* ...)
           #:with (result ...) 
             (rewrite-in (syntax->list #'(expr* ...)) #'(context expr ...))
           #`(context #,(qq-art s (seq result ...)))])])))


(define-art-rewriter ix@
  (λ (stx)
    (syntax-parse stx
      [(_ n:number expr ...) 
       (qq-art stx (ix@ [n] expr ...))]
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
     #:do [(define !s (context-ref*/within (lookup-ctxt) (get-id-ctxt this-syntax) #'!))]
     #:with (result ...)
       (for/list ([e !s])
         (syntax-parse e
           [({~literal !} value:number)
            (define items (require-context (lookup-ctxt) e #'seq))
            (syntax-parse items
              [(_ the-items ...) 
               #:with the-item (or (findf (λ (expr) 
                 (syntax-parse (or (context-ref (get-id-ctxt expr) #'index) #'(index -1))
                   [(_ ix*:number) (= (syntax-e #'ix*) (syntax-e #'value))]))
                 (syntax->list #'(the-items ...)))
                (raise-syntax-error 'seq-ref (format "No item with given index in sequence: ~s" (un-@ items)) e))
               (qq-art e (context the-item))])]))

     #`(context #,@(map delete-expr !s) result ...)]))

(define-for-syntax (do-draw-seq ctxt width height)
  (define max-ix
    (map add1
         (for/fold ([acc #f])
                   ([expr ctxt])
           (define ix (expr-index expr))
           (if acc (max-index acc ix) ix))))
  (when (> (length max-ix) 2) (raise-syntax-error 'draw-seq "cannot draw indexes greater than 2 yet" #f))

  (define-values (each-width each-height)
    (cond 
      [(= (length max-ix) 2)
       (values (floor (/ width (cadr max-ix))) (floor (/ height (car max-ix))))]
      [(= (length max-ix) 1)
       (values (floor (/ width (car max-ix))) (floor height))]
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
