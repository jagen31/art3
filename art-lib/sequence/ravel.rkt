#lang racket

(require "../core.rkt" "../stdlib.rkt" "lib.rkt" "../coordinate/index.rkt" (for-syntax syntax/parse racket/list syntax/id-table))
(provide (all-defined-out) (for-syntax (all-defined-out)))

;; FIXME jagen segregate ravel (its just apl) into: 
;;   1. things that are seq and number specific
;;   2. things that are seq specific, but not number specific
;;   3. things that are general for all indexes
;;   (possibly things that are general for all indexes but specific to numbers)

(begin-for-syntax
  (struct apl-value/s [thunk])
  (struct apl-monad/s [body])
  (struct apl-dyad/s [body]))

;; reify the artwork as a value in apl
(define-syntax *ctxt* (apl-value/s (λ () (current-ctxt))))


(define-for-syntax (float-scalars stx)
  (syntax-parse stx
    [({~literal seq} a)
     #:when (null? (expr-index #'a))
     (float-scalars (put-in-id-ctxt #'a (context-ref (get-id-ctxt stx) #'index)))]
    [_ stx]))

(define-syntax each
  (apl-monad/s
    (λ (stx f)
      (apl-monad/s
        (λ (stx w)
          (define w-values ((apl-value/s-thunk w)))
          (define result
            (flatten
              (for/list ([expr w-values])
                (syntax-parse expr
                  [({~literal seq} inner-expr ...)
                   (define val
                     ((apl-monad/s-body f) stx (apl-value/s (λ () (syntax->list #'(inner-expr ...))))))
                   (float-scalars (qq-art expr (seq #,@((apl-value/s-thunk val)))))]))))
          (apl-value/s (λ () result)))))))

(define-syntax mix
  (apl-monad/s
    (λ (stx w)
      
      (define result
        (for/list ([expr ((apl-value/s-thunk w))])
          (syntax-parse expr
            [({~literal seq} inner-expr ...)
             (compile-rewrite-exprs 
               (list (qq-art expr (put inner-expr ...)))
               '())])))
      (define result2 (flatten result))
      (apl-value/s (λ () result2)))))
  
(define-for-syntax (scalar-extend-left a w)
  (define aix (max-index* (map expr-index a)))
  (define wix (max-index* (map expr-index w)))
  (cond 
    [(and (null? aix) (not (null? wix)))
     (define a* (car a))
     (define wix (max-index* (map expr-index w)))
     (for/list ([ix (get-index-range wix)]) (put-in-id-ctxt a* #`(index #,@ix)))]
    [else a]))

(define-syntax rho
  (apl-monad/s
    (λ (stx w)
      
      (define w-values ((apl-value/s-thunk w)))
      
      (define max-ix
        (for/fold ([acc #f])
                  ([expr w-values])
          (define ix (expr-index expr))
          (if acc (max-index acc ix) ix)))
      (define result
        (compile-rewrite-exprs 
          (list (qq-art stx (ix-- #,@(for/list ([ix max-ix]) (qq-art stx (number #,(add1 ix))))))) '()))
      (apl-value/s (λ () result)))))


(define-syntax enclose
  (apl-monad/s
    (λ (stx w)
      
      (define result (map float-scalars (compile-rewrite-exprs (list (qq-art stx (ix@ () (seq #,@((apl-value/s-thunk w)))))) '())))
      (apl-value/s (λ () result)))))

(define-syntax iota
  (apl-monad/s
    (λ (stx w)
      (define start
        (syntax-parse stx
          [(_ _ {~or* {~seq #:start num:number} {~seq}}) (if (attribute num) (syntax-e #'num) 0)]))
      (define w-values ((apl-value/s-thunk w)))
      (unless (= (length w-values) 1) (raise-syntax-error 'iota (format "expected scalar on right of iota, got ~s" (map un-@ w-values))))
      (define nums (for/list ([i (in-range start (+ start (number-value (car w-values))))]) #`(number #,i)))
      (define result (compile-rewrite-exprs (list (qq-art stx (ix-- #,@nums))) '()))
      (apl-value/s (λ () result)))))

(define-for-syntax (apl-apply-function-dyadically f stx a w)

      (define a-values- ((apl-value/s-thunk a)))
      (define w-values- ((apl-value/s-thunk w)))
      (define a-values (scalar-extend-left a-values- w-values-))
      (define w-values (scalar-extend-left w-values- a-values-))
      (define aix (max-index* (map expr-index a-values)))
      (define wix (max-index* (map expr-index w-values)))
      ;; FIXME jagen figure out how APL does these operators
      (unless (equal? aix wix) (raise-syntax-error 'apl:max "cant do unequal indexes yet" stx))

      (define (do-op l r ix)
        (syntax-parse #`(#,l #,r)
          [(({~literal number} l:number) ({~literal number} r:number))
           (qq-art stx (ix@ #,ix (number #,(f (syntax-e #'l) (syntax-e #'r)))))]
          [(({~literal number} l:number) ({~literal seq} expr ...))
           #:do [(define l* (syntax-e #'l))]
           #:with (expr* ...) 
             (for/list ([e (syntax->list #'(expr ...))])
               (qq-art e (number #,(* l* (number-value e)))))
           (qq-art stx (ix@ #,ix (seq expr* ...)))]
          [(({~literal seq} expr ...) ({~literal number} r:number))
           #:do [(define r* (syntax-e #'r))]
           #:with (expr* ...) 
             (for/list ([e (syntax->list #'(expr ...))])
               (qq-art e (number #,(* r* (number-value e)))))
           (qq-art stx (ix@ #,ix (seq expr* ...)))]
          [(({~literal seq} exprl ...) ({~literal seq} exprr ...))
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
(define-syntax apl:= (apl-dyad/s (λ (stx a w) (apl-apply-function-dyadically (λ (a w) (if (= a w) 1 0)) stx a w))))

(define-for-syntax (window-list li n)
  (for/list ([i (in-range (- (length li) (sub1 n)))])
    (map (λ (ix) (list-ref li ix)) (range i (+ i n)))))

(define-for-syntax (to-foldr-order li)
  (define revd (reverse li))
  (cons (cadr revd) (cons (car revd) (cddr revd))))

(define-syntax reduce
  (apl-dyad/s
    (λ (stx a w)
      (define a-fun (apl-dyad/s-body a))
      (define w-values ((apl-value/s-thunk w)))
      (define max-ix (max-index* (map expr-index w-values)))

      (define-values (axis-num window-size)
        (syntax-parse stx [(reduce _ _ 
          {~or* 
            {~seq {~seq #:axis axis:number} {~seq #:window window:number}}
            {~seq #:axis axis:number}
            {~seq #:window window:number}
            {~seq}})
          (define ax (if (attribute axis) (syntax-e #'axis) (sub1 (length max-ix))))
          (define win (if (attribute window) (syntax-e #'window) #f))
          (values ax win)]))

      (define axis (get-index-axis max-ix axis-num))

    (define result
     (cond 
       [window-size
        
        (for/fold ([acc '()] #:result (reverse acc))
                  ([the-ixs axis])

          (define windowed (window-list the-ixs (or window-size (length the-ixs))))
          ;; reduce over the row/column

          (define window-results 
            (for/list ([the-ixs windowed])
              (for/foldr ([acc (put-in-id-ctxt (context-ref/index w-values (car the-ixs)) #'(index))])
                        ([the-ix (cdr the-ixs)]) 
                (define the-value (put-in-id-ctxt (context-ref/index w-values the-ix) #`(index)))
                ;; FIXME jagen do this operation unboxed to speed it up a lot
                (define result ((apl-value/s-thunk (a-fun stx (apl-value/s (λ () (list acc))) (apl-value/s (λ () (list the-value)))))))
                (float-scalars (put-in-id-ctxt (ensure-id-ctxt #`(seq #,@result)) #'(index))))))

                
          ;; create an index without the axis we're reducing
          (define-values (left right) 
            (values (take (car the-ixs) axis-num) (take-right (car the-ixs) (- (length (car the-ixs)) (add1 axis-num)))))
          (define window-result (compile-rewrite-exprs (list #`(ix@ (#,@left #,@right) (ix-- #,@window-results))) '()))
          (append (reverse window-result) acc))]
       [else 
        (for/fold ([acc '()])
                  ([the-ixs- axis])

          (define the-ixs (reverse the-ixs-))

          (define result
            (for/fold ([acc (put-in-id-ctxt (context-ref/index w-values (car the-ixs)) #'(index))])
                      ([the-ix (cdr the-ixs)]) 
              (define the-value (put-in-id-ctxt (context-ref/index w-values the-ix) #`(index)))
              ;; FIXME jagen do this operation unboxed to speed it up a lot
              (define result ((apl-value/s-thunk (a-fun stx (apl-value/s (λ () (list the-value))) (apl-value/s (λ () (list acc)))))))
              (float-scalars (put-in-id-ctxt (ensure-id-ctxt #`(seq #,@result)) #'(index)))))

                
          ;; create an index without the axis we're reducing
          (define-values (left right) 
            (values (take (car the-ixs) axis-num) (take-right (car the-ixs) (- (length (car the-ixs)) (add1 axis-num)))))
          (append (compile-rewrite-exprs (list #`(ix@ (#,@left #,@right) #,result)) '()) acc))]))

      (apl-value/s (λ () result)))))

(define-for-syntax (do-ravel w)
  (define max-ix (max-index* (map expr-index w)))
  (for/list ([ix (get-index-range max-ix)] [i (in-naturals)])
    (define expr (context-ref/index w ix))
    (define expr* 
      (syntax-parse expr
        [({~literal seq} inner-expr ...)
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
      (define result (list (put-in-id-ctxt (context-ref/index w-values (zero-index (length (expr-index (car w-values))))) #'(index))))
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
      (define max-ix (max-index* (map expr-index a-values)))

      (define repeated (flatten 
        (for/list ([ix (get-index-range max-ix)])
          (define reps (context-ref/index a-values ix))
          (define expr (context-ref/index w-values ix))
          (for/list ([i (in-range (number-value reps))]) expr))))
      (define result
        (for/list ([i (in-naturals)] [expr repeated])
          (put-in-id-ctxt expr #`(index #,i))))
      (apl-value/s (λ () result)))))

(define-syntax apl:compose
  (apl-dyad/s
    (λ (stx a w)
      (define a-monad (apl-monad/s-body a))
      (define w-monad (apl-monad/s-body w))
      (apl-monad/s (λ (stx w) (a-monad stx (w-monad stx w)))))))

(define-for-syntax (wrap-if-scalar vals)
  (define max-ix (max-index* (map expr-index vals)))
  (if (null? max-ix)
    (list (put-in-id-ctxt (car vals) #'(index 0)))
    vals))

(define-syntax catenate
  (apl-dyad/s
    (λ (stx a w)
      (define a-values (wrap-if-scalar ((apl-value/s-thunk a))))
      (define w-values (wrap-if-scalar ((apl-value/s-thunk w))))
      (define max-ix (max-index* (map expr-index w-values)))
      (define axis-num
        (syntax-parse stx [(_ _ _ {~or* {~seq #:axis axis:number} {~seq}})
          (if (attribute axis) (syntax-e #'axis) (sub1 (length max-ix)))]))
      (define axis (get-index-axis max-ix axis-num))
      (define result-
        (for/fold ([acc '()])
                  ([the-ixs axis])
          (append
            (for/list ([the-ix the-ixs])
              (put-in-id-ctxt (context-ref/index w-values the-ix) #`(index #,@(list-update the-ix axis-num add1))))
            acc)))
      (define max-a-ix (max-index* (map expr-index a-values)))
      (define result
        (append 
          (for/list ([the-ixs axis] [the-a-ix (get-index-range max-a-ix)])
              (put-in-id-ctxt (context-ref/index a-values the-a-ix) #`(index #,@(car the-ixs))))
          result-))
      (apl-value/s (λ () result)))))

;;;; NUMBE 

(define-for-syntax (do-negate exprs)
  (for/list ([val exprs])
    (qq-art val 
      #,(syntax-parse val
        [({~literal seq} expr ...) 
         (do-negate (syntax->list #'(expr ...)))]
        [({~literal number} val:number)
         #`(number #,(- (syntax-e #'val)))]))))

(define-syntax negate
  (apl-monad/s 
    (λ (stx w) 
      (define result (do-negate ((apl-value/s-thunk w))))
      (apl-value/s (λ () result)))))

(define-syntax apl:reverse
  (apl-monad/s
    (λ (stx w)
      (define w-values ((apl-value/s-thunk w)))
      (define max-ix (max-index* (map expr-index w-values)))
      (define result
        (if (null? max-ix)
          w-values
          (reverse (map (λ (e) (put-in-id-ctxt e #`(index #,(- (car max-ix) (expr-single-index e) 1)))) w-values))))
      (apl-value/s (λ () result)))))

(define-for-syntax (eval-apl-expr expr ctxt)
  (syntax-parse expr
    [({~datum assign} ([name:id bound-body] ...) body)
     (define ctxt*
       (for/fold ([acc ctxt])
                 ([name (syntax->list #'(name ...))] [bb (syntax->list #'(bound-body ...))])
         (free-id-table-set acc name (eval-apl-expr bb acc))))
     (eval-apl-expr #'body ctxt*)]
    [({~datum lit} num:number ...)
     (define num* (syntax->list #'(num ...)))
     (define arr 
       (compile-rewrite-exprs 
         (list (if (= (length num*) 1) 
                   (qq-art expr (ix@ () (number #,(car num*))))
                   (qq-art expr (ix-- #,@(map (λ (n) (qq-art n (number #,n))) num*))))) '()))
     (apl-value/s (λ () arr))]
    [({~datum apl:if} c t e)
     (define result ((apl-value/s-thunk (eval-apl-expr #'c ctxt))))
     (cond [(zero? (number-value (car result))) 
            (eval-apl-expr #'e ctxt)]
          [else
            (define result (eval-apl-expr #'t ctxt)) 
            result])]
    [({~datum monad-dfn} expr)
     (define fun
       (apl-monad/s
         (λ (stx w)
           (eval-apl-expr #'expr (free-id-table-set (free-id-table-set ctxt #'ω w) #'recur fun)))))
     fun]
    [({~datum dyad-dfn} expr)
     (define fun
       (apl-dyad/s
         (λ (stx a w)
           (eval-apl-expr #'expr (free-id-table-set (free-id-table-set (free-id-table-set ctxt #'ω w) #'α a) #'recur fun)))))
     fun]
    [(head arg ...)
     (define f (eval-apl-expr #'head ctxt))
     (define args (syntax->list #'(arg ...)))
     ;; (displayln (format "rewriting with ~a" (syntax->datum #'head)))
     (cond
       [(apl-dyad/s? f) 
        (define arg1 (eval-apl-expr (car args) ctxt))
        (define arg2 (eval-apl-expr (cadr args) ctxt))
        (define result ((apl-dyad/s-body f) expr arg1 arg2))
        result]
       [(apl-monad/s? f) 
        (define arg1 (eval-apl-expr (car args) ctxt))
        (define result ((apl-monad/s-body f) expr arg1))
        result])]
    [name:id 
     (define it (free-id-table-ref ctxt #'name (λ () #f)))
     (cond
       [it it]
       [else 
        (syntax-local-value #'name)])]))

(define-art-rewriter run-apl
  (λ (stx)
    (syntax-parse stx
      [(_ expr) 
       (define result (eval-apl-expr #'expr (make-immutable-free-id-table)))
       #`(replace-full-context #,@((apl-value/s-thunk result)))])))

(println "catenate")
  (perform (quote-performer)
    (ix-- (seq (numbers 2 2 2)))
    (run-apl (catenate *ctxt* (enclose (lit 0 0)))))

  (println "reduce")
  (perform (quote-performer)
    (ix-- (numbers 1 2 3) (numbers 4 5 6) (numbers 7 8 9))
    (run-apl (reduce apl:+ *ctxt* #:axis 0)))

(println "more money")
  (perform (quote-performer)
    (ix-- (number 1) (number 3) (number 5) (number 7))
    (run-apl 
      ((monad-dfn (apl:if (reduce apl:and (apl:= (lit 0) ω)) 
                         (enclose ω) 
                         (catenate (enclose ω) (recur (negate (reduce apl:- ω #:window 2)))))) *ctxt*)))

(println "reverse")
(perform (quote-performer)
  (numbers 1 2 3 4)
  (run-apl (apl:reverse *ctxt*)))

 (println "first!")
  (perform #;(draw-seq-performer [400 200] [number-drawer]) (quote-performer)
    (ix-- (seq (ix-- (seq (numbers 1 2 3)) (seq (numbers 4 5 6)) (seq (numbers 7 8 9)))))
    (run-apl ((each apl:first) *ctxt*)))

(println "reduce subtraction!")
(perform (quote-performer)
  (numbers 10 3 0 2 0)
  (run-apl (reduce apl:- *ctxt*)))

#;(module+ test
  (println "THE BIG MONEY")
  (perform (quote-performer) #;(draw-seq-performer [800 400] [number-drawer])
    (ix-- (seq (ix-- (numbers 1 2 3) (numbers 4 5 6))) (seq (ix-- (numbers 7 8 9))) (seq (ix-- (numbers 10 11 20))))
    (run-apl
      (reduce apl:+ 
        (apl:* 
          (apl:+ (lit 1) (iota (rho *ctxt*)))
          ((each (monad-dfn (reduce apl:and (ravel (apl:>= (mix (replicate (apl:first (rho ω)) (enclose (lit 12 13 14)))) ω))))) *ctxt*)))))
  
  (println "THE BIG MONEY")
  (perform #;(draw-seq-performer [800 400] [number-drawer]) (quote-performer)
    (ix-- (seq (ix-- (numbers 1 2 3) (numbers 4 5 6))) (seq (ix-- (numbers 7 8 9))) (seq (ix-- (numbers 10 11 20))))
    (run-apl
      ((each (monad-dfn (reduce apl:and (ravel (apl:>= (mix (replicate (apl:first (rho ω)) (enclose (lit 12 13 14)))) ω))))) *ctxt*)))
  
  (define-interpretation testapl)
  
  (interpretation+ testapl
    [sample
     (ix-- (seq (ix-- (seq (ix-- (number 1) (number 2))) (seq (ix-- (number 3) (number 4)))))
           (seq (ix-- (seq (ix-- (number 5) (number 6))) (seq (ix-- (number 7) (number 8))))))])
  
  (perform (quote-performer)
    (seq
      (sample) (interpret testapl)
      (run-apl (rho (mix *ctxt*)))))
  
  (println "nuttin")
  
  (perform (draw-seq-performer [400 200] [number-drawer])
    (sample) (interpret testapl))
  
  (println "mix")
  
  (perform (draw-seq-performer [400 200] [number-drawer])
    (sample) (interpret testapl)
    (run-apl (mix *ctxt*)))
  
  (println "mix each")
  
  (perform (draw-seq-performer [400 200] [number-drawer])
    (sample) (interpret testapl)
    (run-apl ((each mix) *ctxt*)))
  
  (perform (quote-performer)
    (sample) (interpret testapl)
    (run-apl (enclose *ctxt*)))
  
  (perform (quote-performer)
    (run-apl (apl:max (lit 1 3 5) (lit 6 4 2))))
  
  (perform (quote-performer)
    (run-apl (reduce apl:min (lit 1 2 3 4 5 6 7))))
  
  (println "replicate!")
  (perform (draw-seq-performer [400 200] [number-drawer])
    (numbers 1 2 3)
    (run-apl (replicate (lit 2 3 4) *ctxt*)))
  
  (println "first!")
  (perform (draw-seq-performer [400 200] [number-drawer])
    (ix-- (numbers 1 2 3) (numbers 4 5 6) (numbers 7 8 9))
    (run-apl (apl:first *ctxt*)))
  
  (println "iota!")
  (perform (draw-seq-performer [400 200] [number-drawer])
    (run-apl (iota (lit 6))))
  
  (println "ravel")
  (perform (quote-performer) #;(draw-seq-performer [400 200] [number-drawer])
    (ix-- (numbers 1 2 3) (numbers 4 5 6) (numbers 7 8 9))
    (run-apl (ravel *ctxt*)))
  
  (println "reduce")
  (perform (quote-performer)
    (ix-- (numbers 1 2 3) (numbers 4 5 6) (numbers 7 8 9))
    (run-apl (reduce apl:+ *ctxt*)))
  
  (println "reduce")
  (perform (quote-performer)
    (ix-- (numbers 1 2 3) (numbers 4 5 6) (numbers 7 8 9))
    (run-apl (reduce apl:+ *ctxt* #:axis 0)))
  
  (println "small money")
  (perform (quote-performer)
    (ix-- (numbers 46 85 75 82) (numbers 208 1412 1257 1410))
    (run-apl (reduce apl:* (reduce (dyad-dfn (reduce apl:+ (apl:>= (apl:* (iota α #:start 1) (apl:- α (iota α #:start 1))) ω))) *ctxt* #:axis 0)))))
    