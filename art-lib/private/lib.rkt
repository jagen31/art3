#lang racket

(require art/private/core art/private/draw 
         art/coordinate/instant art/coordinate/index art/coordinate/interval art/coordinate/subset art/coordinate/switch
         2htdp/image
         (for-syntax syntax/parse racket/list racket/match (except-in ee-lib racket-var) syntax/id-table syntax/id-set))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art-object (number [n]))
(define-art-object (symbol [s]))

(define-for-syntax (number-value stx)
  (syntax-parse stx
    [(_ val:number) (syntax-e #'val)]))

(define-drawer number-drawer 
  (λ (e)
    (syntax-parse e
      [({~literal number} n:number)
       #`(overlay (text (~s n) 24 'blue) (rectangle #,(drawer-width) #,(drawer-height) 'solid 'transparent))]
      [_ #f])))

(register-drawer! number number-drawer)

(define-drawer symbol-drawer 
  (λ (e)
    (syntax-parse e
      [({~literal symbol} n:id)
       #`(overlay (text (~s 'n) 24 'blue) (rectangle #,(drawer-width) #,(drawer-height) 'solid 'transparent))]
      [_ #'empty-image])))

(register-drawer! symbol symbol-drawer)

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

       #`(@ () #,@(run-art-exprs (list set1 set2) (current-ctxt) (lookup-ctxt)))])))

(define-art-realizer quote-realizer (λ(stx) #`'(#,@(for/list ([e (current-ctxt)]) (un-@ e)))))

(begin-for-syntax 
  (struct art-subrealizer/s [body]))

(define-syntax (define-subrealizer stx)
  (syntax-parse stx
    [(_ name:id body)
     #'(define-syntax name (art-subrealizer/s body))]))

(define-syntax (define-composite-realizer stx)
  (syntax-parse stx
    [(_ name {sub-name ...} [init-statement ...] combiner)
     #'(define-art-realizer name
        ;; phase 1 code
        (λ (stx) 
           (define clauses 
             (flatten
               (for/list ([subn (syntax->list #'(sub-name ...))])
                 (define subperf (lookup subn))
                 ((art-subrealizer/s-body subperf) (current-ctxt)))))
           #`(let () init-statement ... #,(combiner clauses))))]))

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

;; delete by name from id-ctxt
(define-art-rewriter delete-from-id-context
  (λ (stx)
    (syntax-parse stx
      [(_ name:id)
       (define target
         (filter 
           (λ (expr) (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (current-ctxt)))
           (current-ctxt)))
       #`(@ () #,@(map delete-expr target) #,@(map (λ (e) (remove-from-id-ctxt e #'name)) target))])))

;;;;;;;;; SUBSET 


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

(define-for-syntax (float-modulo n m) (- n (* (floor (/ n m)) m)))