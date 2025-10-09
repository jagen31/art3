#lang racket

(require art/private/core art/private/draw
         art/coordinate/instant art/coordinate/index art/coordinate/interval art/coordinate/subset art/coordinate/switch
         2htdp/image (prefix-in 2htdp: 2htdp/image)
         (for-syntax syntax/parse racket/list racket/set racket/string racket/format racket/match syntax/id-table syntax/id-set))
(provide (all-defined-out) (for-syntax (all-defined-out)))



;;;;;;;;;;;; SOME GENERIC OBJECTS that may come in handy.

;;;;;;;;;
(define-art-object (boolean [b]))

(define-for-syntax (boolean-value stx)
  (syntax-parse stx
    [(_ val:boolean) (syntax-e #'val)]))

(define-art-rewriter ->boolean
  (λ (stx)
    #`(context #,@(map delete-expr (current-ctxt))
               #,@(for/list ([expr (current-ctxt)])
                    (syntax-parse expr
                      [({~literal number} 0) (qq-art expr (boolean #f))]
                      [_ (qq-art expr (boolean #t))])))))

;;;;;;;;;;;;



;;;;;;;;;;
(define-art-object (number [n]))

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

;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;
(define-art-object (symbol [s]))

(define-drawer symbol-drawer 
  (λ (e)
    (syntax-parse e
      [({~literal symbol} n:id)
       #`(overlay (text (~s 'n) 24 'blue) (rectangle #,(drawer-width) #,(drawer-height) 'solid 'transparent))]
      [_ #f])))

(register-drawer! symbol symbol-drawer)

;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;
(define-art-object (string [s]))

(define-for-syntax (string-value stx)
  (syntax-parse stx
    [(_ val:string) (syntax-e #'val)]))

(define-drawer string-drawer 
  (λ (e)
    (syntax-parse e
      [({~literal string} str:string)
       #`(overlay (text (~s str) 24 'blue) (rectangle #,(drawer-width) #,(drawer-height) 'solid 'transparent))]
      [_ #f])))

(register-drawer! string string-drawer)

;;;;;;;;;;;;;;;;;;;;;



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

;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;
(define-art-rewriter &&&
  (λ (stx)
    (syntax-parse stx
      [(_ {~and left-expr (left-rw:id _ ...)} {~and right-expr (right-rw:id _ ...)})
       
       (define rw1 (syntax-local-value #'left-rw))
       (define rw2 (syntax-local-value #'right-rw))

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
                 (define subperf (syntax-local-value subn))
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
                  (or (set-empty? names)
                    (syntax-parse expr
                      [(head:id _ ...) (free-id-set-member? names #'head)]))))
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



(define-art-object (art-union-type [expr]))
(define-art-object (art-focus-type [expr]))

(define-art-rewriter compute-type
  (λ (stx)
    (define target
      (filter 
        (λ (expr) (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (current-ctxt)))
        (current-ctxt)))
    (define tys
      (for/fold ([acc (immutable-free-id-set)]) 
                ([e (current-ctxt)])
        (syntax-parse e
          ;; FIXME jagen handle reified rewriters
          [(head:id _ ...) (set-add acc #'head)])))
    (with-syntax ([(ty* ...) (set->list tys)])
      (qq-art stx (art-union-type ty* ...)))))

(define-art-realizer type-string-realizer
  (λ (stx)
    (define typs (context-ref* (current-ctxt) #'art-union-type))
    #`#,(string-join
      (for/list ([typ typs])
        (syntax-parse typ
          [({~literal art-union-type} ty ...)
           (string-join (map (compose ~a syntax-e) (syntax->list #'(ty ...))) " | ")])) "\n")))

(define-art-rewriter objects-equal? 
  (λ (stx)
    (define expr (syntax->datum (car (current-ctxt))))
    (if (andmap (λ (x) (equal? (syntax->datum x) expr)) (cdr (current-ctxt)))
      #'(replace-full-context (boolean #t)) #'(replace-full-context (boolean #f)))))


;; randomly useful
(define-for-syntax (float-modulo n m) (- n (* (floor (/ n m)) m)))

(define-art-rewriter zoom
  (λ (stx)
    (qq-art stx (context 
      #,@(for/list ([expr (current-ctxt)] 
        #:when (not (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (lookup-ctxt)))) 
        (delete-expr expr))))))

(define-art-rewriter apply-coordinates
  (λ (stx)
    (define-values (result deletes)
    (for/fold ([acc (current-ctxt)] [exprs '()]) 
                     ([outer-expr (current-ctxt)] #:when (coordinate/s?
                 (syntax-local-value (car (syntax->list outer-expr)) (λ () #f))))
      
             (define new-ctxt
               (flatten
               (for/list ([inner-expr acc]) 
                 (if (context-within? (get-id-ctxt inner-expr) (get-id-ctxt outer-expr) (lookup-ctxt))
                     ;; TODO jagen31 this should be a merge into id ctxt
                     (list (delete-expr inner-expr) (put-in-id-ctxt inner-expr outer-expr))
                     inner-expr))))

             (values new-ctxt (cons outer-expr exprs))))
    #`(replace-full-context
        #,@result
        #,@(map delete-expr deletes))))