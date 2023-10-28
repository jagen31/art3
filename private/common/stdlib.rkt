#lang racket

(require "core.rkt" syntax-spec (for-syntax syntax/parse racket/list (except-in ee-lib racket-var)))
(provide (all-defined-out))

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

(define-for-syntax (un-@ expr) #`(@ [#,@(get-id-ctxt expr)] #,expr))

(define-performer quote-performer 
  (λ(stx)
    (syntax-parse stx
      [(_ exprs ...)
       #`'(#,@(for/list ([e (syntax->list #'(exprs ...))]) (un-@ e)))])))

(begin-for-syntax 
  (struct art-subperformer/s [body]))

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
                (parameterize ([current-ctxt (filter (λ(expr) (within? (get-id-ctxt expr) (get-id-ctxt melody))) (current-ctxt))])
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
