#lang racket

(require "core.rkt" syntax-spec (for-syntax syntax/parse racket/list (except-in ee-lib racket-var)))
(provide (all-defined-out))

;;;;;;;;;;;; SOME GENERIC OBJECTS that may come in handy.

;;;;;;; LISTS of ITEMS in a context.
(define-art-object (list-items [items]))
(define-art-object (! [ix]))

;; index into the `list-items` (convert `!`s to their corresponding objects)
(define-rewriter list-item-ref
  (syntax-parser
    [_ 
     #:with (result ...)
       (for/fold ([acc '()] #:result (reverse acc)) 
                 ([expr (current-ctxt)])
         (syntax-parse expr
           [({~datum !} value:number)
             (define items (context-ref/surrounding (current-ctxt) (get-id-ctxt expr) #'list-items))
             (unless items (raise-syntax-error 'list-item-ref "no list items in context for ref" expr))
             (syntax-parse items
               [(_ the-items ...) 
                (cons (delete-expr expr) (cons (ctxt->@ (get-id-ctxt expr) (qq-art expr (put #,(list-ref (syntax->list #'(the-items ...)) (syntax-e #'value))))) acc))])]
           [_ acc]))
     (qq-art this-syntax (@ () result ...))]))

(define-performer quote-performer 
  (λ(stx)
    (syntax-parse stx
      [(_ exprs ...)
       #`'(#,@(for/list ([e (syntax->list #'(exprs ...))]) #`(@ [#,@(get-id-ctxt e)] #,e)))])))

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
