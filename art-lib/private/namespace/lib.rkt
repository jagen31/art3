#lang racket

(require art art/coordinate/name 2htdp/image 
         (for-syntax syntax/parse racket/list syntax/id-table racket/dict racket/function))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-name-coordinate name)

(define-art-rewriter name@
  (λ (stx)
    (syntax-parse stx
      [(_ n:id expr ...) (qq-art stx (name@ [n] expr ...))]
      [(_ (n:id ...) expr ...) 
       (qq-art stx (@ [(name n ...)] expr ...))])))

(define-art-embedding (namespace [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (list (quasisyntax/loc stx (@ () expr ...))))])))

(define-art-realizer draw-namespace-realizer
  (λ (stx)
    (syntax-parse stx
      [(_ [(name-to-draw . drawer) ...])
       (define by-name
         (for/fold ([by-name (make-immutable-free-id-table)])
                   ([expr (current-ctxt)])
           (dict-update by-name (expr-single-name expr) (curry cons expr) (λ () '()))))
       (define drawers (make-immutable-free-id-table (map syntax->list (syntax->list #'((name-to-draw . drawer) ...)))))
       (for/fold ([im #'empty-image])
                 ([(k v) (in-dict by-name)])
         #`(above/align 'left #,im (text #,(format "~a ::=" (syntax->datum k)) 24 'blue) (realize #,(dict-ref drawers k) #,@v)))])))
