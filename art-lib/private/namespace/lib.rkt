#lang racket

(require art/base art/coordinate/name 2htdp/image 
         (for-syntax racket/string syntax/parse racket/list syntax/id-set syntax/id-table 
                     racket/dict racket/set racket/function))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-name-coordinate name)

(define-art-rewriter name@
  (λ (stx)
    (syntax-parse stx
      [(_ n:id expr ...) (qq-art stx (name@ [n] expr ...))]
      [(_ (n:id ...) expr ...) (qq-art stx (@ [(name n ...)] expr ...))])))

(define-art-embedding (namespace [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-mapping-rewriter (rewrite-in-namespace [(: s namespace)])
  (λ (stx s)
    (syntax-parse stx
      [(_ expr ...)
       (syntax-parse s
         [(_ expr* ...)
           #:with (result ...) 
             (rewrite-in (syntax->list #'(expr* ...)) #'(context expr ...))
           #`(context #,(qq-art s (namespace result ...)))])])))

(define-art-object (ref []))

(define-mapping-rewriter (resolve-ref [(: r ref)])
  (λ (stx r)
    (syntax-parse r
      [(_ n:id ...)
       (define items (require-context (lookup-ctxt) r #'namespace))
       (syntax-parse items
         [(_ the-items ...)
          #:with (result ...)
          (map
            (λ (x) (remove-from-id-ctxt x #'name))
            (filter 
              (λ (e) 
                (println (map syntax->datum (expr-name e)))
                (println (syntax->datum #'(n ...)))
                (and (not (null? (expr-name e)))
                     (equal? (syntax->datum #'(n ...)) (map syntax->datum (expr-name e)))))
              (syntax->list #'(the-items ...))))
          (unless (not (null? (syntax->list #'(result ...))))
            (raise-syntax-error 'resolve-ref "ref had no value" r))
          #'(context result ...)])])))

(define-art-rewriter ref*
  (λ (stx)
    (syntax-parse stx
      [(_ expr) 
       #:with (result ...) (rewrite (qq-art stx (context (ref expr) (resolve-ref))))
      (qq-art stx (context result ...))])))
  
(define-drawer draw-namespace
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (define by-name
         (for/fold ([by-name (make-immutable-free-id-table)])
                   ([expr (syntax->list #'(expr ...))])
           (define name- (expr-single-name expr))
           ;; FIXME jagen yuck. make a better model.
           (define name (if (free-identifier=? name- #'||) #'<no-name> name-))
           (dict-update by-name name (curry cons expr) (λ () '()))))
       (for/fold ([im #'empty-image])
                 ([(k v) (in-dict by-name)])
         #`(above/align 'left 
             #,im 
             (text #,(format "~a ::=" (syntax->datum k)) 24 'blue) 
             (above #,@(map drawer-recur v) empty-image empty-image)))])))

(register-drawer! namespace draw-namespace)

(define-art-rewriter reify-art-definitions
  (λ (stx)
    (define result
      (for/list ([id (in-set defined-arts)])
        #`(name@ (#,id) #,id)))
    (qq-art stx (context #,@result))))

(define-art-realizer namespace-provide-realizer
  (λ (stx)
    (define exprs-by-name 
      (for/fold ([exprs (make-immutable-free-id-table)])
                ([e (current-ctxt)])
        (if (cons? (expr-name e))
          (dict-update exprs (expr-single-name e) 
            (λ (x) (cons (un-@ (remove-from-id-ctxt e #'name)) x)) '())
          exprs)))
    (define result
      (flatten 
        (for/list ([(k v) (in-dict exprs-by-name)])
          (list #`(define-art #,k #,@(reverse v)) #`(provide #,k)))))
    #`(begin #,@result)))

#;(define-art-realizer namespace-provide-realizer
  (λ (stx)
    (define exprs-by-name 
      (for/fold ([exprs (make-immutable-free-id-table)])
                ([e (current-ctxt)])
        (if (cons? (expr-name e))
          (dict-update exprs (expr-single-name e) 
            (λ (x) (cons (un-@ (remove-from-id-ctxt e #'name)) x)) '())
          exprs)))
    (define result
      (flatten 
        (for/list ([(k v) (in-dict exprs-by-name)])
          (list #`(define-art #,k #,@(reverse v)) #`(provide #,k)))))
    #`(begin #,@result)))