#lang racket

(require art art/coordinate/name 2htdp/image 
         (for-syntax racket/string syntax/parse racket/list syntax/id-set syntax/id-table 
                     racket/dict racket/set racket/function))
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
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-mapping-rewriter (rewrite-in-namespace [(: s namespace)])
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
             #`(context #,(qq-art s (namespace result ...)))])]
       ;; FIXME jagen
       [(head expr ...) (go #'(head expr ... #:capture []) s)]))
    go))

(define-art-object (ref []))

(define-mapping-rewriter (resolve-ref [(: r ref)])
  (λ (stx r)
    (syntax-parse r
      [(_ n:id)
                        
       (qq-art r
         (context
           #,@(map
                (λ (x) (remove-from-id-ctxt x #'name))
                (filter (λ (e) 
                  (and (not (null? (expr-name e)))
                       (context-within? (put-in-ctxt (get-id-ctxt r) #'(name n)) (get-id-ctxt e) (current-ctxt))))
                  (current-ctxt)))))])))
  
(define-drawer draw-namespace
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (define by-name
         (for/fold ([by-name (make-immutable-free-id-table)])
                   ([expr (syntax->list #'(expr ...))])
           (define name- (expr-single-name expr))
           (define name (if (free-identifier=? name- #'||) #'<no-name> name-))
           (dict-update by-name name (curry cons expr) (λ () '()))))
       (for/fold ([im #'empty-image])
                 ([(k v) (in-dict by-name)])
         #`(above/align 'left #,im (text #,(format "~a ::=" (syntax->datum k)) 24 'blue) (above #,@(map drawer-recur v) empty-image)))])))

(register-drawer! namespace draw-namespace)

(define-art-rewriter reflect-art-definitions
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
