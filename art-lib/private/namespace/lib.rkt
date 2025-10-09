#lang racket

(require art/base art/coordinate/name 2htdp/image  (prefix-in 2htdp: 2htdp/image)
         (for-syntax racket/string syntax/parse racket/list syntax/id-set syntax/id-table 
                     racket/dict racket/set racket/function syntax/to-string fmt))
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
                (and (not (null? (expr-name e)))
                     (equal? (syntax->datum #'(n ...)) (map syntax->datum (expr-name e)))))
              (syntax->list #'(the-items ...))))
          (unless (not (null? (syntax->list #'(result ...))))
            (raise-syntax-error 'resolve-ref "ref had no value" r))
          #'(context result ...)])])))

(define-mapping-rewriter (resolve-ref* [(: r ref)])
  (λ (stx r)
    (syntax-parse r
      [(_ n:id ...)
       #:with (the-items ...) (lookup-ctxt)
       #:with (result ...)
        (map
          (λ (x) (qq-art r (context #,(remove-from-id-ctxt x #'name))))
          (filter 
            (λ (e) 
              (and (not (null? (expr-name e)))
                   (equal? (syntax->datum #'(n ...)) (map syntax->datum (expr-name e)))))
            (syntax->list #'(the-items ...))))
        (unless (not (null? (syntax->list #'(result ...))))
          (raise-syntax-error 'resolve-ref* "ref had no value" r))
        #'(context result ...)])))

(define-mapping-rewriter (resolve-ref** [(: r ref)])
  (λ (stx r)
    (syntax-parse r
      [(_ n:id)
       #:with (the-items ...) (lookup-ctxt)
       #:with (result ...)
       (art-var/s-value (syntax-local-value (datum->syntax stx (syntax->datum #'n))))
        
       (println "RESULT !!")
       (println (map un-@ (syntax->list #'(result ...))))
       #'(context result ...)])))

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
           (define name (or name- #'<no-name>))
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

(define-for-syntax (get-art-definitions ctxt #:provide [provide #t])
  (define exprs-by-name 
    (for/fold ([exprs (make-immutable-free-id-table)])
              ([e ctxt])
      (if (cons? (expr-name e))
        (dict-update exprs (car (expr-name e))
          (λ (x) (cons (un-@ (remove-from-id-ctxt e #'name)) x)) '())
        exprs)))
    (define result
      (flatten 
        (for/list ([(k v) (in-dict exprs-by-name)])
          (cons #`(define-art #,k #,@(reverse v)) (if provide (list #`(provide #,k)) '())))))
    result)

(define-art-realizer namespace-provide-realizer
  (λ (stx) #`(begin #,@(get-art-definitions (current-ctxt)))))

(define-art-realizer namespace-define-realizer
  (λ (stx) #`(begin #,@(get-art-definitions (current-ctxt) #:provide #f))))


;; QUOTED REWRITER STUFF
;; TODO jagen31 move this?!

(define-art-object (quoted-rewrite []))

(define-drawer draw-quoted-rewrite
  (λ (stx)
    (syntax-parse stx
      [({~datum quoted-rewrite} {~and expr (head _ ...)}) 
       (define n (expr-single-name stx))
       (define img 
         (cond [(not (rewriter/s? (syntax-local-value #'head)))
                (drawer-recur (car (run-art-exprs (list #'expr) '() (current-ctxt))))]
               [else
               #`(2htdp:beside 
                   (2htdp:line 50 0 'black) 
                   (2htdp:text #,(program-format (string-trim (syntax->string #`(expr)))) 24 'blue)
                   (draw-arrow 50 'black))]))

       #`(let ()
           (define whoa #,img)
           #,(if n
                #`(2htdp:overlay whoa
                    (2htdp:overlay/align 'left 'top 
                      (2htdp:text #,(syntax->string #`(#,n)) 18 'black) 
                      (2htdp:rectangle (+ (2htdp:image-width whoa) 20) (+ (2htdp:image-height whoa) 20) 'outline 'black)))
                #'whoa))])))

(register-drawer! quoted-rewrite draw-quoted-rewrite)


(define-art-rewriter run-rewriters
  (λ (stx)
    (define rewriters (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'quoted-rewrite))

    (define results
      (for/fold ([acc (current-ctxt)])
                ([r rewriters])
        (define/syntax-parse (_ expr) r)
        (run-art-exprs (append acc (list (delete-expr r) #'expr)) '() (lookup-ctxt))))
    #`(replace-full-context #,@results #,@(map delete-expr rewriters))))