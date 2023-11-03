#lang racket

(require syntax-spec rsound rsound/envelope 
         (for-syntax racket (except-in ee-lib racket-var) syntax/parse) 
         (for-meta 2 syntax/parse))

(provide (all-defined-out) (for-syntax (all-defined-out)))

(begin-for-syntax
  (struct object/s [])
  (struct rewriter/s [body])
  (struct coordinate/s [merger within?])
  (define current-ctxt (make-parameter '())))

(syntax-spec
  (binding-class art-performer)
  (binding-class art-object)
  (binding-class art-id)

  (binding-class rewriter)

  (nonterminal rewriter-object-expr
    ;; FIXME jagen
    (o:art-object arg:expr ...))

  (nonterminal rewriter-expr
    (put e:rewriter-object-expr ...)
    (delete-by-id iden:id)
    (pocket-rewrite e:rewriter-expr ...)
    (@ [e:rewriter-object-expr ...] body:rewriter-expr ...)
    (r:rewriter arg:expr ...))

  (host-interface/definitions (define-art-object (obj:art-object [arg:expr ...]))
    #:binding (export obj)
    #'(define-syntax obj (object/s)))

  (host-interface/definitions (define-coordinate (coord:art-object [arg:expr ...] merge:expr within?:expr))
    #:binding (export coord)
    #'(define-syntax coord (coordinate/s merge within?)))

  (host-interface/definitions (define-rewriter r:rewriter body:expr)
    #:binding (export r)
    #'(define-syntax r (rewriter/s body)))

  (host-interface/definitions (define-performer perf:art-performer body:expr)
    #:binding (export perf)
    #'(define-syntax perf body))

  (host-interface/expression (perform perf:art-performer e:rewriter-expr ...)
    #:with (expr ...) (compile-rewrite-exprs (syntax->list #'(e ...)) '())
    #'(perf expr ...)))


(begin-for-syntax

   ;;;;;;;; RENAMED REFERENCE THINGS
   ;; FIXME jagen consider this harder
    (define (compile-art-references stx)
      (syntax-parse stx
        [(exprs ...)
         #:with (compiled ...) (map compile-art-references (syntax->list #'(exprs ...)))
         (quasisyntax/loc stx (compiled ...))]
        [ref:id 
         ;; FIXME jagen how do you know if an id can be compiled?
         (with-handlers ([(λ(x) #t) (λ(x) #'ref)]) (compile-reference #'ref))]
        [_ stx]))

  (define (decompile-reference ref)
    (with-handlers ([(λ(x) #t) (λ(x) ref)]) (compiled-from ref)))

  (define-syntax (qq-art stx)
    (syntax-parse stx
      [(_ loc+id-ctxt expr) 
       #:with compiled #'(quasisyntax/loc loc+id-ctxt #,(compile-art-references #`expr))
       (quasisyntax/loc this-syntax (set-id-ctxt compiled (get-id-ctxt loc+id-ctxt)))]))

  (define-syntax (qq-art/no-context stx)
    (syntax-parse stx
      [(_ loc+id-ctxt expr) 
       #:with compiled #'(quasisyntax/loc loc+id-ctxt #,(compile-art-references #`expr))
       (quasisyntax/loc this-syntax (set-id-ctxt compiled '()))])))


(define-coordinate (id [] (λ (l r) (or r (qq-art l (id #,(gensym))))) (λ (_ __) #t)))

(begin-for-syntax
  ;;;;;;;;;; CONTEXT THINGS
  (define id-ctxt-prop (gensym))

  (define (ctxt->@ ctxt expr) 
    (with-syntax ([(ctxt* ...) ctxt] [expr* expr]) (qq-art expr (@ [ctxt* ...] expr*))))
  (define (get-id-ctxt stx) (syntax-property stx id-ctxt-prop))
  (define (set-id-ctxt stx ctxt) (syntax-property stx id-ctxt-prop ctxt))
  (define (add-to-id-ctxt stx expr) (syntax-property stx id-ctxt-prop (cons expr (syntax-property stx id-ctxt-prop))))

  (define (context-ref* ctxt name)
    (filter (λ(expr) (syntax-parse expr [(head:id _ ...) (free-identifier=? (compiled-from #'head) name)] [_ #f])) ctxt))
  (define (context-ref ctxt name) (define result (context-ref* ctxt name))
    (and (cons? result) (car result)))

  ;; find an element in the context of type name, which contains the given coords
  (define (context-ref/surrounding ctxt coords name)
    (define candidates
      (filter (λ(expr) (syntax-parse expr 
        [(head:id _ ...) 
         (and (free-identifier=? (compiled-from #'head) name)
              (context-within? coords (get-id-ctxt expr)))] 
        [_ #f]))
      ctxt))
    (and (not (empty? candidates)) (car (sort candidates within?))))

  (define (remove-from-id-ctxt stx k) (syntax-property stx id-ctxt-prop 
    (for/foldr ([acc '()]) 
               ([prop (get-id-ctxt stx)])
      (syntax-parse prop
        [(head:id _ ...)
         #:when (free-identifier=? (compiled-from #'head) k)
         acc]
        [_ (cons prop acc)]))))

  (define (put-in-id-ctxt stx k v) (add-to-id-ctxt (remove-from-id-ctxt stx k) (qq-art k (#,k #,@v))))


  ;;;;;;;;;; COORDINATE THINGS
  (define (merge-coordinates left right)
    (define left-coords (for/list ([expr left])
      (syntax-parse expr
        [(head:id _ ...)
         (define merger (coordinate/s-merger (lookup #'head)))
         (merger expr (context-ref right (compiled-from #'head)))])))

    
    (define coords 
      (for/foldr ([acc left-coords])
                 ([expr right])
        (syntax-parse expr
          [(head:id _ ...)
           #:when (not (context-ref left-coords (compiled-from #'head)))
           (define merger (coordinate/s-merger (lookup #'head)))
           (cons (merger (context-ref left-coords (compiled-from #'head)) expr) acc)]
          [_ acc])))
    coords)

    ;; FIXME jagen yikes
    (define (within? l r)
      (syntax-parse l
        [(head:id _ ...)
         #:when (lookup #'head object/s?)
           #t]
        [(head:id _ ...)
         #:when (lookup #'head coordinate/s?)
         ((coordinate/s-within? (lookup #'head)) l r)]))

    (define (context-within? ctxt-l ctxt-r)
      (for/and ([inner-coord ctxt-l])
        (syntax-parse inner-coord
          [(head:id _ ...)
           (within? inner-coord (context-ref ctxt-r (compiled-from #'head)))])))


  ;; utility function to generate a `delete-by-id` instruction for an expr
  (define (delete-expr stx)
    (with-syntax ([id (syntax-parse (context-ref (get-id-ctxt stx) #'id) [({~datum id} the-id:id) #'the-id])])
      (qq-art stx (delete-by-id id))))



  (define (un-@ expr) #`(@ [#,@(get-id-ctxt expr)] #,expr))


  (define put-id (compile-reference #'id))
  (define (compile-rewrite-exprs exprs ctxt)
    (cond
      [(null? exprs) ctxt]
      [else 
        (define expr (car exprs))
        (syntax-parse expr
          [({~datum put} inner-expr ...)
           (define coordinated
             (for/list ([inner-expr (syntax->list #'(inner-expr ...))])
               (syntax-parse inner-expr
                 [(head:id arg ...)
                  #:do [(define it (lookup #'head))]
                  #:fail-unless (object/s? it) (raise-syntax-error 'compile-rewrite-exprs (format "unrecognized object: ~a" (syntax->datum inner-expr)) inner-expr)
                  (void)])
               (define inner-ctxt (or (get-id-ctxt inner-expr) '()))
               (define maybe-id (if (context-ref inner-ctxt #'id) '() (list #`(#,put-id #,(gensym)))))
               (define inner-ctxt* (append maybe-id inner-ctxt))
               (define ctxt* (merge-coordinates (or (get-id-ctxt expr) '()) inner-ctxt*))
               (set-id-ctxt
                inner-expr
                ;; FIXME jagen gensym
                (append  ctxt*))))

             (compile-rewrite-exprs (cdr exprs) (append ctxt coordinated))]
          [({~datum delete-by-id} the-id:id)
            (define ctxt*
              (filter 
                (λ(expr) 
                  (syntax-parse (context-ref (get-id-ctxt expr) #'id) 
                    [(_ the-id*:id) (not (free-identifier=? #'the-id #'the-id*))]
                    [_ #t])) 
                  ctxt))
            (compile-rewrite-exprs (cdr exprs) ctxt*)]
          [({~datum @} [coord ...] body ...)
           (define coords* (merge-coordinates (or (get-id-ctxt expr) '()) (syntax->list #'(coord ...))))
           (define coordinated 
             (for/list ([b (syntax->list #'(body ...))])
               (set-id-ctxt b (merge-coordinates coords* (or (get-id-ctxt b) '())))))
           (define ctxt* (compile-rewrite-exprs coordinated ctxt))
           (compile-rewrite-exprs (cdr exprs) ctxt*)]
          [({~datum pocket-rewrite} expr ...)
           (define evald (compile-rewrite-exprs (syntax->list #'(expr ...)) '()))
           (compile-rewrite-exprs (cdr exprs) (append ctxt evald))]
          [(object:id arg ...)
           #:when (lookup #'object object/s?)
           #:with expr* expr
           (compile-rewrite-exprs (cons #'(put expr*) (cdr exprs)) ctxt)]
          [(realizer:id arg ...)
           #:when (lookup #'realizer rewriter/s?)
           (define realized (parameterize ([current-ctxt ctxt]) ((rewriter/s-body (lookup #'realizer)) expr)))
           (compile-rewrite-exprs (cons realized (cdr exprs)) ctxt)]
          [(unknown:id arg ...)
           (raise-syntax-error 'compile-rewrite-exprs (format "unknown rewriter: ~a" (syntax->datum expr)) expr)])])))
