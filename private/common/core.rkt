#lang racket

(require syntax-spec rsound rsound/envelope 
         (for-syntax racket (except-in ee-lib racket-var) syntax/parse) 
         (for-meta 2 syntax/parse))

(provide (all-defined-out) (for-syntax (all-defined-out)))

(begin-for-syntax
  (struct object/s [])
  (struct coordinate/s [merger within?])
  (define current-ctxt (make-parameter '()))
  (define current-coords (make-parameter '())))

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
    #'(define-syntax r body))

  (host-interface/definitions (define-performer perf:art-performer body:expr)
    #:binding (export perf)
    #'(define-syntax perf body))

  (host-interface/expression (perform perf:art-performer e:rewriter-expr ...)
    #:with (expr ...) (compile-rewrite-exprs (syntax->list #'(e ...)) '() '())
    #'(perf expr ...)))


(define-coordinate (id [] (λ (_ r) r) (λ (_ __) #t)))

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


  ;;;;;;;;;; CONTEXT THINGS
  (define id-ctxt-prop (gensym))

  (define (ctxt->@ ctxt expr) 
    (with-syntax ([(ctxt* ...) ctxt] [expr* expr]) (qq-art expr (@ [ctxt* ...] expr*))))
  (define (get-id-ctxt stx) (syntax-property stx id-ctxt-prop))
  (define (set-id-ctxt stx ctxt) (syntax-property stx id-ctxt-prop ctxt))
  (define (context-ref ctxt name)
    (findf (λ(expr) (syntax-parse expr [(head:id _ ...) (free-identifier=? (compiled-from #'head) name)] [_ #f])) ctxt))

  ;; find an element in the context of type name, which contains the given coords
  (define (context-ref/surrounding ctxt coords name)
    (findf (λ(expr) (syntax-parse expr 
        [(head:id _ ...) 
         (and (free-identifier=? (compiled-from #'head) name)
              (within? coords (get-id-ctxt expr)))] 
        [_ #f])) 
      ctxt))


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

    (define (within? ctxt-l ctxt-r)
      (for/and ([inner-coord ctxt-l])
        (syntax-parse inner-coord
          [(head:id _ ...)
           #:when (lookup #'head object/s?)
           #t]
          [(head:id _ ...)
           #:when (lookup #'head coordinate/s?)
           ((coordinate/s-within? (lookup #'head)) inner-coord (context-ref ctxt-r (compiled-from #'head)))]))))


  ;; utility function to generate a `delete-by-id` instruction for an expr
  (define-for-syntax (delete-expr stx)
    (with-syntax [(id (syntax-parse (context-ref (get-id-ctxt stx) #'id) [({~datum id} the-id:id) #'the-id]))]
      (qq-art stx (delete-by-id id))))


  (define-for-syntax put-id (compile-reference #'id))
  (define-for-syntax (compile-rewrite-exprs exprs coords ctxt)
    (cond
      [(null? exprs) ctxt]
      [else 
        (define expr (car exprs))
        (syntax-parse expr
          [({~datum put} expr ...)
           (define coordinated
             (for/list ([expr (syntax->list #'(expr ...))])
               (set-id-ctxt
                expr
                ;; FIXME jagen gensym
                (cons #`(#,put-id #,(gensym)) coords))))

             (compile-rewrite-exprs (cdr exprs) coords (append ctxt coordinated))]
          [({~datum delete-by-id} the-id:id)
            (define ctxt*
              (filter 
                (λ(expr) 
                  (syntax-parse (context-ref (get-id-ctxt expr) #'id) 
                    [(_ the-id*:id) (not (free-identifier=? #'the-id #'the-id*))]
                    [_ #t])) ctxt))
            (compile-rewrite-exprs (cdr exprs) coords ctxt*)]
          [({~datum @} [coord ...] body ...)
           (define coords* (merge-coordinates coords (syntax->list #'(coord ...))))
           (define ctxt* (compile-rewrite-exprs (syntax->list #'(body ...)) coords* ctxt))
           (compile-rewrite-exprs (cdr exprs) coords ctxt*)]
          [({~datum pocket-rewrite} expr ...)
           (define evald (compile-rewrite-exprs (syntax->list #'(expr ...)) coords '()))
           (compile-rewrite-exprs (cdr exprs) coords (append ctxt evald))]
          [(realizer:id arg ...)
           (define realized (parameterize ([current-ctxt ctxt] [current-coords coords]) ((lookup #'realizer) expr)))
           (compile-rewrite-exprs (cons realized (cdr exprs)) coords ctxt)])]))
