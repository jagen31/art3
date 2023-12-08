#lang racket

(require syntax-spec rsound rsound/envelope
         (for-syntax racket (except-in ee-lib racket-var) syntax/parse data/gvector) 
         (for-meta 2 syntax/parse))

(provide (all-defined-out) (for-syntax (all-defined-out)))

(begin-for-syntax
  (struct object/s [])
  (struct embed/s [compile])
  (struct rewriter/s [body])
  (struct coordinate/s [])
  (define current-ctxt (make-parameter '()))
  
  (struct merge-rule/s [merge] #:transparent)
  (struct within?-rule/s [within?] #:transparent)

  (define merge-rules (gvector))
  (define within?-rules (gvector)))

(syntax-spec
  (binding-class art-performer)
  (binding-class art-object)

  (nonterminal rewriter-expr
    (put (head:art-object e:expr ...) ...)
    (delete-by-id iden:id)
    (replace-full-context body:expr ...)
    (debug-perform (perf:art-performer arg:expr ...))
    (pocket-rewrite e:rewriter-expr ...)
    (@ [(head:art-object e:expr ...) ...] body:rewriter-expr ...)
    (r:art-object arg:expr ...))

  (host-interface/definitions (define-art-object (obj:art-object [arg:expr ...]))
    #:binding (export obj)
    #'(define-syntax obj (object/s)))

  (host-interface/definitions (define-coordinate (coord:art-object [arg:expr ...]))
    #:binding (export coord)
    #'(define-syntax coord (coordinate/s)))
  
  (host-interface/definitions (define-art-embedding (embed:art-object [arg:expr]) body:expr)
    #:binding (export embed)
    #'(define-syntax embed (embed/s body)))

  (host-interface/definitions (define-art-rewriter r:art-object body:expr)
    #:binding (export r)
    #'(define-syntax r (rewriter/s body)))

  (host-interface/definitions (define-art-realizer perf:art-performer body:expr)
    #:binding (export perf)
    #'(define-syntax perf body))

  (host-interface/expression (perform (perf:art-performer arg:expr ...) e:rewriter-expr ...)
    #:with (expr ...) (compile-rewrite-exprs (syntax->list #'(e ...)) '())
    
    (parameterize ([current-ctxt (syntax->list #'(expr ...))])
      ((lookup #'perf) #'(perf arg ...)))))


(begin-for-syntax

  ;;;;;;;; RENAMED REFERENCE THINGS
  ;; FIXME jagen consider this harder
   (define (compile-art-references stx)
     (syntax-parse stx
       [(exprs ...)
        #:with (compiled ...) (map compile-art-references (syntax->list #'(exprs ...)))
        (set-id-ctxt (quasisyntax/loc stx (compiled ...)) (get-id-ctxt stx))]
       [ref:id 
        ;; FIXME jagen how do you know if an id can be compiled?
        (with-handlers ([(λ(x) #t) (λ(x) #'ref)]) (compile-reference #'ref))]
       [_ stx]))

  (define (decompile-reference ref)
    (with-handlers ([(λ(x) #t) (λ(x) ref)]) (compiled-from ref)))
  (define (compile-reference2 ref)
    (with-handlers ([(λ(x) #t) (λ(x) ref)]) (compile-reference ref)))

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

(define-syntax (define-nonhom-merge-rule stx)
  (define (do-it lname rname remove body)
     #`(begin-for-syntax (gvector-add! merge-rules 
         (merge-rule/s (λ (l r ctxt) 
                         (define l* (context-ref l #'#,lname))
                         (define r* (context-ref r #'#,rname))
                         (if (and l* r*) (put-in-ctxt (remove-from-ctxt r #'#,remove) (#,body l* r* l r ctxt)) r))))))
  (syntax-parse stx
    [(_ lname:id rname:id #:keep-left body) (do-it #'lname #'rname #'rname #'body)]
    [(_ lname:id rname:id #:keep-right body) (do-it #'lname #'rname #'lname #'body)]))

(define-syntax (define-hom-merge-rule stx)
  (syntax-parse stx
    [(_ name:id body)
     #`(begin-for-syntax 
     (gvector-add! merge-rules 
         (merge-rule/s (λ (l r ctxt) 
                         (define l* (context-ref l #'name))
                         (define r* (context-ref r #'name))
                         (if (or l* r*) (put-in-ctxt r (body l* r* l r ctxt)) r)))))]))

(define-syntax (define-hom-within?-rule stx)
  (syntax-parse stx
    [(_ name:id body)
     #'(begin-for-syntax (gvector-add! within?-rules 
         (within?-rule/s (λ (l r ctxt) 
                           (define l* (context-ref l #'name))
                           (define r* (context-ref r #'name))
                           (if (and l* r*) (body l* r* l r ctxt) #t)))))]))

(define-syntax (define-nonhom-within?-rule stx)
  (syntax-parse stx
    [(_ lname:id rname:id body) 
     #`(begin-for-syntax (gvector-add! within?-rules 
         (within?-rule/s (λ (l r ctxt) 
                           (define l* (context-ref l #'lname))
                           (define r* (context-ref r #'rname))
                           (if (and l* r*) (body l* r* l r ctxt) #t)))))]))

(define-coordinate (art-id []))
;; FIXME jagen this will never run
(define-hom-merge-rule art-id (λ (l r _ __ ___) (or r (qq-art l (art-id #,(gensym))))))
(define-hom-within?-rule art-id (λ (l r _ __ ___) #t))

(begin-for-syntax
  ;;;;;;;;;; CONTEXT THINGS
  (define id-ctxt-prop (gensym))

  (define (ctxt->@ ctxt expr) 
    (with-syntax ([(ctxt* ...) ctxt] [expr* expr]) (qq-art expr (@ [ctxt* ...] expr*))))
  (define (get-id-ctxt stx) (syntax-property stx id-ctxt-prop))
  (define (set-id-ctxt stx ctxt) (syntax-property stx id-ctxt-prop ctxt))
  (define (ensure-id-ctxt stx) (if (get-id-ctxt stx) stx (set-id-ctxt stx '())))
  (define (add-to-id-ctxt stx expr) (syntax-property stx id-ctxt-prop (cons expr (syntax-property stx id-ctxt-prop))))

  (define (context-ref* ctxt name)
    (filter (λ(expr) (syntax-parse expr [(head:id _ ...) (free-identifier=? (decompile-reference #'head) (decompile-reference name))] [_ #f])) ctxt))
  (define (context-ref ctxt name) 
    (define result (context-ref* ctxt name))
    (and (cons? result) (car result)))

  ;; find a elements of type name, which contain the given coords
  (define (context-ref*/surrounding ctxt coords name)
    (define candidates
      (filter (λ(expr) (syntax-parse expr 
        [(head:id _ ...) 
         (and (free-identifier=? (compiled-from #'head) (decompile-reference name))
              (context-within? coords (get-id-ctxt expr) ctxt))] 
        [_ #f]))
      ctxt))
     (sort candidates (λ (l r) (context-within? (get-id-ctxt l) (get-id-ctxt r) ctxt))))

  (define (context-ref/surrounding ctxt coords name)
    (define candidates (context-ref*/surrounding ctxt coords name))
    (and (not (empty? candidates)) (car candidates)))

  ;; find elements of type name, which are contained by the given coords
  (define (context-ref*/within ctxt coords name)
    (define candidates
      (filter (λ(expr) (syntax-parse expr 
        [(head:id _ ...) 
         (and (free-identifier=? (compiled-from #'head) (decompile-reference name))
              (context-within? (get-id-ctxt expr) coords ctxt))] 
        [_ #f]))
      ctxt))
    candidates)

  (define (remove-from-ctxt ctxt k)
    (for/foldr ([acc '()]) 
               ([prop ctxt])
      (syntax-parse prop
        [(head:id _ ...)
        ;; FIXME jagen yeesh, figure out if k should be compiled or not or both
         #:when (free-identifier=? (compiled-from #'head) (decompile-reference k))
         acc]
        [_ (cons prop acc)])))

  (define (remove-from-id-ctxt stx k) (set-id-ctxt stx (remove-from-ctxt (get-id-ctxt stx) k)))

  (define (put-in-ctxt ctxt expr)
    (syntax-parse expr
      [(head:id _ ...) (cons expr (remove-from-ctxt ctxt #'head))]))
  (define (put-in-id-ctxt stx expr) (set-id-ctxt stx (put-in-ctxt (get-id-ctxt stx) (qq-art expr #,expr))))


  ;;;;;;;;;; COORDINATE THINGS
  (define (merge-coordinates left right ctxt)
    (for/fold ([acc right]) ([merge-rule merge-rules]) ((merge-rule/s-merge merge-rule) left acc ctxt)))

  (define (context-within? ctxt-l ctxt-r ctxt)
    (for/and ([rule within?-rules]) ((within?-rule/s-within? rule) ctxt-l ctxt-r ctxt)))


  ;; utility function to generate a `delete-by-id` instruction for an expr
  (define (delete-expr stx)
    (with-syntax ([id (syntax-parse (context-ref (get-id-ctxt stx) #'art-id) [({~datum art-id} the-id:id) #'the-id])])
      (qq-art stx (delete-by-id id))))

  (define (un-@ expr) 
    (syntax-parse expr
      [({~datum @} coord inner-expr ...)
       (quasisyntax/loc expr (@ coord #,@(map un-@ (syntax->list #'(inner-expr ...)))))]
      [(head:id inner-expr ...)
       #:when (lookup #'head embed/s?)
       (quasisyntax/loc expr (@ [#,@(get-id-ctxt expr)] (head #,@(map un-@ (syntax->list #'(inner-expr ...))))))]
      [_ (quasisyntax/loc expr (@ [#,@(get-id-ctxt expr)] #,expr))]))

  (define put-id (compile-reference #'art-id))

  (define (compile-rewrite-exprs exprs ctxt)
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
                    #:fail-unless (or (object/s? it) (embed/s it)) (raise-syntax-error 'compile-rewrite-exprs (format "unrecognized object: ~a" (syntax->datum inner-expr)) inner-expr)
                    (void)])

                 (define inner-expr* 
                   (syntax-parse inner-expr
                     [(embedding:id arg ...)
                      #:when (lookup #'embedding embed/s?)
                      #:with (new-exprs ...) ((embed/s-compile (lookup #'embedding)) inner-expr ctxt)
                      (set-id-ctxt #'(embedding new-exprs ...) (get-id-ctxt inner-expr))]
                     [_ inner-expr]))
                     
                 (define inner-ctxt (or (get-id-ctxt inner-expr*) '()))
                 (define maybe-id (if (context-ref inner-ctxt #'art-id) '() (list #`(#,put-id #,(gensym)))))
                 (define inner-ctxt* (append maybe-id inner-ctxt))
                 (define ctxt* (merge-coordinates (or (get-id-ctxt expr) '()) inner-ctxt* ctxt))

                 (set-id-ctxt inner-expr* ctxt*)))

               (compile-rewrite-exprs (cdr exprs) (append ctxt coordinated))]
            [({~datum delete-by-id} the-id:id)
              (define ctxt*
                (filter 
                  (λ(expr) 
                    (syntax-parse (context-ref (get-id-ctxt expr) #'art-id) 
                      [(_ the-id*:id) 
                       (not (free-identifier=? #'the-id #'the-id*))]
                      [_ #t])) 
                    ctxt))
              (compile-rewrite-exprs (cdr exprs) ctxt*)]
            [({~datum replace-full-context} body ...)
             (compile-rewrite-exprs (cdr exprs) (syntax->list #'(body ...)))]
            [({~datum @} [coord ...] body ...)
             (define coords* (merge-coordinates (or (get-id-ctxt expr) '()) (syntax->list #'(coord ...)) ctxt))
             (define coordinated 
               (for/list ([b (syntax->list #'(body ...))])
                 (set-id-ctxt b (merge-coordinates coords* (or (get-id-ctxt b) '()) ctxt))))
             (define ctxt* (compile-rewrite-exprs coordinated ctxt))
             (compile-rewrite-exprs (cdr exprs) ctxt*)]
            [({~datum debug-perform} perf:id)
             (displayln ((lookup #'perf) #`(perf #,@ctxt)))
             (compile-rewrite-exprs (cdr exprs) ctxt)]
            [({~datum pocket-rewrite} inner-expr ...)
             (define coordinated
               (for/list ([inner-expr (syntax->list #'(inner-expr ...))])
                 (define inner-ctxt (or (get-id-ctxt inner-expr) '()))
                 (define ctxt* (merge-coordinates (or (get-id-ctxt expr) '()) inner-ctxt ctxt))
                 (set-id-ctxt inner-expr ctxt*)))
             (define evald (compile-rewrite-exprs coordinated '()))
             (compile-rewrite-exprs (cdr exprs) (append ctxt evald))]
            [(object:id arg ...)
             #:when (or (lookup (compile-reference2 #'object) object/s?) (lookup (compile-reference2 #'object) embed/s?))
             #:with expr* expr
             (compile-rewrite-exprs (cons #'(put expr*) (cdr exprs)) ctxt)]
            [(realizer:id arg ...)
             #:when (lookup (compile-reference2 #'realizer) rewriter/s?)
             (displayln (format "rewriting with ~s" (syntax->datum #'realizer)))
             (define realized (parameterize ([current-ctxt ctxt]) ((rewriter/s-body (lookup (compile-reference2 #'realizer))) expr)))
             (compile-rewrite-exprs (cons realized (cdr exprs)) ctxt)]
            [(unknown:id arg ...)
             (println (lookup #'unknown))
             (raise-syntax-error 'compile-rewrite-exprs (format "unknown rewriter: ~a" (syntax->datum expr)) expr)])]))
  (compile-rewrite-exprs (map ensure-id-ctxt exprs) (map ensure-id-ctxt ctxt))))
