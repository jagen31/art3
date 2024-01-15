#lang racket

(require syntax-spec rsound rsound/envelope
         (for-syntax racket (except-in ee-lib racket-var) syntax/parse data/gvector syntax/id-set) 
         (for-meta 2 syntax/parse))

(provide (all-defined-out) (for-syntax (all-defined-out)))

(begin-for-syntax
  (struct object/s [])
  (struct embed/s [compile])
  (struct rewriter/s [body])
  (struct realizer/s [body])
  (struct art-var/s [value])
  (struct coordinate/s [])
  (define current-ctxt (make-parameter '()))
  (define lookup-ctxt (make-parameter '()))
  
  (struct merge-rule/s [merge] #:transparent)
  (struct within?-rule/s [within?] #:transparent)

  (define merge-rules (gvector))
  (define within?-rules (gvector)))

(define-syntax (define-art-object stx)
  (syntax-parse stx
    [(_ (name:id [arg ...]))
     #'(define-syntax name (object/s))]))

(define-syntax (define-coordinate stx)
  (syntax-parse stx
    [(_ (name:id [arg ...]))
     #'(define-syntax name (coordinate/s))]))

(define-syntax (define-art-embedding stx)
  (syntax-parse stx
    [(_ (name:id [arg ...]) body)
     #'(define-syntax name (embed/s body))]))

(define-syntax (define-art-rewriter stx)
  (syntax-parse stx
    [(_ name:id body)
     #'(define-syntax name (rewriter/s body))]))

(define-syntax (define-art-realizer stx)
  (syntax-parse stx
    [(_ name:id body)
     #'(define-syntax name (realizer/s body))]))



(define-syntax (realize stx)
  (syntax-parse stx
    [(_ (perf arg ...) e ...)
     #:with (expr ...) (run-art-exprs (syntax->list #'(e ...)) '())

     (realize-art-exprs #'(perf arg ...) (syntax->list #'(expr ...)))]))

(define-for-syntax (realize-art-exprs stx exprs)
  (syntax-parse stx
    [(perf:id arg ...)
     (define real (syntax-local-value #'perf (λ () #f)))
     (unless real (raise-syntax-error 'realize (format "not a realizer") #'perf))
    
     (parameterize ([current-ctxt exprs] [lookup-ctxt exprs])
       ((realizer/s-body real) (quasisyntax/loc stx (perf arg ...))))]))

;;;;;;; reflected context.
(define-art-embedding (reflected [items])
  (λ (stx ctxt)
    (syntax-parse stx
      ;; crucially, don't rewrite.
      [(head:id expr ...) (syntax->list #'(expr ...))])))


(define-art-rewriter @
  (λ (stx)
    (syntax-parse stx
      [(_ [coord ...] expr ...) 
       (define coords* (merge-coordinates (or (get-id-ctxt stx) '()) (syntax->list #'(coord ...)) (current-ctxt)))
       (set-id-ctxt #'(context expr ...) coords*)])))

(begin-for-syntax

  (define-syntax (qq-art stx)
    (syntax-parse stx
      [(_ loc+id-ctxt expr) 
       #:with expr* #'(set-id-ctxt (quasisyntax/loc loc+id-ctxt expr) (get-id-ctxt loc+id-ctxt))
       (quasisyntax/loc stx expr*)])))

(define-syntax (define-nonhom-merge-rule stx)
  (define (do-it lname rname remove body)
     #`(define-syntax _ (gvector-add! merge-rules 
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
     #`(define-syntax _
     (gvector-add! merge-rules 
       (merge-rule/s (λ (l r ctxt) 
                       (define l* (context-ref l #'name))
                       (define r* (context-ref r #'name))
                       (if (or l* r*) (put-in-ctxt r (body l* r* l r ctxt)) r)))))]))

(define-syntax (define-hom-within?-rule stx)
  (syntax-parse stx
    [(_ name:id body)
     #'(define-syntax _ 
         (gvector-add! within?-rules 
           (within?-rule/s (λ (l r ctxt) 
                             (define l* (context-ref l #'name))
                             (define r* (context-ref r #'name))
                             (cond
                               [(and l* (not r*)) #t]
                               [(and r* (not l*)) #f]
                               [(and r* l*) (body l* r* l r ctxt)]
                               [else #t])))))]))

(define-syntax (define-nonhom-within?-rule stx)
  (syntax-parse stx
    [(_ lname:id rname:id body) 
     #`(define-syntax __
         (gvector-add! within?-rules 
           (within?-rule/s (λ (l r ctxt) 
                             (define l* (context-ref l #'lname))
                             (define r* (context-ref r #'rname))
                             (if (and l* r*) (body l* r* l r ctxt) #t)))))]))

(define-coordinate (art-id []))
;; FIXME jagen this will never run
(define-hom-merge-rule art-id (λ (l r _ __ ___) (or r (qq-art l (art-id #,(gensym))))))
(define-hom-within?-rule art-id (λ (l r _ __ ___) #t))

(define-for-syntax (expr-id stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'art-id) 
    [(_ id) #'id]
    [_ #f]))


(begin-for-syntax
  ;;;;;;;;;; CONTEXT THINGS
  (define id-ctxt-prop (gensym))

  (define (ctxt->@ ctxt expr) 
    (with-syntax ([(ctxt* ...) ctxt] [expr* expr]) (qq-art expr (@ [ctxt* ...] expr*))))
  (define (get-id-ctxt stx) (syntax-property stx id-ctxt-prop))
  (define (set-id-ctxt stx ctxt) (syntax-property stx id-ctxt-prop ctxt))
  (define (ensure-id-ctxt stx) (if (get-id-ctxt stx) stx (set-id-ctxt stx '())))
  (define (add-to-id-ctxt stx expr) (syntax-property stx id-ctxt-prop (cons expr (syntax-property stx id-ctxt-prop))))

  (define (get-context ctxt expr head)
    (define result1 (context-ref (get-id-ctxt expr) head))
    (cond
      [result1 result1]
      [else (context-ref/surrounding ctxt (get-id-ctxt expr) head)]))

  (define (require-context ctxt expr head)
    (define result (get-context ctxt expr head))
    (unless result 
      (define msg 
        (format "no ~a in context. object: ~a. candidates: ~a" 
          (syntax->datum head) (un-@ expr) (map un-@ (context-ref* (lookup-ctxt) head))))
      (raise-syntax-error 'require-context msg expr))
    result)

  (define (context-ref* ctxt name)
    (filter (λ(expr) (syntax-parse expr [(head:id _ ...) (free-identifier=? #'head name)] [_ #f])) ctxt))
  (define (context-ref ctxt name) 
    (define result (context-ref* ctxt name))
    (and (cons? result) (car result)))

  ;; find a elements of type name, which contain the given coords
  (define (context-ref*/surrounding ctxt coords name)
    (define candidates
      (filter (λ(expr) (syntax-parse expr 
        [(head:id _ ...) 
         (and (free-identifier=? #'head name)
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
         (and (free-identifier=? #'head name)
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
         #:when (free-identifier=? #'head k)
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
       #:do [(define maybe-embed (syntax-local-value #'head (λ () #f)))]
       #:when (embed/s? maybe-embed)
       (quasisyntax/loc expr (@ [#,@(get-id-ctxt expr)] (head #,@(map un-@ (syntax->list #'(inner-expr ...))))))]
      [_ (quasisyntax/loc expr (@ [#,@(get-id-ctxt expr)] #,expr))]))

  (define put-id #'art-id)
 
  (define-syntax (rewrite-in stx) 
    (syntax-parse stx [(_ ctxt expr ...) #`(run-art-exprs (list expr ...) ctxt (lookup-ctxt))]))
  (define-syntax (rewrite stx) (syntax-parse stx [(_ expr ...) #'(rewrite-in '() expr ...)]))
  (define-syntax (rewrite1 stx) 
    (syntax-parse stx [(_ expr ...) #'#`(context #,@(rewrite expr ...))]))



  (define (run-art-exprs exprs ctxt [lk-ctxt '()])
    (for/fold ([acc ctxt]) ([expr exprs]) (run-art-expr expr acc (append acc lk-ctxt))))

  (define (run-art-expr expr- ctxt [lk-ctxt '()])
    (define expr (ensure-id-ctxt expr-))
    (syntax-parse expr
      [({~datum context} inner-expr ...) 
       (run-art-exprs
         (for/list ([b (syntax->list #'(inner-expr ...))])
           (set-id-ctxt b (merge-coordinates (get-id-ctxt expr) (or (get-id-ctxt b) '()) (current-ctxt)))) 
         ctxt lk-ctxt)]
      [({~datum delete-by-id} the-id:id) (filter (λ(expr) (not (free-identifier=? #'the-id (expr-id expr)))) ctxt)]
      [({~datum replace-full-context} body ...) 
       (println "AND HERE") (run-art-exprs (map ensure-id-ctxt (syntax->list #'(body ...))) '() '())]
      [({~datum debug-realize} (perf:id arg ...))
       (displayln
         (eval-syntax
           (parameterize ([current-ctxt ctxt] [lookup-ctxt lk-ctxt])
             ((realizer/s-body (syntax-local-value #'perf)) (quasisyntax/loc expr (perf arg ...))))))
       ctxt]
      [(object:id arg ...)
       #:do [(define maybe-obj (syntax-local-value #'object (λ () #f)))]
       #:when (or (embed/s? maybe-obj) (object/s? maybe-obj))
       (define expr*
         (cond
           [(embed/s? maybe-obj)
            (with-syntax ([(new-exprs ...) 
                           (parameterize ([current-ctxt ctxt] [lookup-ctxt lk-ctxt])
                             ((embed/s-compile maybe-obj) expr ctxt))])
              (set-id-ctxt (qq-art expr (object new-exprs ...)) (get-id-ctxt expr)))]
           [else expr]))
                
       (define inner-ctxt (or (get-id-ctxt expr*) '()))
       (define maybe-id (if (context-ref inner-ctxt #'art-id) '() (list #`(#,put-id #,(gensym)))))
       (define inner-ctxt* (append maybe-id inner-ctxt))

       (define expr** (set-id-ctxt expr* inner-ctxt*))
       ;; FIXME jagen fixme
       (append ctxt (list expr**))]
      [(rewriter:id arg ...)
       #:when (rewriter/s? (syntax-local-value #'rewriter (λ () #f)))
       (displayln (format "rewriting with ~s" (syntax->datum #'rewriter)))
       (define realized (parameterize ([current-ctxt ctxt] [lookup-ctxt lk-ctxt]) ((rewriter/s-body (syntax-local-value #'rewriter)) expr)))
       (run-art-expr (ensure-id-ctxt realized) ctxt lk-ctxt)]
      [(unknown:id arg ...)
       (raise-syntax-error 'run-art-expr (format "unknown object/context/rewriter: ~a" (syntax->datum expr)) expr)]
      [id:id
       #:do [(define var (syntax-local-value #'id (λ () #f)))]
       #:fail-unless (art-var/s? var) (raise-syntax-error 'run-art-expr (format "unknown var: ~s" #'id) #'id)
       (run-art-expr (qq-art #'id (context #,@(art-var/s-value var))) ctxt lk-ctxt)])))

(begin-for-syntax
  (define defined-arts (mutable-free-id-set)))

(define-syntax (define-art stx)
  (syntax-parse stx
    [(_ name:id body ...)
       #`(begin
           (define-syntax name 
             (art-var/s (map (λ (e) (remove-from-id-ctxt (ensure-id-ctxt e) #'art-id))
                             (rewrite #'body ...))))
           (begin-for-syntax (set-add! defined-arts #'name)))]))