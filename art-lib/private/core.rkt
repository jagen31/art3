#lang racket

(require (for-syntax racket syntax/parse data/gvector syntax/id-set) 
         (for-meta 2 syntax/parse))

(provide (all-defined-out) (for-syntax (all-defined-out)))


;; art objects- represents anything, really
(begin-for-syntax (struct object/s []))

(define-syntax (define-art-object stx)
  (syntax-parse stx
    [(_ (name:id [arg ...]))
     #'(define-syntax name (object/s))]))

;;;;;;;;;;;;;;;;;;;;;;;




;; coordinates- a special kind of object that typically goes in identity contexts of other objects
;; (in fact, at the time of writing, most tooling will not handle coordinates outside of identity 
;; contexts).  they denote the location of the object.  A relation `within?` is basically required 
;; and indicates whether one object is contained by another.  Other relations can be defined, 
;; `within?` is just the widely supported one by libraries (for example, `intersection` which pairs 
;; two coordinates with their intersection)
(begin-for-syntax (struct coordinate/s object/s []))

(define-syntax (define-coordinate stx)
  (syntax-parse stx
    [(_ (name:id [arg ...]))
     #'(define-syntax name (coordinate/s))]))

(begin-for-syntax

  ;; a rule to be applied to merge a pair of coordinates
  (struct merge-rule/s [merge])
  ;; all the rules
  (define merge-rules (gvector))
  ;; run the rules
  (define (merge-coordinates left right ctxt)
    (for/fold ([acc right]) ([merge-rule merge-rules]) ((merge-rule/s-merge merge-rule) left acc ctxt)))

  ;; a rule to be applied to determine if one coordinate is within another
  (struct within?-rule/s [within?])
  ;; all the rules
  (define within?-rules (gvector))
  ;; run the rules
  (define (context-within? ctxt-l ctxt-r ctxt)
    (for/and ([rule within?-rules]) ((within?-rule/s-within? rule) ctxt-l ctxt-r ctxt))))

;;;;;;;;;;;;;;;;;;






;; rewriters- special objects which are used to transform art.
(begin-for-syntax (struct rewriter/s object/s [body]))

(define-syntax (define-art-rewriter stx)
  (syntax-parse stx
    [(_ name:id body)
     #'(define-syntax name (rewriter/s body))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; embeddings- a special kind of object which is really a subcontext containing its own rewriting 
;; procedures and set of coordinates.
;; compile : Syntax -> Syntax
(begin-for-syntax (struct embed/s [compile]))

(define-syntax (define-art-embedding stx)
  (syntax-parse stx
    [(_ (name:id [arg ...]) body)
     #'(define-syntax name (embed/s body))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;; contexts which are used for rewriting art.  current context is everything we've seen up to 
;;;;;;;;;; the most recent embedding.  lookup context is everything we've seen, period.  Generally 
;;;;;;;;;; speaking- context for rewrites penetrates the embedding barriers (that's why it's called
;;;;;;;;;; "lookup context", you use it for lookups), but rewrites only modify objects
;;;;;;;;;; inside the embedding barrier (so only delete/modify within the
;;;;;;;;;; current context, leave the lookup context alone). One extra note is
;;;;;;;;;; the lookup context always contains the current context.
(begin-for-syntax
  (define current-ctxt (make-parameter '()))
  (define lookup-ctxt (make-parameter '())))
  





;; art variables - just an easy interface for decomposing arts which interacts with racket modules 
;; nicely.  not as fundamental as it sounds
(begin-for-syntax
  (struct art-var/s [value])
  (define defined-arts (mutable-free-id-set)))

(define-syntax (define-art stx)
  (syntax-parse stx
    [(_ name:id body ...)
       #`(begin
           (define-syntax name 
             (art-var/s (map (λ (e) (ensure-id-ctxt e))
                             (rewrite #'body ...))))
           (begin-for-syntax (set-add! defined-arts #'name)))]))

;;;;;;;;;;;;;;;;;;;;;;





;; realizers- for compiling a rewritten art into racket
(begin-for-syntax (struct realizer/s [body]))

(define-syntax (define-art-realizer stx)
  (syntax-parse stx
    [(_ name:id body)
     #'(define-syntax name (realizer/s body))]))












;;;; THE MOST IMPORTANT UTILITIES (almost fundamental)

;; quasiquote to inject into art (really just copy the identity context from the provided object, in 
;; addition to the source location)
(begin-for-syntax
  (define-syntax (qq-art stx)
    (syntax-parse stx
      [(_ loc+id-ctxt expr) 
       #:with expr* #'(set-id-ctxt (quasisyntax/loc loc+id-ctxt expr) (get-id-ctxt loc+id-ctxt))
       (quasisyntax/loc stx expr*)])))

;; important rewriter , introduces coordinates to identity contexts
(define-art-rewriter @
  (λ (stx)
    (syntax-parse stx
      [(_ [coord ...] expr ...) 
       (define coords* (merge-coordinates (or (get-id-ctxt stx) '()) (syntax->list #'(coord ...)) (current-ctxt)))
       (set-id-ctxt #'(context expr ...) coords*)])))

(begin-for-syntax
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
      [_ (quasisyntax/loc expr (@ [#,@(get-id-ctxt expr)] #,expr))])))
 

;; define a merge rule for one coord
(define-syntax (define-hom-merge-rule stx)
  (syntax-parse stx
    [(_ name:id body)
     #`(define-syntax _
     (gvector-add! merge-rules 
       (merge-rule/s (λ (l r ctxt) 
                       (define l* (context-ref l #'name))
                       (define r* (context-ref r #'name))
                       (if (or l* r*) (put-in-ctxt r (body l* r* l r ctxt)) r)))))]))

;; define a merge rule between two different coords
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

;; define a within? rule for one coord
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

;; define a within? rule for two different coords
(define-syntax (define-nonhom-within?-rule stx)
  (syntax-parse stx
    [(_ lname:id rname:id body) 
     #`(define-syntax __
         (gvector-add! within?-rules 
           (within?-rule/s (λ (l r ctxt) 
                             (define l* (context-ref l #'lname))
                             (define r* (context-ref r #'rname))
                             (if (and l* r*) (body l* r* l r ctxt) #t)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;; CONTEXT THINGS- useful utilities for managing contexts
(begin-for-syntax

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
        [_ (cons prop acc)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;;;;;;;; IDENTITY CONTEXT THINGS- useful utilities for managing identity contexts (the context
;;;;;;;;;; attached to each art expr)
(begin-for-syntax
  (define id-ctxt-prop (gensym))
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


  (define (remove-from-id-ctxt stx k) (set-id-ctxt stx (remove-from-ctxt (get-id-ctxt stx) k)))

  (define (put-in-ctxt ctxt expr)
    (syntax-parse expr
      [(head:id _ ...) (cons expr (remove-from-ctxt ctxt #'head))]))
  (define (put-in-id-ctxt stx expr) (set-id-ctxt stx (put-in-ctxt (get-id-ctxt stx) (qq-art expr #,expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;; art-id special coordinate- each object gets an id

(define-coordinate (art-id []))
;; FIXME jagen this will never run
(define-hom-merge-rule art-id (λ (l r _ __ ___) (or r (qq-art l (art-id #,(gensym))))))
(define-hom-within?-rule art-id (λ (l r _ __ ___) #t))

(define-for-syntax (expr-id stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'art-id) 
    [(_ id) #'id]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;





;; the rewriter
(begin-for-syntax
  (define (run-art-exprs exprs ctxt [lk-ctxt '()])
    (for/fold ([acc ctxt]) ([expr exprs]) (run-art-expr expr acc (append acc lk-ctxt))))

  (define (run-art-expr expr- ctxt [lk-ctxt '()])
    (define expr (ensure-id-ctxt expr-))
    (syntax-parse expr
      [({~datum replace-full-context} body ...) (run-art-exprs (syntax->list #'(body ...)) '() '())]
      [({~datum context} inner-expr ...) 
       (run-art-exprs
         (for/list ([b (syntax->list #'(inner-expr ...))])
           (set-id-ctxt b (merge-coordinates (get-id-ctxt expr) (or (get-id-ctxt b) '()) (current-ctxt)))) 
         ctxt lk-ctxt)]
      [({~datum delete-by-id} the-id:id) 
       (filter (λ (expr) (not (free-identifier=? #'the-id (expr-id expr)))) ctxt)]
       [(rewriter:id arg ...)
       #:when (rewriter/s? (syntax-local-value #'rewriter (λ () #f)))
       #;(displayln (format "rewriting with ~s" (syntax->datum #'rewriter)))
       (define realized (parameterize ([current-ctxt ctxt] [lookup-ctxt lk-ctxt]) 
         ((rewriter/s-body (syntax-local-value #'rewriter)) (put-in-id-ctxt expr #`(art-id #,(gensym))))))
       (run-art-expr (ensure-id-ctxt realized) ctxt lk-ctxt)]
      [(object:id arg ...)
       #:do [(define maybe-obj (syntax-local-value #'object (λ () #f)))]
       #:when (or (embed/s? maybe-obj) (object/s? maybe-obj) (coordinate/s? maybe-obj))
       (define expr*
         (cond
           [(embed/s? maybe-obj)
            (with-syntax ([(new-exprs ...) 
                           (parameterize ([current-ctxt ctxt] [lookup-ctxt lk-ctxt])
                             ((embed/s-compile maybe-obj) expr ctxt))])
              (set-id-ctxt (qq-art expr (object new-exprs ...)) (get-id-ctxt expr)))]
           [else expr]))

       (define inner-ctxt (or (get-id-ctxt expr*) '()))
       (define maybe-id (if (context-ref inner-ctxt #'art-id) '() (list #`(art-id #,(gensym)))))
       (define inner-ctxt* (append maybe-id inner-ctxt))

       (define expr** (set-id-ctxt expr* inner-ctxt*))
       ;; FIXME jagen fixme
       (append ctxt (list expr**))]
      
      [(unknown:id arg ...)
       (raise-syntax-error 'run-art-expr 
         (format "unknown object/context/rewriter: ~a" (syntax->datum expr)) expr)]
      [id:id
       #:do [(define var (syntax-local-value #'id (λ () #f)))]
       #:fail-unless (art-var/s? var) (raise-syntax-error 'run-art-expr (format "unknown var: ~s" #'id) #'id)
       (run-art-expr (qq-art #'id (context #,@(art-var/s-value var))) ctxt lk-ctxt)]))

  ;; shorthands for running the rewriter
  (define-syntax (rewrite-in stx) 
    (syntax-parse stx [(_ ctxt expr ...) #`(run-art-exprs (list expr ...) ctxt (append ctxt (lookup-ctxt)))]))
  (define-syntax (rewrite stx) (syntax-parse stx [(_ expr ...) #'(rewrite-in '() expr ...)]))
  (define-syntax (rewrite1 stx) 
    (syntax-parse stx [(_ expr ...) #'#`(context #,@(rewrite expr ...))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; the realizer
(define-for-syntax (realize-art-exprs stx exprs)
  (syntax-parse stx
    [(perf:id arg ...)
     (define real (syntax-local-value #'perf (λ () #f)))
     (unless real (raise-syntax-error 'realize (format "not a realizer") #'perf))
 
     (parameterize ([current-ctxt exprs] [lookup-ctxt exprs])
       ((realizer/s-body real) (quasisyntax/loc stx (perf arg ...))))]))

;; invoke the realizer
(define-syntax (realize stx)
  (syntax-parse stx
    [(_ (perf arg ...) e ...)
     #:with (expr ...) (run-art-exprs (syntax->list #'(e ...)) '())

     (realize-art-exprs #'(perf arg ...) (syntax->list #'(expr ...)))]))
