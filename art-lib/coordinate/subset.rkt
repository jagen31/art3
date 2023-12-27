#lang racket

(require art/private/core (for-syntax syntax/parse racket/list racket/set syntax/id-set racket/syntax))
(provide (all-defined-out) (for-syntax all-defined-out))

(define-syntax define-subset-coordinate
  (λ (stx)
    (syntax-parse stx
      [(_ subset:id @-name:id)
        #:with [copy-to find-all expr-ss]
              (list (format-id #'subset "copy-~a-to" #'subset)
                    (format-id #'subset "~a-find-all" #'subset)
                    (format-id #'subset "expr-~a" #'subset))

#'(begin
;;;;;;;;;;; SUBSET COORDINATE THINGS
(define-hom-merge-rule subset 
  (λ (l r __ ___ ____)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (syntax-parse #`(#,l #,r)
        ;; FIXME (... ...).
        [((_ item:id (... ...)) (_ item*:id (... ...)))
         #:with (result (... ...)) 
           (free-id-set->list (immutable-free-id-set (append (syntax->list #'(item (... ...))) (syntax->list #'(item* (... ...))))))
         (qq-art l (subset result (... ...)))]
        [_ 
         (error 'oops "whoops")]))))
  
(define-hom-within?-rule subset
  (λ (l r __ ___ ____)
    (let/ec break
      (unless r (break #t))
      (unless l (break #f))
      (syntax-parse #`(#,l #,r)
        [((_ item:id (... ...)) (_ item*:id (... ...)))
         (free-id-subset? (immutable-free-id-set (syntax->list #'(item* (... ...)))) (immutable-free-id-set (syntax->list #'(item (... ...)))))]))))

(define-coordinate (subset []))

(define-for-syntax (expr-ss stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'subset) 
    [(_ s (... ...)) (syntax->list #'(s (... ...)))]
    [_ '()]))

(define-art-rewriter @-name
  (λ(stx)
    (syntax-parse stx
      [(_ [item (... ...)] expr (... ...))
       (qq-art stx (@ [(subset item (... ...))] expr (... ...)))])))

(define-art-rewriter copy-to
  (λ (stx)
    (syntax-parse stx
      [(_ (ss* (... ...)))
       (define coords (syntax->list #'(ss* (... ...))))
       (define target
         (filter 
           (λ(expr) (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (current-ctxt)))
           (current-ctxt)))
       (with-syntax ([(target* (... ...))
         (for/list ([item target])
           ;; FIXME jagen preserve orthogonality?
           (put-in-id-ctxt item #'(subset ss* (... ...))))])
         #`(@ () target* (... ...)))])))

(define-for-syntax (find-all ctxt)
  (for/fold ([acc (immutable-free-id-set)]) 
            ([e ctxt]) 
    (set-union (immutable-free-id-set (expr-ss e)) acc))))])))
