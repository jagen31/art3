#lang racket

(require art/private/core (for-syntax syntax/parse racket/list racket/syntax))
(provide (all-defined-out) (for-syntax (all-defined-out)))

;;;;;;;;;;; INDEX COORDINATE CLASS

(define-syntax define-index-coordinate
  (λ (stx)
    (syntax-parse stx
      [(_ index:id)
       #:with [do-merge-index do-index-within? expr-single-index expr-index max-index 
               get-index-range max-index* context-ref/index get-index-axis zero-index]
              (list (format-id #'index "do-merge-~a" #'index) (format-id #'index"do-~a-within?" #'index) 
                    (format-id #'index "expr-single-~a" #'index) (format-id #'index "expr-~a" #'index)
                    (format-id #'index "max-~a" #'index) (format-id #'index "get-~a-range" #'index)
                    (format-id #'index "max-~a*" #'index) (format-id #'index "context-ref/~a" #'index)
                    (format-id #'index "get-~a-axis" #'index) (format-id #'index "zero-~a" #'index))
                    

#'(begin
;;;;;;;;;;; INDEX COORDINATE THINGS
(define-hom-merge-rule index
  (λ (l r _ __ ___)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (syntax-parse #`(#,l #,r)
        [((_ ixl:number (... ...)) (__ ixr:number (... ...)))
         (qq-art r (index ixl (... ...) ixr (... ...)))]))))
  
(define-hom-within?-rule index (λ (l r _ __ ___)
  (syntax-parse #`(#,l #,r)
    [(({~datum index} lix (... ...)) ({~datum index} rix (... ...)))
     (equal? (syntax->datum #'(lix (... ...))) (syntax->datum #'(rix (... ...))))])))

(define-coordinate (index [val]))

(define-for-syntax (expr-single-index stx)
  (define indices (expr-index stx))
  (if (and (not (empty? indices)) (null? (cdr indices)))
    (car indices)
    (raise-syntax-error 'expr-single-index (format "expr must be rank 1, got index: ~s" indices) stx)))

(define-for-syntax (expr-index stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'index) 
    [(_ ix:number (... ...)) (syntax->datum #'(ix (... ...)))]
    [_ '()]))

(define-for-syntax (max-index l r)
  (let/ec return
    (for ([li l] [ri r])
      (when (> li ri) (return l))
      (when (< li ri) (return r)))
    l))

(define-for-syntax (max-index* is) (map add1 (foldl max-index (zero-index (length is)) is)))
  
(define-for-syntax (get-index-range max-ix)
  (cond
   [(null? max-ix) '(())]
   [(null? (cdr max-ix)) (map list (range (car max-ix)))]
   [else 
    (define each (range (car max-ix)))
    (define rest (get-index-range (cdr max-ix)))
    (for/foldr ([acc '()]) 
               ([i each])
     (append (map (λ (l) (cons i l)) rest) acc))]))

(module+ test
  (begin-for-syntax
  (require rackunit)
  
    (check-equal? (get-index-range '(2 2 2)) '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1)))))

(define-for-syntax (context-ref/index ctxt ix) 
  (car (filter 
         (λ (expr) (equal? ix (map syntax->datum (cdr (syntax->list (context-ref (get-id-ctxt expr) #'index))))))
         ctxt)))

(define-for-syntax (get-index-axis max-ix axis)
  (define range (get-index-range max-ix))
  (group-by (lambda (ix) 
    `(,@(take ix axis) ,@(take-right ix (sub1 (- (length max-ix) axis))))) range))


(module+ test
  (begin-for-syntax
  (require rackunit)
  
    (check-equal? (get-index-axis '(2 2 2) 0) '(((0 0 0) (1 0 0)) ((0 0 1) (1 0 1)) ((0 1 0) (1 1 0)) ((0 1 1) (1 1 1))))))

(define-for-syntax (zero-index n) (build-list n (λ (_) 0))))])))