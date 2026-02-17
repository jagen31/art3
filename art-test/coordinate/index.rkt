#lang racket
(require art/private/core art/coordinate/index (for-syntax rackunit))

(define-index-coordinate index)

(begin-for-syntax

  do-merge-index

  (check-equal? (syntax->datum (do-merge-index #'(index 1 2) #'(index 3 4))) '(index 1 2 3 4))
  (check-equal? (syntax->datum (do-merge-index #f #'(index 3 4))) '(index 3 4))
  (check-equal? (syntax->datum (do-merge-index #'(index 1 2) #f)) '(index 1 2))

  (check-equal? (do-index-within? #'(index 1) #'(index 1)) #t)
  (check-equal? (do-index-within? #'(index 1) #'(index 3)) #f)
  (check-equal? (do-index-within? #'(index 1 2) #'(index 1)) #t)
  (check-equal? (do-index-within? #'(index 1) #'(index 1 2)) #f)

  (check-exn 
    exn:fail?
    (λ ()
      (expr-single-index (ensure-id-ctxt #'(number 42)))))
  (check-exn 
    exn:fail?
    (λ ()
      (expr-single-index (put-in-id-ctxt (ensure-id-ctxt #'(number 42)) #'(index 1 2)))))
  (check-equal? 
    (expr-single-index (put-in-id-ctxt (ensure-id-ctxt #'(number 42)) #'(index 1))) 1)

  (check-equal?  (expr-index (ensure-id-ctxt #'(number 42))) '())
  (check-equal? 
    (expr-index (put-in-id-ctxt (ensure-id-ctxt #'(number 42)) #'(index 1 2))) '(1 2))

  max-index
  max-index*
  context-ref/index
  zero-index

  (check-equal? 
    (get-index-range '(2 2 2)) '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1)))
  
  (check-equal? 
    (get-index-axis '(2 2 2) 0) 
    '(((0 0 0) (1 0 0)) ((0 0 1) (1 0 1)) ((0 1 0) (1 1 0)) ((0 1 1) (1 1 1)))))

