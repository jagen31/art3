#lang racket

(require "../core.rkt" "../stdlib.rkt" (for-syntax syntax/parse racket/list))
(provide (all-defined-out))

;;;;;;;;;;; INTERVAL COORDINATE THINGS
(begin-for-syntax
  (define (merge-interval l r)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (syntax-parse #`(#,l #,r)
        ;; FIXME ....
        #:datum-literals [start end]
        [((_ (start s1*:number) (end e1*:number)) (_ (start s2*:number) (end e2*:number)))
  
         (define s1 (syntax-e #'s1*))
         (define s2 (syntax-parse #'s2* [val:number (syntax-e #'val)] [_ #f]))
         (define s* (if s1 (if s2 (+ s1 s2) s1) (or s2 (break #f))))
  
         (define e2 (syntax-parse #'e2* [val:number (syntax-e #'val)] [_ #f]))
         (define e1 (syntax-e #'e1*))
         (when (and (not e1) (not e2)) (break #f))
         (define e* (and e2 (+ s1 e2)))

         #;(unless (or (not e1) (not e*) (< e* e1)) (println "oops") #;(raise-syntax-error 'merger (format "end is outside of parent interval: ~s" e2) e2))
  
         (qq-art r (interval (start #,s*) (end #,e*)))]
        [_ 
         (error 'oops "whoops")])))
  
  (define (interval-within? l r)
    (let/ec break
      (unless r (break #t))
      (unless l (break #f))
      (syntax-parse #`(#,l #,r)
        #:datum-literals [start end]
        [((_ (start s1*:number) (end e1*:number)) (_ (start s2*:number) (end e2*:number)))
         (define-values (s1 e1 s2 e2) (values (syntax-e #'s1*) (syntax-e #'e1*) (syntax-e #'s2*) (syntax-e #'e2*)))
         (and (>= s1 s2) (<= e1 e2))]))))



(define-coordinate (interval [start end] merge-interval interval-within?))

(define-rewriter i@
  (λ(stx)
    (syntax-parse stx
      [(_ [start* end*] expr ...)
       (qq-art stx (@ [(interval (start start*) (end end*))] expr ...))])))

(define-rewriter --
  (λ(stx)
    (syntax-parse stx
      [(_ start* {~and box [len:number expr ...]} ...)
       #:with (result ...)
         (for/fold ([acc '()] [t (syntax-e #'start*)] #:result (reverse acc))
                   ([box (syntax->list #'(box ...))] [l (syntax->list #'(len ...))] [e (syntax->list #'((expr ...) ...))])
           (values (cons #`(i@ [#,t #,(+ t (syntax-e l))] #,@e) acc) (+ t (syntax-e l))))
       (qq-art this-syntax (@ () result ...))])))

(define-art-object (repeat []))

(define-mapping-rewriter (expand-repeat [(: repeats repeat)])
  (λ (repeat)
    (syntax-parse repeat
      [(_ size*:number expr ...)
       #:do [
        (define size (syntax-e #'size*))
        (define-values (the-start the-end) (syntax-parse (context-ref (get-id-ctxt repeat) #'interval) 
          [({~datum interval} ({~datum start} s) ({~datum end} e)) (values (syntax-e #'s) (syntax-e #'e))]))
       ]
       #:with (result ...)
         (for/list ([i (in-range 0 (- the-end the-start) size)])
           #`[#,size expr ...])
       (qq-art this-syntax (-- 0 result ...))]
      [_ (error 'expand-repeat "oops")])))
  

(define-rewriter translate
  (syntax-parser
    [(_ value:number)
     #:with (result ...) (for/foldr ([acc '()]) 
                ([expr (current-ctxt)])
       (define-values (the-start the-end) (syntax-parse (context-ref (get-id-ctxt expr) #'interval) [({~datum interval} ({~datum start} s) ({~datum end} e)) (values #'s #'e)]))
       (cons (delete-expr expr)
         (cons (qq-art expr 
           ;; FIXME jagen
           (@ [(interval (start value) (end +inf.0))] 
             (@ [#,@(get-id-ctxt expr)] (put #,expr)))) acc)))
     (qq-art this-syntax (@ () result ...))]))

(define-art-object (rhythm []))

(define-mapping-rewriter (apply-rhythm [(: rhythms rhythm)])
  (λ (r)
    (syntax-parse r
      [(_ expr:number ...)
       ;; FIXME copy id ctxt
       #:with (result ...)
         (for/list ([e (syntax->list #'(expr ...))] [i (in-naturals)])
           #`[#,e (! #,i)])

       (qq-art this-syntax
          (@ ()
            (-- 0 result ...)
            ;; FIXME jagen TOTALLY UNSAFE (this will seq-ref in the surrounding context :'( )
            (seq-ref)))])))
