#lang racket

(require art/base art/coordinate/interval art/coordinate/instant art/coordinate/switch 
         art/private/lib art/sequence 2htdp/image 
         (for-syntax racket/format syntax/parse racket/list racket/math racket/match))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-interval-coordinate interval)

;;;;;;; time context.
(define-art-embedding (timeline [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-mapping-rewriter (rewrite-in-timeline [(: s timeline)])
  (λ (stx s)
    (syntax-parse stx
      [(_ expr ...)
       (syntax-parse s
         [(_ expr* ...)
           #:with (result ...) 
             (rewrite-in (syntax->list #'(expr* ...)) #'(context expr ...))
           #`(context #,(qq-art s (timeline result ...)))])])))

(define-drawer draw-timeline 
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (define ctxt (syntax->list #'(expr ...)))
       (define max-end
         (for/fold ([acc 1]) ([e ctxt])
           (define end (expr-interval-end e))
           (if (infinite? end) acc (max acc end))))

       (define each-width (/ (drawer-width) max-end))
       (define line
         (for/fold ([im #'empty-image])
                   ([i (in-range max-end 2)])
           #`(beside #,im 
               (overlay (text #,(~s (add1 i)) 16 'blue) 
                        (rectangle #,(* each-width 2) 10 'solid 'transparent)))))


         (for/fold ([im #`(rectangle #,(drawer-width) #,(drawer-height) 'solid 'transparent)])
                   ([e ctxt])
           (match-define (cons start end) (expr-interval e))

           (define end* (if (infinite? end) max-end end))
           (define x-start (* start each-width))
           (define x-end (* end* each-width))
           (define width* (- x-end x-start))

           (define sub-pic (parameterize ([drawer-width width*]) (drawer-recur e)))

           #`(overlay/xy
               (add-line (add-line (add-line #,sub-pic 0 10 #,width* 10 'purple) 0 0 0 20 'purple)
                 #,width* 0 #,width* 20 'purple)
                 #,(- x-start) #,(- (/ (drawer-height) 2)) #,im))])))

(register-drawer! timeline draw-timeline)

;;;;;;;;;; INTERVAL

(define-art-rewriter i@
  (λ(stx)
    (syntax-parse stx
      [(_ start*:number expr ...) (qq-art stx (i@ [start* +inf.0] expr ...))]
      [(_ [start* end*] expr ...) (qq-art stx (@ [(interval (start start*) (end end*))] expr ...))])))

(define-art-rewriter --
  (λ(stx)
    (syntax-parse stx
      [(_ start*:number {~and box [len:number expr ...]} ...)
       #:with (result ...)
         (for/fold ([acc '()] [t (syntax-e #'start*)] #:result (reverse acc))
                   ([box (syntax->list #'(box ...))] [l (syntax->list #'(len ...))] [e (syntax->list #'((expr ...) ...))])
           (values (cons #`(i@ [#,t #,(+ t (syntax-e l))] #,@e) acc) (+ t (syntax-e l))))
       (qq-art this-syntax (@ () result ...))]
      [(_ expr ...) (qq-art this-syntax (-- 0 expr ...))])))


;; repeats are reified since that's something you might want to realize directly (the alternative is
;; to have it be a rewrite.  In that case, a realizer will never encounter a repeat directly)
(define-art-object (loop []))

(define-mapping-rewriter (expand-loop [(: repeats loop)])
  (λ (stx repeat)
    (syntax-parse repeat
      [(_ size*:number expr ...)
       #:do [
        (define size (syntax-e #'size*))
        (define iv (context-ref (get-id-ctxt repeat) #'interval))
        (unless iv (raise-syntax-error 'expand-loop
          (format "repeat requires a beat interval, got: ~s" (syntax->datum (un-@ repeat))) repeat))
        (define-values (the-start the-end) (syntax-parse iv
          [({~literal interval} ({~datum start} s) ({~datum end} e)) (values (syntax-e #'s) (syntax-e #'e))]))
       ]
       #:with (result ...)
         (for/list ([i (in-range 0 (- the-end the-start) size)]) #`[#,size expr ...])
       (qq-art this-syntax (-- 0 result ...))]
      [_ (error 'expand-loop "oops")])))

;; repeats are reified since that's something you might want to realize directly (the alternative is
;; to have it be a rewrite.  In that case, a realizer will never encounter a repeat directly)
(define-art-object (repeat [num]))

(define-mapping-rewriter (expand-repeat [(: repeats repeat)])
  (λ (stx repeat)
    (syntax-parse repeat
      [(_ size*:number)
       #:do [
        (define size (syntax-e #'size*))
        (define iv (context-ref (get-id-ctxt repeat) #'interval))
        (unless iv (raise-syntax-error 'expand-loop
          (format "repeat requires a beat interval, got: ~s" (syntax->datum (un-@ repeat))) repeat))
        (define iv* (car (merge-coordinates (list iv) (list #`(interval (start 0) (end #,size))) (lookup-ctxt))))
        (define id-ctxt (put-in-ctxt (get-id-ctxt repeat) iv*))
        (define-values (the-start the-end) (syntax-parse iv
          [({~literal interval} ({~datum start} s) ({~datum end} e)) (values (syntax-e #'s) (syntax-e #'e))]))
       ]
       #:with (expr ...) (filter (λ (e) (context-within? (get-id-ctxt e) id-ctxt (lookup-ctxt))) (current-ctxt))
       #:with (del ...) (map delete-expr (syntax->list #'(expr ...)))
       #:with (result ...) (for/list ([i (in-range 0 (- the-end the-start) size)]) #`[#,size expr ...])
       (qq-art this-syntax (context del ... (-- 0 result ...)))]
      [_ (error 'expand-loop "oops")])))

(define-art-rewriter translate
  (syntax-parser
    [(_ value:number) 
     (define exprs (filter (λ (x) (context-within? (get-id-ctxt x) (get-id-ctxt this-syntax) (lookup-ctxt))) (current-ctxt)))
     #`(context #,@(map delete-expr exprs) (i@ value #,@exprs))]))

(define-art-rewriter dilate
  (syntax-parser
    [(_ value-:number) 
     (define exprs (filter (λ (x) (context-within? (get-id-ctxt x) (get-id-ctxt this-syntax) (lookup-ctxt))) (current-ctxt)))
     (define result (for/list ([e exprs])
       (define start (expr-interval-start e))
       (define my-start (or (expr-interval-start this-syntax) 0))
       (define value (syntax-e #'value-))
       (if start
         #`(i@ [#,(+ my-start (* (- start my-start) value)) #,(+ my-start (* value (- (expr-interval-end e) my-start)))]
             #,(remove-from-id-ctxt e #'interval))
         e)))
     #`(replace-full-context #,@result)]))



;; rhythms combine with seqs to place objects in intervals.  This could probably be generalized to
;; applying a list of coordinates to a list of objects, but the rhythm syntax for intervals is so
;; nice & quick.
(define-art-object (rhythm []))

(define-art-rewriter uniform-rhythm
  (λ (stx)
    (syntax-parse (context-ref (get-id-ctxt stx) #'interval)
      [(_ (start s) (end e))
       (syntax-parse stx
        [(_ val:number)
         (define num (inexact->exact (floor (/ (- (syntax-e #'e) (syntax-e #'s)) (syntax-e #'val)))))
         (qq-art stx (rhythm #,@(build-list num (λ (_) #'val))))])])))

(define-for-syntax (do-apply-rhythm stx r exprs)

  (define seq (require-context (lookup-ctxt) r #'seq))
  (define items-
    (syntax-parse seq [(_ seq-expr ...) (syntax->list #'(seq-expr ...))]))

  (define items (sort items- < #:key expr-single-index))

  (with-syntax ([(result ...) 
    (for/list ([e exprs] [i (in-naturals)])
      #`[#,e #,(quasisyntax/loc stx #,(remove-from-id-ctxt (list-ref items i) #'index))])])

    (qq-art r (context #,(delete-expr seq) (-- 0 result ...)))))

;; holes indicate spaces where objects should go.  They work well with rhythms.
(define-art-object (hole []))


(define-drawer hole-drawer
  (λ (e)
    (syntax-parse e
      [({~literal hole})
       #`(overlay (rectangle 15 25 'outline 'blue) (rectangle #,(drawer-width) #,(drawer-height) 'solid 'transparent))]
      [_ #f])))

(register-drawer! hole hole-drawer)

(define-art-rewriter fill-holes
  (λ (stx)
    (syntax-parse stx
      [(_ head:id)
       #:do [
       (define-values (holes items)
         (values (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'hole)
                 (context-ref*/within (lookup-ctxt) (get-id-ctxt stx) #'head)))]
       #:with (result ...)
         (for/fold ([acc '()] #:result (reverse acc))
                   ([h holes])
           (define items* (context-ref*/surrounding items (get-id-ctxt h) #'head))
           (append (map (λ (e) (qq-art h #,(remove-from-id-ctxt e #'interval))) items*) acc))
       #`(context #,@(map delete-expr items) #,@(map delete-expr holes) result ...)])))

(define-mapping-rewriter (rhythm->holes [(: r rhythm)])
  (λ (stx r)
    ;; cheeky implementation
    (syntax-parse r
      [(_ num:number ...)
       #:with (hole* ...) 
         (build-list (length (syntax->list #'(num ...))) (λ (_) #'(hole)))
       #`(context #,@(rewrite #'(seq (ix-- hole* ...)) r #'(apply-rhythm)))])))

(define-art-object (divisions [n]))

(define-art-rewriter exact-subdivide
  (λ (stx)
    (syntax-parse stx
      [(_ division* ε*:number)
       (define division (syntax-e #'division*))
       (define ε (syntax-e #'ε*))
       (define exprs
         (flatten
           (for/list ([expr (current-ctxt)])
             (define-values (s e) (values (expr-interval-start expr) (expr-interval-end expr)))
             (define s* (* s division))
             (define e* (* e division))
             (define (round+ensure-whole n)
               (define rounded (round n))
               (if (infinite? rounded)
                   rounded
                   (and (< (- rounded n) ε) (> (- rounded n) (- ε)) (inexact->exact rounded))))
              
             (define s** (or (round+ensure-whole s*) (raise-syntax-error 'exact-subdivide "score does not subdivide into the given number of divisions." expr)))
             (define e** (or (round+ensure-whole e*) (raise-syntax-error 'exact-subdivide "score does not subdivide into the given number of divisions." expr)))
             (list (delete-expr expr) 
                   (put-in-id-ctxt (remove-from-id-ctxt expr #'interval) 
                                   #`(interval (start #,s**) (end #,e**)))))))
       #`(context #,(qq-art stx (divisions division*)) #,@exprs)])))


(define-art-rewriter unsubdivide
  (λ (stx)
    (define divs (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'divisions))
    (define result
      (for/fold ([acc (current-ctxt)])
                ([div divs])
        (define/syntax-parse (_ ds) div)
        (run-art-exprs (list (qq-art div (dilate #,(/ 1 (syntax-e #'ds)))) (delete-expr div)) (current-ctxt))))
    #`(replace-full-context #,@result)))

(define-art-rewriter apply-rhythm*
  (λ (stx)
    (syntax-parse stx
      [(apply-rhythm* expr:number ...)
       (do-apply-rhythm stx stx (syntax->list #'(expr ...)))])))

(define-mapping-rewriter (apply-rhythm [(: rhythms rhythm)])
  (λ (stx r)
    (syntax-parse r
      [(_ expr:number ...)
       (do-apply-rhythm stx r (syntax->list #'(expr ...)))])))

#;(realize (quote-realizer)
  (seq (ix-- (dumb) (dumber) (dumbest)))
  (rhythm 3 4 5)
  (apply-rhythm))

(define-art-rewriter unapply-rhythm
  (λ (stx)
    (syntax-parse stx
      [(_ name:id)
       #:do [(define exprs (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'name))
             (define sorted (sort exprs < #:key expr-interval-start))
             (define r
               (for/fold ([rs '()] [t (expr-interval-start stx)] #:result (reverse rs))
                         ([e exprs])
                 (define end* (expr-interval-end e))
                 (values (cons (- end* t) rs) end*)))]
      #:with (sorted* ...) 
        (for/list ([expr sorted]) (remove-from-id-ctxt expr #'interval))
      #:with (r* ...) r
      #`(@ () #,@(map delete-expr sorted) #,(qq-art stx (seq (ix-- sorted* ...))) #,(qq-art stx (rhythm r* ...)))])))


(define-art-rewriter coalesce-seq
  (λ (stx)
    (syntax-parse stx
      [_
       #:do [(define exprs (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'seq))
             (define sorted (sort exprs < #:key expr-interval-start))]
       #:with (result ...)        
         (flatten 
           (for/list ([e sorted])
             (syntax-parse e
               [({~literal seq} expr ...)
                (map (λ (e) (remove-from-id-ctxt e #'index)) (syntax->list #'(expr ...)))])))
      #`(@ () #,@(map delete-expr sorted) #,(qq-art stx (seq (ix-- result ...))))])))

;; FIXME jagen WHAT to do with this...
;;;;;;; INSTANT/SWITCH
;; interval -> instant + switch
(define-art-rewriter d/dt
  (λ (stx)
    (syntax-parse stx
      [_
       (define target
         (filter 
           (λ (expr)
             (and (context-within? (get-id-ctxt expr) (get-id-ctxt stx) (current-ctxt))))
           (current-ctxt)))
       (with-syntax ([(target* ...)
         (flatten
           (for/list ([item target])
             (define current-interval (context-ref (get-id-ctxt item) #'interval))
             (if current-interval
              (syntax-parse current-interval
                [(_ (_ s:number) (_ e:number))
                  (define new-item (remove-from-id-ctxt item #'interval))
                  (list (delete-expr item)
                        (put-in-id-ctxt (put-in-id-ctxt new-item #'(switch #t)) #'(instant s))
                        (put-in-id-ctxt (put-in-id-ctxt new-item #'(switch #f)) #'(instant e)))])
              item)))])
         #`(@ () target* ...))])))
