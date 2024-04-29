#lang racket

(require art/private/core 2htdp/image 
         (for-syntax syntax/parse syntax/id-table racket/string syntax/to-string fmt))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art-realizer draw-realizer 
  (λ (stx) 
    (syntax-parse stx
      [(_ [width:number height:number])
       #`(above/align 'left
         #,@(for/list ([e (current-ctxt)]) 
              (parameterize ([drawer-width (syntax-e #'width)] 
                             [drawer-height (syntax-e #'height)]) 
                (drawer-recur e)))
         empty-image empty-image)])))

(define (draw-arrow width color)
   (above/align 'right (line 10 5 color) (line width 0 color) (line 10 -5 color)))


(define-for-syntax (do-draw-trace exprs width height ctxt im- embed)
  ;; we'll rewrite step by step and draw each step
  (for/fold ([acc ctxt] [im im-]) 
            ([e exprs])
    (define-values (ctxt* im+)
      (syntax-parse e
        [(head:id expr ...)
         #:do [(define maybe-embed (syntax-local-value #'head))]
         #:when (embed/s? maybe-embed) 
         (define-values (c i) 
           (do-draw-trace (syntax->list #'(expr ...)) width height '() #'empty-image (λ (x) (list #`(head #,@(embed x))))))
         (values c #`(above/align 'left (text #,(format "~a:" (symbol->string (syntax->datum #'head))) 20 'purple) #,i))]
        [_
         (define rewriter-image (realize-art-exprs #'(draw-quoted goldenrod) (list e)))
         (define rewriter-image*
           #`(above/align 'left  #,rewriter-image (rectangle 10 10 'solid 'transparent) (draw-arrow (image-width #,rewriter-image)'purple)))
         (define rewritten (rewrite-in acc e))
         (define rewritten-image (realize-art-exprs #'(draw-realizer [800 100]) (embed rewritten)))
         (values rewritten #`(above/align 'left #,rewriter-image* #,rewritten-image))]))
    (values ctxt* #`(above/align 'left #,im #,im+))))

(define-art-realizer draw-trace-realizer
  (λ (stx)
    (define-values (width height) 
      (syntax-parse stx [(_ [w:number h:number]) (values (syntax-e #'w) (syntax-e #'h))]))
    (define refs (context-ref* (current-ctxt) #'reflected))
    #`(overlay
        #,(for/fold ([im #'empty-image])
                    ([ref refs])
          (syntax-parse ref
            [(_ expr ...) 
             (define-values (ctxt im*)
               (do-draw-trace (syntax->list #'(expr ...)) width height '() im (λ (x) x)))
             im*]))
        (rectangle #,width #,height 'solid 'transparent))))

(begin-for-syntax 
  (define recursive-drawers (make-free-id-table))
  (struct drawer/s [body])
  (define drawer-width (make-parameter 0))
  (define drawer-height (make-parameter 0)))

(define-syntax (define-drawer stx)
  (syntax-parse stx
    [(_ name:id body)
     #'(define-syntax name (drawer/s body))]))

(define-syntax (register-drawer! stx)
  (syntax-parse stx
    [(_ head:id r:id) 
     #'(begin-for-syntax (free-id-table-set! recursive-drawers #'head #'r))]))

(define-art-realizer draw-quoted
  (λ (stx)
    (syntax-parse stx
      [(_ colo:id)
       #`(text #,(program-format (string-trim (syntax->string #`(#,@(current-ctxt))))) 24 '#,(syntax-e #'colo))])))

(define-for-syntax (drawer-recur stx)
  (syntax-parse stx
    [(head:id _ ...)
     (define draw (free-id-table-ref recursive-drawers #'head (λ () #f)))
     (if draw (or ((drawer/s-body (syntax-local-value draw)) stx) #'empty-image) (realize-art-exprs #'(draw-quoted blue) (list stx)))]))
