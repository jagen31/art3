#lang racket

(require art/base art/timeline art/sequence (prefix-in plot: plot) (prefix-in im: 2htdp/image) racket/draw
         (for-syntax data/gvector racket/list racket/class racket/draw racket/math racket/match syntax/parse (prefix-in plot: plot/no-gui) images/compile-time))

(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art-object (function [vars form]))
(define-art-object (bitmap [data]))
(define-art-object (point-set [data]))

(define-drawer bitmap-drawer
  (λ (e)
    (syntax-parse e
      [(_ bs)
       #`(let ([image (im:rotate 0 (read-bitmap (open-input-bytes bs) 'png/alpha))])
           (im:scale/xy (/ #,(drawer-width) (im:image-width image)) (/ #,(drawer-height) (im:image-height image)) image))])))

(register-drawer! bitmap bitmap-drawer)

(define-for-syntax (save-png bm)
  (define p (open-output-bytes))
  (send bm save-file p 'png #:unscaled? #t)
  (define bs (get-output-bytes p))
  bs)

(define-mapping-rewriter (function->image [(: fn function)])
  (λ (stx fn)
    (syntax-parse stx
      [(_ bound ...)
       (syntax-parse fn
         [({~literal function} (var ...) form)
          (define f (eval-syntax #'(plot:function (λ (var ...) form) bound ...)))
          (qq-art fn
            (bitmap #,(parameterize ([plot:plot-width    150]
                                     [plot:plot-height   150]
                                     [plot:plot-x-label  #f]
                                     [plot:plot-y-label  #f]
                                     [plot:plot-x-ticks plot:no-ticks]
                                     [plot:plot-y-ticks plot:no-ticks]
                                     [plot:plot-line-width 0]
                                     [plot:plot-foreground-alpha 0]
                                     [plot:plot-background-alpha 0])
                        (save-png (plot:plot-bitmap f)))))])])))

(define-mapping-rewriter (image->point-set [(: im bitmap)])
  (λ (stx bm)
    (syntax-parse bm
      [(_ bs)
       (define image (read-bitmap (open-input-bytes (syntax-e #'bs)) 'png/alpha))
       (define width (send image get-width))
       (define height (send image get-height))
       (define pos (gvector))
       (for* ([x (in-range width)] [y (in-range height)])
         (define it (make-bytes 4))
         (send image get-argb-pixels x y 1 1 it)
         (when (ormap (λ (x) (not (= x 255))) (list (bytes-ref it 1) (bytes-ref it 2) (bytes-ref it 3)))
           (gvector-add! pos #`[#,x #,y])))
       (qq-art bm (point-set [#,width #,height] #,(gvector->list pos)))])))

(define-mapping-rewriter (point-set->image [(: ps point-set)])
  (λ (stx fn)
    (syntax-parse stx
      [(_ bound ...)
       (syntax-parse fn
         [({~literal point-set} [w h] ([x y] ...))
          (define f (eval-syntax #'(plot:points (map vector (list x ...) (list y ...)))))
          (qq-art fn
            (bitmap #,(parameterize ([plot:plot-width    150]
                                     [plot:plot-height   150]
                                     [plot:plot-x-label  #f]
                                     [plot:plot-y-label  #f]
                                     [plot:plot-x-ticks plot:no-ticks]
                                     [plot:plot-y-ticks plot:no-ticks]
                                     [plot:plot-line-width 0]
                                     [plot:plot-foreground-alpha 0]
                                     [plot:plot-background-alpha 0])
                        (save-png (plot:plot-bitmap f)))))])])))

(define-mapping-rewriter (fill-holes-from-points [(: ho hole)])
  (λ (stx ho)
    (define start (expr-interval-start ho))
    (define end (expr-interval-end ho))

    (define/syntax-parse {~and ps (_ [w h] ([x- y-] ...))} (require-context (lookup-ctxt) ho #'point-set))
    (define/syntax-parse (_ elem ...) (require-context (lookup-ctxt) ho #'seq))

    (define ps-width (syntax-e #'w))
    (define ps-height (syntax-e #'h))
    (define ps-start (expr-interval-start #'ps))
    (define ps-end (expr-interval-end #'ps))
    (define points (syntax->datum #'([x- y-] ...)))

    (define proportion (/ (- start ps-start) (- ps-end ps-start)))
    (define where (* ps-width proportion))
    (match-define (list _ y) (argmin (λ (xy) (abs (- where (car xy)))) points))
    (define y-proportion (/ (- ps-height y) ps-height))

    (define elem* (syntax->list #'(elem ...)))
    (define/syntax-parse result-ix (round (* (sub1 (length elem*)) y-proportion)))

    (qq-art ho (! result-ix))))

   
    
    
   
(define-art-realizer graph-realizer
  (λ (stx)
    (syntax-parse stx
      [(_ bound ...)
       #:with (renderer ...)
         (for/fold ([acc '()])
                   ([expr (current-ctxt)])
           (syntax-parse expr
             [({~literal function} (var ...) form)
              (cons #'(plot:function (λ (var ...) form) bound ...) acc)]))
       #'(plot:plot (list renderer ...))])))

#;(define-art my-music
  (i@ [0 8]
    (voice@ (soprano)
      (function (x) (+ (sin x) (sin (* 2 x)))))
    (voice@ (alto)
      (function (x) (- (expt x 2))))
    (voice@ (tenor)
      (function (x) (- (+ (* 2 x) 2))))))

#;(dmr [800 100]
     my-music
     (function->image pi (- pi)))

#;(define-art my-music-compiled
  my-music
  (function->image pi (- pi))
  (image->point-set)
  
  

  (i@ [0 8]
    (voice@ [soprano]
            (chord c 0 [m])
            (loop 2 (rhythm 1.25 0.25 0.25 0.25)) (expand-loop)
            (chord->scalar-note-seq [a 0 4] [a 0 5]))
    (voice@ [alto]
            (chord c 0 [m])
            (loop 2 (rhythm .25 0.25 0.25 1)) (expand-loop)
            (chord->scalar-note-seq [a 0 3] [a 0 4]))
    (voice@ [tenor]
            (chord c 0 [m])
            (urhy 1/4)
            (chord->scalar-note-seq [a 0 2] [a 0 3])))
  
  (rhythm->holes)

  (fill-holes-from-points)
  (delete point-set)

  (seq-ref)

  (tuning 12tet)
  (note->tone)
  (volume 5))
  
#;(dmr [800 200] my-music-compiled)