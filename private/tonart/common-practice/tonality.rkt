#lang racket

(require data/collection (prefix-in list: racket/base))
(module+ test (require rackunit))

;; starting on f is nice for computations
(define circle (cycle '(f c g d a e b)))
(define rev-circle (cycle '(b e a d g c f)))
(define line (cycle '(c d e f g a b)))

(define (transpose-by-interval pitch accidental num type)
  (define line* (drop (index-of line pitch) line))
  (define pitch* (first (drop (sub1 num) line*)))
  ;; FIXME jagen maybe a better way?
  ;; currently the algorithm produces P1 M2 M3 A4 P5 M6 M7
  (define ix-of-pitch (index-of circle pitch))
  (define ix-of-pitch* (index-of circle pitch*))
  (define accidental* (if (< ix-of-pitch* ix-of-pitch) (add1 accidental) accidental))

  (define num* (add1 (modulo (sub1 num) 7)))

  (define accidental** (+ accidental* 
    (match type 
      ['perfect (if (= num* 4) -1 0)] 
      ['major 0] ['minor -1] 
      ['diminished (if (= num* 5) -1 -2)] ['augmented (if (eq? num* 4) 0 1)])))
  (list pitch* accidental**))


(module+ test

  (define input-result-pairs
    '(((a 1 3 minor) . (c 1))
      ((f 0 4 perfect) . (b -1))
      ((f 0 4 augmented) . (b 0))
      ((a 0 3 major) . (c 1))
      ((f 0 5 augmented) . (c 1))
      ((g 0 5 perfect) . (d 0))
      ((c 0 4 perfect) . (f 0))
      ((d 0 3 major) . (f 1))
      ((c 0 3 major) . (e 0))
      ((c 0 6 major) . (a 0))
      ((c 0 7 major) . (b 0))
      ((d 0 7 major) . (c 1))

      ((e -1 1 perfect) . (e -1))
      ((e -1 2 major) . (f 0))
      ((e -1 3 minor) . (g -1))
      ((e -1 4 perfect) . (a -1))
      ((e -1 5 perfect) . (b -1))
      ((e -1 6 minor) . (c -1))
      ((e -1 7 minor) . (d -1))

      ((a 0 1 perfect) . (a 0))
      ((a 0 1 augmented) . (a 1))
      ((a 0 8 perfect) . (a 0))
      ((a 0 8 augmented) . (a 1))))

  (for-each (λ (x) (check-equal? (apply transpose-by-interval (car x)) (cdr x))) input-result-pairs))


#;(define (identify-interval p1 a1 p2 a2)
  (define interval (add1 (- (index-of p2 line) (index-of p1 line))))
  (define-values (index-of-p1 index-of-p2) (index-of p1 cycle) (index-of p2 cycle))
  (define type 
    (if (< index-of-p1 index-of-p2)
      (match* ((- a1 a2) interval)
        [(0 5) 'perfect]
        [(1 5)]
        [(0 4) 'augmented]
        [(0 6) 'minor]
        [(0 _ 'major)]))))

(define (generate-stack pitch accidental intervals types)
  (list:map transpose-by-interval 
    (list:build-list (length intervals) (λ(x) pitch))
    (list:build-list (length types) (λ(x) accidental))
    intervals
    types))

(define (generate-scale pitch accidental type)
  (define types
    (match type
      ['major '(perfect major major perfect perfect major major)]
      ['minor '(perfect major minor perfect perfect minor minor)]))
  (generate-stack pitch accidental (build-list 7 add1) types))

(module+ test
  (check-equal? (generate-scale 'a 0 'major) '((a 0) (b 0) (c 1) (d 0) (e 0) (f 1) (g 1)))
  (check-equal? (generate-scale 'e -1 'minor) '((e -1) (f 0) (g -1) (a -1) (b -1) (c -1) (d -1))))

(define (generate-chord pitch accidental type)
  (match-define `((,intervals ...) (,types ...))
    (match type
      ['major '((1 3 5) (perfect major perfect))]
      ['minor '((1 3 5) (perfect minor perfect))]
      ['diminished '((1 3 5) (perfect minor diminished))]))
  (generate-stack pitch accidental intervals types))

(module+ test
  (check-equal? (generate-chord 'f 1 'minor) '((f 1) (a 0) (c 1)))
  (check-equal? (generate-chord 'a -1 'diminished) '((a -1) (c -1) (e -2))))
