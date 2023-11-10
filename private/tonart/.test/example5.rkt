#lang scribble/lp2

OK, here is a jam that I came up with today.  It was designed as both a mood lifter and an etude so I could
develop better hand independence, especially with syncopation in the left hand.  In this file I'm going
to lay out the main elements of it, show some 'subjams' I needed to learn to work up to it, and show
some variations.

Here are the main elements:

@chunk[<the-music>
  (interpretation+ main
    [the-comp-rhythm (rhythm 1.5 1 1 0.5 1 0.5 2.5)]
    [the-comp-harmony 
      (relative-harmony M [M 7] M [m 7] M)]
    [the-melody-rhythm (i@ [0 8] (uniform-rhythm 0.5))])]

The comp has a syncopated rhythm and a harmony, and the melody is just constant eighth notes.

(the harmony is specified in 'harmonic relativity', if you've never seen it.  It indicates- starting 
on a major chord, jump a major 7th and play another major chord.  Then jump a minor 7th and play another 
major chord.  Note- in practice you'll actually be jumping the inverse- so a minor 2nd and a major 2nd.
Just for clarity, the exact chords I was mainly practicing with were E major, Eb major, Db major)

Here is the jam:

@chunk[<the-music>
  (interpretation+ main
    [the-composition
      (i@ (0 8)
        (key+starting-chord-related-by-aug-5th right-hand left-hand)
        (ss@ (left-hand) (the-comp-harmony) (the-comp-rhythm))
        (ss@ (right-hand) (the-melody-rhythm))
        (run-interpretation main))])]

`key+starting-chord-related-by-aug-5th` is obviously not a standard library function, it is something 
specific to our composition.  It indicates that the key of the melody is related to the starting chord 
of the comp by an augmented 5th.  The jam is polytonal, but the home key of the comp is really just a 
perfect 4th away from the melody key, which is less crazy sounding.  Concretely, if melody key = Ab
then comp starting chord = E (A5) and comp key/final chord = Db (P4).


@chunk[<the-definitions>
  ;; the object
  (define-art-object (key+starting-chord-related-by-aug-5th [key-voice chord-voice]))
  ;; the interpretation of the object
  (define-mapping-rewriter (->key+starting-chord [(: expr key+starting-chord-related-by-aug-5th)])
    (λ (stx expr)
      ;; the rewriter provides the starting key, the object provides the voices
      (syntax-parse (list stx expr)
        [((_ key-pitch:id key-accidental:number) (_ key-voice chord-voice))
         (define p+a 
           (transpose-by-interval (syntax-e #'key-pitch) (syntax-e #'key-accidental) 5 'augmented))
         (define-values (p* a*) (values (car p+a) (cadr p+a)))

         (with-syntax ([p p*] [a a*])
           (qq-art stx
             (|@| () 
               (ss@ (chord-voice) (pitch p a))
               (ss@ (key-voice) (key key-pitch key-accidental major)))))])))
]

This is the official composition, everything before is considered scaffolding or composite parts.
Everything after is moving us towards a specific realization of the composition, either as a score,
or as a computer performance.  Oh, or a practice exercise.

Here is the version to perform off of, which rewrites the composition slightly and specifies it to be 
in Ab/Db.

@chunk[<the-music>
  (interpretation+ main
    [the-composition-for-perf
      (i@ (0 8)
        (the-composition)
        (run-interpretation main)
        ;; it's in a flat! (and d flat)
        (->key+starting-chord a -1)
        (ss@ (left-hand) 
          (relative-harmony->chord-seq)
          (apply-rhythm* 4 1.5 2.5)))])]

Here is a further version which can be compiled directly into code to run on the sampler.

@chunk[<the-music>
  (interpretation+ main
    [the-composition-for-computer-perf
      (i@ (0 64)

        ;; loop it every 8 beats (for a total of 8 times.  Sorry for the confusion, the 8 means every 8 beats.)
        (loop 8 (the-composition-for-perf))
        (expand-loop)
        (run-interpretation main)

        (ss@ (right-hand) 
          ;; loop this every 16
          (loop 16
            ;; just some sequence
            (-- [8 (seq (^ 1) (^ 2) (^ 3) (^ 2) (^ 1) (^ 0) (^ 1) (^ 2) (^ 3) (^ 4) (^ 5) (^ 6) (^ 7) (^ 8) (^ 5) (^ 8))]
                [8 (seq (^ 1) (^ 4) (^ 6) (^ 4) (^ 6) (^ 8) (^ 6) (^ 8) (^ 11) (^ 8) (^ 6) (^ 8) (^ 6) (^ 4) (^ 1) (^ 4))]))
          (expand-loop)
          (apply-rhythm)
          (-- [16] [16 (transpose-diatonic -1)] [16 (transpose-diatonic -2)] [16 (transpose-diatonic -3)])
          (run-transpose-diatonic))

        (ss@ (left-hand) 
          ;; map the chords to the comp rhythm.
          (rhythm->holes)
          (fill-holes chord)
          ;; write out the chords as notes
          (chord->notes/simple 3))

        (ss@ (right-hand) (apply-rhythm) (octave 5) (^->note))

        ;; midi things
        (note->midi)
        (instrument-map [organ . 000/000_Montre_8]
                        [trumpet . 000/065_Quintadena_8])
        (ss@ (left-hand) (instrument organ))
        (ss@ (right-hand) (instrument trumpet))
        (tempo 120)

        ;; convert measure intervals to raw beat intervals
        (metric-interval->interval)
        ;; convert to on/off events
        (d/dt))])]

@chunk[<the-definitions>
  (define-mapping-rewriter (chord->notes/simple [(: crd chord)])
    (λ (stx crd)
      (syntax-parse stx
        [(_ octave*:number)
         (syntax-parse crd
           [(_ root accidental mode)
            #:do[ 
              (define octave (syntax-e #'octave*))
              (define pcs (generate-chord (syntax-e #'root) (syntax-e #'accidental) (syntax-e #'mode)))
              (define cdis (distance-above-c (caar pcs)))]
            #:with (result ...)
              (for/list ([pc pcs])
                (with-syntax ([p (first pc)] 
                              [a (second pc)] 
                              [o (if (>= (distance-above-c (first pc)) cdis) octave (add1 octave))])
                  (qq-art crd (note p a o))))
            #'(|@| () result ...)])])))]

@chunk[<*>
  (require "../../common/core.rkt" "../../common/stdlib.rkt" 
    "../../common/coordinate/interval.rkt" "../../common/coordinate/subset.rkt" 
    "../rewriter/stdlib.rkt" 
    "../rewriter/common-practice/lib.rkt"
    "../rewriter/common-practice/tonality.rkt"
    "../rewriter/common-practice/coordinate/metric-interval.rkt"
    "../realizer/electronic/lib.rkt" 
    "../realizer/electronic/linuxsampler/lib.rkt"
    "../realizer/visual/musicxml/lib.rkt"
    racket
    (for-syntax racket syntax/parse racket/match "../rewriter/common-practice/tonality.rkt"))

  <the-definitions>
  <the-music>
  #;(displayln (perform quote-performer (put (the-composition-for-computer-perf)) (run-interpretation main)))

  (define result 
    (perform linuxsampler-performer 
      (put (the-composition-for-computer-perf))
      (run-interpretation main)))
       
  (define file (open-output-file "private/tonart/realizer/electronic/linuxsampler/.test/test.cpp" 
                                   #:exists 'replace))
  (displayln result file)
  (close-output-port file)]
