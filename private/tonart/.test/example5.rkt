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
    [the-melody-rhythm (i@ [0 4] (uniform-rhythm 0.5))])]

The comp has a syncopated rhythm and a harmony, and the melody is just constant eighth notes.

(the harmony is specified in 'harmonic relativity', if you've never seen it.  It indicates- starting 
on a major chord, jump a major 7th and play another major chord.  Then jump a minor 7th and play another 
major chord.  Note- you'll actually be jumping the inverse- so a minor 2nd and a major 2nd.
Just for clarity, the exact chords are going to end up E major, Eb major, Db major)

Here is a quick utility which will establish the starting chord of the comp and the key of the melody.
It is a polytonal jam, the starting chord of the comp is a diminshed 4th from the key, but the home
key of the comp is really a 5th away, which sounds far less crazy.  Concretely, if melody key = Ab
then comp starting pitch = E and comp key/final chord = Db.

@chunk[<the-definitions>
  ;; the object
  (define-art-object (related-key+starting-pitch [key-voice pitch-voice]))
  ;; the interpretation of the object
  (define-mapping-rewriter (x-related-key+starting-pitch [(: expr related-key+starting-pitch)])
    (λ (stx expr)
      ;; the rewriter provides the starting key, the object provides the voices
      (syntax-parse (list stx expr)
        [((_ key-pitch:id key-accidental:number) (_ key-voice pitch-voice))
         (define p+a 
           (transpose-by-interval (syntax-e #'key-pitch) (syntax-e #'key-accidental) 5 'augmented))
         (define-values (p* a*) (values (car p+a) (cadr p+a)))

         (with-syntax ([p p*] [a a*])
           (qq-art stx
             (|@| () 
               (ss@ (pitch-voice) (pitch p a))
               (ss@ (key-voice) (key key-pitch key-accidental)))))])))
]

Here is the jam:

@chunk[<the-music>
  (interpretation+ main
    [the-composition
      (measure@ (1 2)
        (related-key+starting-pitch right-hand left-hand)
        (ss@ (left-hand) (the-comp-harmony) (the-comp-rhythm))
        (ss@ (right-hand) (the-melody-rhythm))
        (run-interpretation main))])]

This is the official composition, everything before is considered scaffolding or composite parts.
Everything after is moving us towards a specific realization of the composition, either as a score,
or as a computer performance.  Oh, or a practice exercise.

Here is the version to perform off of, which is specified to be in Ab/Db.

@chunk[<the-music>
  (interpretation+ main
    [the-composition-for-perf
      (measure@ (1 2)
        (the-composition)
        (run-interpretation main)
        (x-related-key+starting-pitch a -1)
        (relative-harmony->chord-seq)
        (ss@ (left-hand) (apply-rhythm* 4 1.5 2.5)))])
]

Here are a couple useful exercises to work up to it:

@chunk[<the-music>
  (interpretation+ main
    [the-subjam1
      (measure@ (1 2)
        (the-harmony) (rhythm 2 2 4))])]

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

  (perform quote-performer
    (put (the-composition-for-perf))
    (run-interpretation main))]