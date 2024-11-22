#lang scribble/doc

@require[(except-in scribble/manual index)
         scribble-abbrevs/manual art]

@title{Core Library}

Art comes with a library of objects, coordinates, contexts, rewriters, and realizers which will be
generally useful across various languages.

@table-of-contents[]

@(require scribble/eval)
 
 
@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require art art/sequence/ravel)]
                  

@section{General Forms}

@defform[(define-art id art-statement ...)]
Define an art variable.  The statements are evaluated in an empty context.

@defform[(|@| [coordinate ...] art-statement...)]

Intuitively, run the given statements at the given coordinates.

This is accomplished by independently merging the given coordinates into the identity context of each statement,
then running the statements.

@defform[(draw-realizer [width height])]

Draw an artwork.  See Extending the Draw Realizer.

@defform[(number value)]

A number.

@examples[
    #:eval helper-eval
    (realize (draw-realizer [50 50]) (number 42))
]

@defform[(numbers number ...)]

a series of indexed numbers, shorthand for @code{(ix-- (number ...) (number ...) (number ...))}. 

@examples[
    #:eval helper-eval
    (realize (draw-realizer [300 50]) (seq (numbers 42 1.618 57 3.14)))
]

@defform[(symbol value)]

A symbol.

@examples[
    #:eval helper-eval
    (realize (draw-realizer [50 50]) (symbol hello))
]

@defform[(hole)]

Represents a place where an object might go.

@examples[
    #:eval helper-eval
    (realize (draw-realizer [50 50]) (hole))
]

@defform[(fill-holes id)]

A rewriter which maps all holes within its coordinates to all objects of type @code{id} which surround the hole.
The inserted objects will have their own identities and the same coordinates as the former hole.

@examples[
  #:eval helper-eval
  (define-art square1  (seq (ix-- (numbers 1 4/3) (numbers 5/3 2))))
  (realize (draw-realizer [200 200]) square1)

  (define-art mystery-square (seq (ix-- (ix-- (hole) (symbol square)) (ix-- (symbol back) (hole)))))
  (realize (draw-realizer [200 200]) mystery-square)

  (realize (draw-realizer [200 200]) 
    (seq square1 mystery-square (run-apl (mix *ctxt*)) (fill-holes number)))
]

@section{Coordinates}

@subsection{Interval}

Intervals are coordinates with a start and an end.  Interval @code{within?} is the
intuitive definition of closed interval containment.  Merging intervals offsets the
right interval by the start of the left interval. Intervals are used in timelines and music.

@defform[(interval [number number])]

An interval coordinate. 

@examples[
  #:eval helper-eval
  (realize (draw-realizer [800 100]) 
    (timeline
      (|@| [(interval [2 5])] (symbol hello))
      (|@| [(interval [5 10])] (symbol world))))
]

@defform[(i@ [number number] body ...)]

Shorthand for an interval.

@examples[
  #:eval helper-eval
  (realize (draw-realizer [800 100]) 
    (timeline
      (i@ [2 5] (symbol hello))
      (i@ [5 10] (symbol world))))
]

@defform[(-- number [number art-expr ...] ...)]
@defform[(-- [number art-expr ...] ...)]

Create a series of consecutive intervals of the given lengths, running the statements within those intervals.
The first number determines the start of the first interval.  If it is not provided then it is 0.

@examples[
  #:eval helper-eval
  (realize (draw-realizer [800 100]) 
    (timeline
      (-- 2 [3 (symbol hello)] [5 (symbol world)])))
]

@defform[(translate number)]

Translate all intervals by the given amount.
@examples[
  #:eval helper-eval
  (realize (draw-realizer [800 100]) 
    (timeline
      (-- 2 [3 (symbol hello)] [5 (symbol world)])
      (translate -2)
      (-- 2 [3 (symbol hello-)] [5 (symbol world-)])))
]

@defform[(dilate number)]

Dilate all intervals by the given factor.  The distance between the target interval and the start of the dilation is
also dilated (which is the intuitive behavior).

@examples[
  #:eval helper-eval
  (realize (draw-realizer [800 100]) 
    (timeline
      (-- 1 [1 (symbol hello)] [1 (symbol world)])
      (dilate 4)
      (-- 1 [1 (symbol hello-)] [1 (symbol world-)])))

  (realize (draw-realizer [800 100]) 
    (timeline
      (-- 1 [1 (symbol hello)] [1 (symbol world)])
      (i@ 1 (dilate 4))
      (-- 1 [1 (symbol hello-)] [1 (symbol world-)])))
]

@defform[(loop number art-expr ...)]
@defform[(expand-loop)]

Represents a loop of the specified duration.  The loop must be expanded within an interval and
will be expanded to consecutive intervals containing the given expressions.  It will be repeated 
as many times as fits cleanly into the surrounding interval. TODO explain rewriters inside loops

@examples[
  #:eval helper-eval
  (realize (draw-realizer [800 100]) 
    (timeline
      (i@ [0 30]
        (loop 10 (-- 2 [3 (symbol hello)] [5 (symbol world)])))
      (expand-loop)))
]

@defform[(rhythm number ...)]

Represents a series of consecutive intervals of the given durations.  A sequence and a rhythm can be converted
to a concrete series of intervals via @code{apply-rhythm}.  A form that is in context over the entire span 
of the rhythm can be converted to a concrtete series of intervals via @code{rhythm->holes} and @code{fill->holes}.

@defform[(uniform-rhythm number)]

A rewriter that creates a rhythm subdividing its interval into equal parts of the provided length.

@defform[(rhythm->holes)]
A rewriter that converts a rhythm to a consecutive series of holes.  Useful in tandem with fill-holes.

@examples[
  #:eval helper-eval
    (define-art world-domination-plan
      (timeline
        (i@ [0 3.5] (symbol Cat)) (i@ [0 2.5] (symbol Hello_____World)) (i@ [2 4] (symbol ____!))
        (rhythm 1 1.25 0.75 0.5)))

    (code:comment @#,elem{Better decipher this, before it's too late!})
    (realize (draw-realizer [800 100]) world-domination-plan)

    (code:comment @#,elem{We've had a breakthrough! We've almost got it...})
    (realize (draw-realizer [800 100]) 
      world-domination-plan (rewrite-in-timeline (rhythm->holes)))

    (code:comment @#,elem{Oh no, it's all meowver---- *HISSSSS* [END TRANSMISSION]})
    (realize (draw-realizer [800 100]) 
      world-domination-plan (rewrite-in-timeline (rhythm->holes) (fill-holes symbol)))
]

@subsection{Sets}

Sets are coordinates with a set of identifiers.  Set @code{within?} and @code{merge} can either be
superset and union, or subset and intersection.  Subset sets are used for voices in music.  Superset
sets are used for a model of lexical scope.

@subsection{Indices}

Indices are coordinates with a list of numbers.  An index with n-numbers is called an n-dimensional index.
Index @code{within?} is @code{=} and @code{merge} is append.  Indices are used in @code{sequences}.

@defform[(index expr ...)]
@defform[(ix@ [number ...] expr ...)]
@defform[(ix-- expr ...)]

@subsection{Name}

Names are coordinates with a list of identifiers. Name @code{within?} is prefix match and @code{merge} 
is append.  Names are used in namespaces.

@defform[(name id ...)]
@defform[(name@ (id ...) expr ...)]

@section{Contexts}

@subsection{Timeline}

Timelines are contexts equipped with an interval coordinate.

@defform[(timeline expr ...)]
@defform[(rewrite-in-timeline expr ...)]

@examples[
  #:eval helper-eval
  (define-art layer3 (timeline (i@ [0 1] (number 42)) (i@ [2 3] (number 42))))
  (define-art layer2 (timeline (i@ [0 3] layer3) (i@ [6 9] layer3)))
  (define-art layer1 (timeline (i@ [0 9] layer2) (i@ [18 27] layer2)))

  (realize (draw-realizer [800 50]) layer1)
]

@subsection{Sequence}
Sequences are contexts equipped with an index coordinate.

@defform[(seq expr ...)]
@defform[(rewrite-in-seq expr ...)]

@examples[
  #:eval helper-eval
  (define-art the-seqs
    (ix-- 
      (seq (ix-- (number 42) (number 42) (symbol X))) 
      (seq (ix-- (number 42) (symbol X) (number 42))) 
      (seq (ix-- (symbol X) (number 42) (number 42)))))
  (realize (draw-realizer [500 50]) (seq the-seqs))
  (realize (draw-realizer [200 100]) (seq the-seqs (run-apl (mix *ctxt*))))
]

@defform[(! ix)] 
@defform[(seq-ref)] 

@defform[(ix-loop expr ...)]
@defform[(expand-ix-loop expr ...)]

@subsection{Namespace}

Namespaces are contexts equipped with a name coordinate.

@defform[(namespace expr ...)]
@defform[(rewrite-in-namespace expr ...)]

@examples[
  #:eval helper-eval
  (realize (draw-realizer [800 50]) 
    (namespace
      (name@ the-answer (number 42))
      (name@ the-questions 
        (namespace 
          (name@ q1 (symbol |What is 4 + 2?|))
          (name@ q2 (symbol Life))))))
]

@defform[(ref id)]
@defform[(resolve-ref)]

Namespaces have some convenient interactions with art definition forms.

@defform[(reify-art-definitions)]

Reify identifiers bound using @code{define-art} into a namespace art. FIXME jagen this has 500
possible semantics and needs clarification.

@examples[
  #:eval helper-eval
  (realize (draw-realizer [800 50]) (namespace (reify-art-definitions)))
]

@defform[(namespace-provide-realizer)]

Compile all names in a namespace to racket art definitions, and provide them.

@examples[
  #:eval helper-eval
  (module test racket
    (require art art/namespace)
    (realize (namespace-provide-realizer)
      (name@ the-answer (number 42))
      (name@ the-questions 
        (namespace 
          (name@ q1 (symbol |What is 4 + 2?|))
          (name@ q2 (symbol Life))))))

  (require 'test)

  (realize (draw-realizer [50 50]) the-answer)
]

A cool technique is possible using the two forms above- running a transform over an entire module
before exporting.

@examples[
 #:eval helper-eval
  (module test2 racket
    (require art art/namespace art/sequence art/sequence/ravel)

    (code:comment @#,elem{<Foo, Bar>})
    (define-art thing1 (seq (ix-- (symbol Foo) (symbol Bar))))

    (code:comment @#,elem{<1, 2, 3>})
    (define-art thing2 (seq (numbers 1 2 3)))

    (code:comment @#,elem{provide the definitions, but reversed and repeated 3 times, for demo purposes.})
    (realize (namespace-provide-realizer)
      (reify-art-definitions)
      (rewrite-in-seq (run-apl (replicate (lit 3) (enclose (apl:reverse *ctxt*)))))))

  (require 'test2)
  (realize (draw-realizer [800 50]) thing1 thing2)
]
