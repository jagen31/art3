#lang scribble/doc

@require[scribble/manual
         scribble-abbrevs/manual
         art]

@title{Core Library}

Art comes with a library of objects, coordinates, contexts, rewriters, and realizers which will be
generally useful across various languages.

@table-of-contents[]

@(require scribble/eval)
 
 
@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require art art/timeline)]

@section{Objects}

@defform[(number value)]

A number.

@examples[
    #:eval helper-eval
    (realize (draw-realizer [50 50]) (number 42))
]

@defform[(symbol value)]

A symbol.

@examples[
    #:eval helper-eval
    (realize (draw-realizer [50 50]) (symbol hello))
]

@section{Coordinates}

@subsection{Interval}

Intervals are coordinates with a start and an end.  Interval @code{within?} is the
intuitive definition of closed interval containment.  Merging intervals offsets the
right interval by the start of the left interval. Intervals are used in timelines and music.

@defform[(\@ [(interval (start number) (end number))] art-expr ...)]

@examples[
    #:eval helper-eval
    (realize (draw-realizer [800 100]) 
      (timeline
        (|@| [(interval (start 2) (end 5))]
          (symbol hello))
        (|@| [(interval (start 5) (end 10))]
          (symbol world))))
]

Run the statements inside the interval denoted by start and end.  An interval within
another interval will have its start and end offset by the start of the outer interval.

@defform[(i@ [number number] body ...)]

A shorthand for the above.

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

Translate all intervals by the given number.
@examples[
    #:eval helper-eval
    (realize (draw-realizer [800 100]) 
      (timeline
        (-- 2 [3 (symbol hello)] [5 (symbol world)])
        (translate -2)))
]

@defform[(loop number art-expr ...)]
@defform[(expand-loop)]

Represents a loop of the specified duration.  The loop must be expanded within an interval and
will be expanded to consecutive intervals containing the given expressions.  It will be repeated 
as many times as fits cleanly into the surrounding interval. TODO explain rewriters inside loops

@subsection{Subset}

Sets are coordinates with a set of identifiers.  Subset @code{within?} and @code{merge} can either be
superset and union, or subset and intersection.  Subset sets are used for voices in music.  Superset
sets are used for lexical scope.

@subsection{Index}

Indices are coordinates with a list of numbers.  An index with n-numbers is called an n-dimensional index.
Index @code{within?} is @code{=} and @code{merge} is append.  Indices are used in @code{sequences}.

@subsection{Name}

Names are coordinates with a list of identifiers. Name @code{within?} is prefix match and @code{merge} 
is append.  Names are used in namespaces.

@section{Contexts}