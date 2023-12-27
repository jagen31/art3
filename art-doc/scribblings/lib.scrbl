#lang scribble/doc

@require[scribble/manual
         scribble-abbrevs/manual
         art]

@title{Core Library}

Art comes with a library of coordinates, objects, and rewriters which will be
generally useful across various languages.

@table-of-contents[]

@section{Intervals}

Intervals are coordinates with a start and an end.

@defform[(\@ [(interval (start number) (end number))] art-expr ...)]

Run the statements inside the interval denoted by start and end.  An interval within
another interval will have its start and end offset by the start of the outer interval.

@defform[(i@ [number number] body ...)]

A shorthand for the above.

@defform[(-- number [number art-expr ...] ...)]
@defform[(-- [number art-expr ...] ...)]

Create a series of consecutive intervals of the given lengths, running the statements within those intervals.
The first number determines the start of the first interval.  If it is not provided then it is 0.

@defform[(translate number)]

Translate all intervals by the given number.

@defform[(loop number art-expr ...)]
@defform[(expand-loop)]

Represents a loop of the specified duration.  The loop must be expanded within an interval and
will be expanded to consecutive intervals containing the given expressions.  It will be repeated 
as many times as fits cleanly into the surrounding interval. TODO explain rewriters inside loops

@defform