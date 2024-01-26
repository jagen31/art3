#lang scribble/doc

@require[scribble/manual
         scribble-abbrevs/manual]

@title{Language Concepts}

@table-of-contents[]

@section{Notations: the art forms}

@racketgrammar*[#:literals (put delete-by-id \@ ...)
  (art-expr (code:line (delete-by-id id ...))
            (code:line (art-object-id racket-expr ...))
            (code:line (art-rewriter-id racket-expr ...)))]

@defform[(define-art-object (art-object-id [id ...]))]
@defform[(define-art-rewriter art-rewriter-id racket-expr)]
@defform[(define-art-realizer art-realizer-id racket-expr)]
@defform[(realize art-realizer-id art-expr ...)]

@section{Procedural Concepts: The three re's of art}

Let's start at the very beginning, with re, re, re.  Represent, rewrite, and realize.
These are the main procedures that go into writing an art program.

@subsection{Representing}

@margin-note{In tonart, expressions are placed in time.  Expressions at the same
time are related.  For example, a scale degree n occurring at the same time as a
scale represents the nth note of that scale.} 
A work is @bold{represented} by describing the various objects in the
work as (s-)expressions, then composing the expressions within a coordinate
system, which implicitly relates the expressions to each other in that space.

Representation deals almost exclusively with objects and the markup language.
Art is a notation system in the sense that the final output is meant for a
realizer to read and use to produce a performance.  The objects in the
representation are what the realizer actually sees.

@subsection{Rewriting}

@margin-note{The rewriter to convert scale degrees to notes demands that, for
every scale degree in the region it is rewriting, a scale and an octave be
present in the surrounding context.  From there the rewrite is very simple.} 
The work is @bold{rewritten} using a meta-language, which is a procedural rewrite
system that makes the full racket language available, operating on art syntax objects with
your favorite racket syntax libraries.  The rewrite system is aware of the
coordinate system and the relation it defines between the objects.

Rewriting deals with rewriters and the meta-language.  Of course,
rewriters also implicitly deal with the objects they are rewriting.  The general
purpose of rewriting is transforming the work into something that more closely
resembles what you would like the realizer or the audience to see.  No one
except you will see the rewriters!  They are entirely a part of the meta
language: keep that in mind.  If it helps, think of rewriting as having the
computer perform some work for you to help make the composing easier.

@subsection{Realizing}

@margin-note{For music, realizers range from playing a sound immediately to
compiling to c++ code to compiling to MusicXML, to quoting the music expressions and
putting them in a list.}
Finally, the art is @bold{realized}, meaning it is compiled to a viable representation in racket.
Realizers (also called realizers) are extremely diverse.

Realization is the only phase working with true racket syntax objects, and it is
about taking art code at phase 1 to racket code at phase 0.  In other words, all
representation, rewriting, and realization happens at compile time over art
syntax, and the realizer finally outputs the corresponding racket syntax that
will be compiled by racket into the runtime code.  Again, realizers are diverse 
and a piece might be realized at different times, in different formats, in
different ways.  @margin-note*{Leaving a comment as to which step represents the
actual composition is extremely helpful!} A composer with multiple realizations
in mind should write a program that rewrites into a single ur-composition
representing the actual, official work.  Then, further chains of rewrites should
branch off from there, resulting in a tree. Each branch massages the
work into the formats needed by the realizers.  

@section{Denotations: art expressions in context}

The fundamental concepts of art are expressions, objects, coordinates, contexts,
rewriters, and realizers. 

@subsection{Expressions}

Expressions are syntaxes describing features of the artwork.
Objects are recognized classes of expressions.  Examples of music objects are
note, scale, and rhythm.  Examples of drawing objects are shape, color, and
fill.  Art programming is about placing objects within a coordinate system.
Coordinates are objects with some special properties and interactions with core
language forms.  The only coordinate every expression is guaranteed to have is
@code{id}, which is unique for every expression.  Examples of music
coordinates are (time) interval and voices.  Examples of drawing coordinates are
bounding-box or overlap-ordering.  

@subsection{Contexts}

The coordinate system is put to use via contexts.  Contexts are special sets of
expressions which are subsets of all the expressions of the artwork.  Contexts
are defined by functions from coordinates to expressions.  The simplest context
is the "identity" context, which is a type of context that every expression has.
Theoretically speaking, the only coordinate it is determined by is the
expression's id.  This context contains expressions that only apply to that
expression and no others.  Coordinates almost always go in an expression's
identity context.

@margin-note{
  When I say a context is within another context,
  I am referring to the within? relation.  When I say an expression is "in the context"
  of another expression I am saying that the inner expression's identity context is within the outer expression's.}

The other main context is the general coordinate context.  The general
coordinate context is determined by a relation @code{within?}, which pairs
contexts together.  All objects in the context which are not coordinates are
ignored.  If no coordinates are in either context then they automatically satisfy
the relation in both directions (this is an important invariant which needs to
be maintained when writing coordinates!  The standard macros do this
automatically).  From there, global rules are used to compare the coordinates in
the two contexts.  Every coordinate has a homogenous rule for when both contexts
contain an expression of that coordinate type.  Examples of this are- intervals
check if the left interval is inside the right.  Voices check if the left is a
subset of the right ('(tenor) is within '(tenor alto soprano)).  In many
languages this will be enough.  However arbitrary rules can be added.  For
example, in music, beat intervals (from beat 1 to 8) and measure intervals (from
measure 1 beat 1 to measure 3 beat 2) can be interspersed freely.  However,
these are not orthogonal, they represent the same dimension.  Luckily two
special rules for testing metric-interval/interval and interval/metric-interval
pairs exist to make the within? relation work.  The rules are checked in no
particular order and conjuncted, so they @bold{have to commute}.  This also implies that
if all the rules are run on two contexts with no coordinates, then they must all
be true.  The macros for defining simple rules all preserve this condition
automatically.



@subsection{Rewriters and Realizers}

The last two conceptual object to talk about are rewriters and realizers.  

Rewriters are functions from the rewriter syntax, the artwork, and a target
coordinate set to an art language expression.  An art language expression can be
a rewriter or an art form.  Art forms are @code{put},
@code{delete-by-id}, and @code{"at sign"}.  (Note: an art-language expression
can also be just an object, but that is syntactic sugar for @code{(put
<the-object>)}).  Rewriters act on the target area, typically by finding all
expressions of a certain object type and deleting them, then replacing them with
something else.  An example is note->midi, which converts all notes in a target
area to midi numbers.  Some rewriters are more interesting, like @code{copy-to},
which takes all expressions in the target area and copies them into another.

Realizers are effectively syntax transforms, being functions transforming art
syntax to racket syntax.  A particular realizer is triggered by the racket form
@code{(realize <realizer> <art-expression> ...)}.  The @code{perform} form is an
expression, which is replaced by the result of the realizer.  Realizers
typically recognize a set of objects.  For example, a music realizer might
recognize notes and tones.  A good tactic for constructing realizers is to make
a realizer for each object the realizer can recognize and compose them together.
For example, music realizers are usually composed of a note subrealizer and a
tone subrealizer.

@subsection{A note on boundaries}

It is important to note that the boundaries
between what should be an object and what should be a rewriter aren't
immediately clear.  For example, in music, @code{transpose} could be a rewriter
which turns notes into other notes.  Or it could be an object with a
corresponding rewriter, @code{run-transpose}.  This is traditionally called
@code{reification}.  A useful rule is: if you'd ever want to directly display
the rewriter to a realizer, e.g.  write "transpose by a 5th" on the score, use
a reified rewriter.  Otherwise, it is simpler to just write a plain old
rewriter.  Somewhat evilly, there could also be more reification.
@code{run-transpose} could be made into an object and @code{do-run-transpose}
could be the rewriter.  Don't get caught up in these traps, save them for
Haskell programmers.

Another interesting boundary is what should be accomplished via rewriting and
what should be done by the realizer.  This question comes up in music often.  An
example is- there's a notation system called "figured bass" which has been used
since the baroque era to efficiently represent harmony.  @margin-note*{This is
actually the etymology of the word "realize", which I have borrowed from music.
Performing notes and chords from a fiugured bass is called "realizing" the
figured bass.} Early music keyboardists can read from it and perform it
directly, live. To do this well requires training and lots of practice.  But,
performing in this manner provides multiple benefits.  

1. The compactness of the format enables the realizer to read the rest of the
score more easily. 
2. The denotation of figures is more abstract than exact
notes, which enables a more flexible interpretation, responding to the
particular realizers, their interpretations, the particular instruments, the
room, etc. as the music is being produced.

For realizers who aren't trained in figured bass, editions exist where the figured
bass has been rewritten to be notes on a grand staff, representing what a
early music player might have played.  Of course, some of the benefits of a live
realization are lost.

Generalizing on this, the boundary for what should be done by a rewriter and
what should be done by a realizer is dependent on how feasible it is to have a
specific realizer perform that thing (the realizer that just quotes the
expressions can, unsurprisingly, perform everything!), but also on the desire to
have the objects exist in compositions.  For example, I stop rewriting at tones for all
computer music realizers, even though for some, I could theoretically convert the tones
into the equivalent vectors representing the tone as bytes.  I do this because I
don't see much other use for the bytevectors, and the realizer can easily do the work.
The boundary where rewriting stops and realizing begins can be understood as the 
"input type" of the realizer.
