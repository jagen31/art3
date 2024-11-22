#lang scribble/doc

@require[scribble/manual
         scribble-abbrevs/manual]

@title{Language Concepts}

@table-of-contents[]

@section{Intro}

Art is a family of languages which are abstract syntaxes for different forms of
art.  Abstract syntax, in the view of this documentation, is an interface to a
language which composes terms of the language in a geometric space.  The structure of
that space models some important relationship between the terms.  The
usual example is the abstract syntax tree, which models the term/subterm
relation, used for determining properties like evaluation order and
binding structure.

At the theoretical level, an Art language is designed around a geometry which
models some essential structure present in an art form.  Terms express the
elements of the art.  The arrangement of terms within the geometry models their
interactions.  The language is an interface to the art form, programs are works
of art, and interpretations of those programs are performances.  

Practically, each Art language has a set of terms called @code{coordinates}, and
a set of terms called @code{objects}.  A work of art is composed by attaching 
coordinates to objects.  The set of coordinates attached to an Art object is
intended to be interpreted as a geometric object (such as a point, a shape, a
surface etc.), embedded within this Art language's chosen geometric space.
This geometric object usually somehow relates to the existence of the Art
object, be it in space, time, or logic.  Common concepts like intersection,
overlap, and containment can then be computed and leveraged in metaprogramming
and performance.

@section{Languages}

The most well-developed Art language to date is Tonart, for writing music.  That
is in a separate collection.

The collection here, aside from containing the core art macros, data structures
and evaluator, contains a few small Art languages which can be composed together
into larger ones.  For example, the @code{timeline} Art language defines one
coordinate, @code{interval}, which is also used in @code{Tonart}.   The
@code{seq} sequence language is frequently embedded inside @code{Tonart}
programs.

Another language, datenart, in a collection of the same name, structures syntax
fragments within the geometry of a relational database.  The benefit of
using syntax objects in a fake database is you can compose notation for any
data, in a similarly powerful but much more ergonomic way as storing code in
databases via strings.  Then you can either compile to SQL DDL and DML, or
virtually run SQL on the syntaxes using a SQL metalanguage, within the compiler.

@section{Composition}

Art languages can essentially be freely composed (with some concerns around
languages that share coordinates, such as is the case between Timeline Art and
Tonart, or between an Art and itself).  So, it isn't super clear what
coordinates a given Art program will actually have.  Art programs can be
inlined into one another, which will freely mix their coordinates.  Or they can
be nested, which will create a boundary between the two, making the inner art an
object placed within the outer art's geometry.  If you are
familiar with APL, this is the same difference between adding a dimension and
adding a box (nested array).  In fact, an APL interface is provided as a
metalanguage for sequence Art, which lets you manipulate inlined and nested
sequence arts.

An example of this composition is music stored within a database- this can be
Tonart inside Datenart.  A funnier example of this is a database composed into
music (Datenart inside Tonart).

@section{Structure}

The objects in an Art language are extensible and are likewise known to
freely intermingle.  When we try to pin down the syntax plus the structure as a
whole it is called the "object language" of the artwork.  As has been hinted at,
the geometry comes into play at two times:

1. When doing metaprogramming over the object language.  Art provides an
interface for implementing rewrites.  It allows a rewrite to be applied to a
region of the artwork.  Within the rewrite's implementation, which is in full
racket, a library is made available for observing the forms within the
rewriter's region, for observing the forms surrounding the rewriter's region,
for observing the forms surrounding each form within the rewriter's region, and so
on.

2. When interpreting the Art program.  The same library available to rewriters
is available to interpreters, which are called @code{realizers}.  Realizers
interpret the art by compiling it to Racket (implementation: macro).
Technically a realizer could compile one Art to another Art, which makes it seem
like a rewriter.  But what it produces is actually Racket that realizes another
art, so they aren't even remotely composable.  Use realizers to compile out of
the art world, and implement art-to-art transforms as rewriters.

@section{Fun}

The most important thing to do when writing Art is to have fun!  Please!  Life
is short!
