# The Art Language

## What is it?

This is the art language, a language, or set of languages really for creating
works and composing them together.  The big idea behind the art language is:
composing geometrically by writing and rewriting expressions in a coordinate
system, or frequently, multiple coordinate systems.

Art places an emphasis on separating an expression's form from its meaning.
There are two main ways in which an art progam is interpreted- through rewrites
and through performance.  

### Rewriting

Rewriting is much broader than just a tool for interpretation- rewrites are used
for everything, such as "deleting all sounds above 440 Hz" or "giving all shapes
within this bounding box a red fill".  Those are examples of rewrites used to
efficiently construct a piece.  The rewrites we are focused on are ones that
preserve meaning.  For example, "replace the love theme with its equivalent
notes", or "fill in the harmony for the given chord structure".  These rewrites
provide an interpretation by saying either "the new piece is equivalent to the
former", or "the new piece defines a set of performances which is a subset of
the former".

(N.B. there is no language distinction between constructive and meaning
preserving rewrites, it is a mental model which you should throw away
immediately upon concluding this README.)

### Performing

#### Humans

Performance, on the other hand, is directly interpreting the expressions within
the coordinate system.  No changes are made to the art.  For example, a soprano
sings an A in the 5th octave after seeing `(note a 0 5)` @ `(interval (start 0)
(end 4))` `(voice soprano)`.  Or, a soprano sings an A in the 5th octave after
seeing {`(key d major)`, `(^ 5)` @ `(interval (start 0) (end 4))` `(voice
soprano)`, `(octave 6)` @ `(interval (start 0) (end 32))` `(voice soprano)`}
(N.B. she is probably reading a score representation of these, not reading them
from a markdown file!  Also, the `(octave 6)` because `octave` defines where the
C is in the scale.) Frequently, unless you are dealing with wildly gifted
musicians or just having some fun, you'll use meaning preserving rewrites to
turn the second score into the first one.  But the idea is at any point within a
chain of meaning preserving rewrites, the rewriting can STOP and an attempt to
perform the art can immediately be made, which, provided it is performed
properly, will result in two thumbs up from the artist.

(N.B. now, this is not true if the composer says that `(note a 0 5)` is the
ACTUAL composition and the `(key d major)` so on so forth stuff was just
scaffolding to construct this piece computationally.  Be careful with what
rewrites are meaning preserving and which are constructive.  As an artist, write
this down (as simple as marking "this is the actual composition!" or 
"this is part of the actual composition!").  As a performer, ask!)

#### Computers

An important concept is that performing is not only left up to humans.
Computers can also perform.  For example, a computer can take {`(shape circle)`
@ `(bounding-box (x1 0) (x2 100) (y1 0) (y2 100))`, `(fill solid red)`}, and
turn it into a window with a red circle, most likely by compiling to some
drawing library.  In the same way that human performers are capable of
understanding certain things, so too with computer performers.  Usually this is
much more precise with computers (e.g. the `tone-performer` requires you convert
all the notes you want played down to expressions of the form `(tone <Hz>)`, and
performs them as sine tones).  That being said, priming chatgpt with the syntax
of the language and telling it to go to town is also a perfectly valid try at a
performer, which will be a lot more flexible and unpredictable in the inputs it
can or can't read, much like your drunk uncle trying to sing carols with you on
Christmas.

## Project structure

...

## TODO

- Fix the strange quasiquote unquote problem in rewriters (workaround is to use `with-syntax` instead.  which is probably better practice anyhow)
- Figure out a better way to do bindings for rewriters (use `syntax-spec` better)
- ss@ -> voice@
- Finish `metric-interval`/`interval` compatibility
- Assess the viability of merge rules / within? rules.
- Fix hyfrydol :((((
- better support for infinite intervals (so you don't have to always provide an end)
- possibly making stdlib coordinates into coordinate families