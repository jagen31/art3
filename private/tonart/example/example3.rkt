#lang scribble/lp2

MOZART
Where did I stop?

SALIERI
(consulting the
manuscript)
The end of the Recordare — Statuens
in parte dextra.

MOZART
So now the Confutatis. Confutatis
Maledictis. When the wicked are
confounded. Flammis acribus addictis.
How would you translate that?

SALIERI
Consigned to flames of woe.

MOZART
Do you believe in it?

SALIERI
What?

MOZART
A fire which never dies. Burning one
forever?

SALIERI
Oh, yes.

MOZART
Strange!

SALIERI
Come. Let’s begin.

He takes his pen.

SALIERI
Confutatis Maledictis.

MOZART
We ended in F Major?

SALIERI
Yes.

MOZART
So now — A minor. Suddenly.

Salieri writes the key signature.

@chunk[<the-key>
  (i@ [0 32] (key a 0 minor))
]

MOZART
The Fire.

SALIERI
What time?

MOZART
Common time.

@chunk[<the-time-sig>
  (i@ [0 32] (time-sig 4 4))
]

Salieri writes this, and continues now to write as swiftly
and urgently as he can, at Mozart’s dictation. He is obviously
highly expert at doing this and hardly hesitates. His speed,
however, can never be too fast for Mozart’s impatient mind.

MOZART
Start with the voices. Basses first.
Second beat of the first measure -
A.
(singing the note)
Con-fu-ta-tis.

@chunk[<the-definitions>
  (define-simple-rewriter confutatis-rhythm expand-confutatis
    (rhythm 0.75 0.25 1 1))]

@chunk[<the-music>
  (m@ [1 4 (basses)]
    (confutatis-rhythm)
    (seq (note a 0 3) (note e 0 3) (note c 0 4) (note a 0 3)))
]

(speaking)
Second measure, second beat.
(singing)
Ma-le-dic-tis.

@chunk[<the-music>
  (m@ [5 8 (basses)]
    (confutatis-rhythm)
    (seq (note b 0 3) (note e 0 3) (note d 0 4) (note g 1 3)))]

(speaking)
G-sharp, of course.

SALIERI
Yes.

MOZART
Third measure, second beat starting
on E.
(singing)
Flam-mis a-cri-bus ad-dic-tis.

@chunk[<the-definitions>
  (define-simple-rewriter flammis-rhythm expand-flammis
    (rhythm 0.75 0.25 0.75 0.25 0.75 0.25 1 1))]

@chunk[<the-music>
  (m@ [9 14 (basses)]
    (flammis-rhythm)
    (seq (note e 0 4) (note d 0 4) (note c 0 4) (note b 0 3) (note a 0 3) (note g 0 3) (note f 0 3) (note d 0 3)))]

(speaking)
And fourth measure, fourth beat — D.
(singing)
Ma-le-dic-tis, flam-mis a-cri-bus ad-
dic-tis.

@chunk[<the-music>
  (m@ [15 24 (basses)]
    (-- 0 [2 (rhythm 0.75 0.25 0.75 0.25)] 
          [5 (flammis-rhythm)])
    (-- 0 [2 (seq (note d 0 4) (note d 0 4) (note d 0 4) (note e 0 3))]
          [5 (seq (note g 1 3) (note e 0 3)
                  (note c 0 4) (note a 0 3) (note c 0 4) (note a 0 3) 
                  (note g 1 3) (note e 0 3))]))]

(speaking)
Do you have that?

SALIERI
I think so.

MOZART
Sing it back.

Salieri sings back the first six measures of the bass line.
After the first two measures a chorus of basses fades in on
the soundtrack and engulfs his voice. They stop.

MOZART
Good. Now the tenors. Fourth beat of
the first measure — C.
(singing)
Con-fu-ta-tis.

@chunk[<the-music>
  (m@ [3 6 (tenors)]
    (confutatis-rhythm)
    (seq (note c 0 4) (note a 0 3) (note d 0 4) (note g 1 3)))
]

(speaking)
Second measure, fourth beat on D.
(singing)
Ma-le-dic-tis.

@chunk[<the-music>
  (m@ [7 10 (tenors)]
    (confutatis-rhythm)
    (seq (note d 0 4) (note b 0 3) (note e 0 4) (note a 0 3)))
]

(speaking)
All right?

SALIERI
Yes.

MOZART
Fourth measure, second beat — F.
(singing)
Flam-mis a-cri-bus ad-dic-tis, flam-
mis a-cri-bus ad-dic-tis.


@chunk[<the-music>
  (m@ [13 23 (tenors)]
    (-- 0 [4 (rhythm 0.75 0.25 0.75 0.25 0.75 0.25 0.75 0.25)] [5 (flammis-rhythm)])
    (-- 0 [4 (seq (note f 0 4) (note e 0 4) (note d 0 4) (note c 0 4) (note b 0 3) (note a 0 3) (note g 1 3) (note e 0 3))]
          [5 (seq (note d 0 4) (note d 0 4) (note d 1 4) (note d 1 4) (note d 1 4) (note d 1 4) (note e 0 4) (note e 0 3))]))]

His voice is lost on the last words, as tenors engulf it and
take over the soundtrack, singing their whole line from the
beginning, right to the end of the sixth measure where the
basses stopped, but he goes on mouthing the sounds with them.
Salieri writes feverishly. We see his pen jotting down the
notes as quickly as possible: the ink flicks onto the page.
The music stops again.

MOZART
Now the orchestra. Second bassoon
and bass trombone with the basses.
Identical notes and rhythm.
(He hurriedly hums
the opening notes of
the bass vocal line)

@chunk[<the-music>
  (m@ [0 24 (basses)] 
    (copy-to (bassoon-2))
    (copy-to (bass-trombone)))]

The first bassoon and tenor trombone -

SALIERI
(labouring to keep up)
Please! Just one moment.

Mozart glares at him, irritated. His hands move impatiently.
Salieri scribbles frantically.

MOZART
It couldn’t be simpler.

SALIERI
(finishing)
First bassoon and tenor trombone -
what?

MOZART
With the tenors.

SALIERI
Also identical?

@chunk[<the-music>
  (m@ [0 24 (tenors)] 
    (copy-to (bassoon-1))
    (copy-to (tenor-trombone)))]

MOZART
Exactly. The instruments to go with
the voices. Trumpets and timpani,
tonic and dominant.

@chunk[<the-music>
  (ss@ (trumpet)
    (-- 0 [1 (^ 1)] [1] [1 (^ 1)] [1] 
          [1 (^ 4)] [1] [1 (^ 4)] [1]
          [1 (^ 1) (^ 5)] [1] [1 (^ 1) (^ 5)] [1]
          [1 (^ 4)] [1] [1 (^ 4)] [1]
          [1 (^ 5)] [1] [1 (^ 1)] [1]
          [1 (^ 5)])
          
    (i@ [0 24] (copy-to (timpani))))
  
  (i@ [0 24]
    (ss@ (trumpet) (octave 4))
    (ss@ (timpani) (octave 3)))]


He again hums the bass vocal line from the beginning,
conducting. On the soundtrack, we hear the second bassoon
and bass trombone play it with him and the first bassoon and
tenor trombone come in on top, playing the tenor vocal line.
We also hear the trumpets and timpani. The sound is bare and
grim. It stops at the end of the sixth measure. Salieri stops
writing.


SALIERI
And that’s all?

MOZART
Oh no. Now for the Fire.
(he smiles)
Strings in unison — ostinato on all -
like this.

He sings the urgent first measure of the ostinato.

@chunk[<the-music>
  (ss@ (strings)
    (i@ [0 24] (octave 4))

    (i@ [0 20] 
      (repeat 1 (rhythm 0.125 0.125 0.25 0.25 0.25)))
    (i@ [0 16] 
      (repeat 2 (-- 0 [1 (seq (^ 1) (^ 2) (^ 3) (^ 3) (^ 3))] 
                      [1 (seq (^ 3) (^ 2) (^ 1) (^ 1) (^ 1))]))))]

MOZART
(speaking)
Second measure on B.

He sings the second measure of the ostinato.

MOZART
(speaking)
Do you have me?

SALIERI
I think so.

MOZART
Show me.

Salieri sings the first two measures of the string ostinato.

MOZART
(excitedly)
Good, good — yes! Put it down. And
the next measures exactly the same,
rising and rising — C to D to E, up
to the dominant chord. Do you see?

@chunk[<the-music>
  (ss@ (strings)
    (-- 0 [4] [4 (transpose-diatonic 1)] [4 (transpose-diatonic 2)] [4 (transpose-diatonic 3)])
    
    (-- 16 [1 (seq (^ 5) (^ 7) (^ 2) (^ 2) (^ 2))] [1 (seq (^ 2) (^ 7) (^ 5) (^ 5) (^ 5))] 
           [1 (seq (^ 5) (^ 1) (^ 3) (^ 3) (^ 3))] [1 (seq (^ 3) (^ 1) (^ 5) (^ 5) (^ 5))]
           [1 (^ 5)] [1 (^ 5)]))]

As Salieri writes, Mozart sings the ostinato from the
beginning, but the unaccompanied strings overwhelm his voice
on the soundtrack, playing the first six bars of their
agitated accompaniment. They stop.

SALIERI
That’s wonderful!

@chunk[<the-music>
  (i@ [0 24] (expand-confutatis) (expand-flammis) (expand-repeat) (apply-rhythm) (run-transpose-diatonic) (^->note) (note->midi))]

@chunk[<*>
  (require "../../common/core.rkt" "../../common/stdlib.rkt" 
         "../../common/coordinate/interval.rkt" "../../common/coordinate/subset.rkt" 
         "../stdlib.rkt" 
         "../common-practice/lib.rkt"
         "../computer/lib.rkt" "../organ/hymn.rkt"
  rsound)
  (set-output-device! 1)

  <the-definitions>
  
    (define sound 
      (perform music-rsound-performer 

        (m@ [0 32 (basses)] (instrument |Montre 8 Flute 4|))
        (m@ [0 32 (tenors)] (instrument |Montre 8 Flute 4|))

        (m@ [0 32 (bass-trombone)] (instrument |Tromp. en chamade|))
        (m@ [0 32 (bassoon-2)] (instrument |Voix Celeste 8|))

        (m@ [0 32 (tenor-trombone)] (instrument |Tromp. en chamade|))
        (m@ [0 32 (bassoon-1)] (instrument |Voix Celeste 8|))

        (m@ [0 32 (trumpet)] (instrument |Quintadena8Viola4|))
        (m@ [0 32 (timpani)] (instrument |Cornemuse 8|))

        (m@ [0 32 (strings)] (instrument |Quintadena8Viola4|))

        <the-key>
        <the-time-sig>
        <the-music>))
        
    (play sound)
    (rs-write sound "confutatis.wav")]