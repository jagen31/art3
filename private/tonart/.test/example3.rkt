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

@chunk[<the-header>
  (i@ [0 48] (key a 0 minor))
]

MOZART
The Fire.

SALIERI
What time?

MOZART
Common time.

@chunk[<the-header>
  (i@ [0 48] (time-sig 4 4))
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

@chunk[<the-confutatis>
  (m@ [1 4 (basses)]
    (confutatis-rhythm)
    (seq (note a 0 3) (note e 0 3) (note c 0 4) (note a 0 3)))
]

(speaking)
Second measure, second beat.
(singing)
Ma-le-dic-tis.

@chunk[<the-confutatis>
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

@chunk[<the-confutatis>
  (m@ [9 14 (basses)]
    (flammis-rhythm)
    (seq (note e 0 4) (note d 0 4) (note c 0 4) (note b 0 3) (note a 0 3) (note g 0 3) (note f 0 3) (note d 0 3)))]

(speaking)
And fourth measure, fourth beat — D.
(singing)
Ma-le-dic-tis, flam-mis a-cri-bus ad-
dic-tis.

@chunk[<the-confutatis>
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

@chunk[<the-confutatis>
  (m@ [3 6 (tenors)]
    (confutatis-rhythm)
    (seq (note c 0 4) (note a 0 3) (note d 0 4) (note g 1 3)))
]

(speaking)
Second measure, fourth beat on D.
(singing)
Ma-le-dic-tis.

@chunk[<the-confutatis>
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


@chunk[<the-confutatis>
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

@chunk[<the-confutatis>
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

@chunk[<the-confutatis>
  (m@ [0 24 (tenors)] 
    (copy-to (bassoon-1))
    (copy-to (tenor-trombone)))]

MOZART
Exactly. The instruments to go with
the voices. Trumpets and timpani,
tonic and dominant.

@chunk[<the-confutatis>
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

@chunk[<the-confutatis>
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

@chunk[<the-confutatis>
  (ss@ (strings)
    (-- 0 [4] [4 (transpose-diatonic 1)] [4 (transpose-diatonic 2)] [4 (transpose-diatonic 3)])
    
    (-- 16 [1 (seq (^ 5) (^ 7) (^ 2) (^ 2) (^ 2))] [1 (seq (^ 2) (^ 7) (^ 5) (^ 5) (^ 5))] 
           [1 (seq (^ 5) (^ 1) (^ 3) (^ 3) (^ 3))] [1 (seq (^ 3) (^ 1) (^ 5) (^ 5) (^ 5))]
           [0.5 (^ 5)] [0.5 (^ 5)]))]

As Salieri writes, Mozart sings the ostinato from the
beginning, but the unaccompanied strings overwhelm his voice
on the soundtrack, playing the first six bars of their
agitated accompaniment. They stop.

SALIERI
That’s wonderful!

MOZART
Yes, yes - go on. The Voca Me. 
Suddenly sotto voce. Write that down: 
sotto voce, pianissimo. Voca me cum 
benedictis. Call me among the blessed.

@chunk[<the-voca> 
  (i@ [0 32] (dynamic pp))]

He is now sitting bolt upright, hushed and inspired.

MOZART
C Major. Sopranos and altos in thirds.  
Altos on C. Sopranos above.
(singing the alto 
part)
Vo-ca, vo-ca me, vo-ca me cum be-ne-
dic-tis.

@chunk[<the-voca>
  (i@ [0 15]
    (ss@ (altos)
      (key c 0 major)
      (rhythm 3.5 0.5 1.5 0.5 2 1.5 0.5 0.5 0.5 0.5 0.5 2 1)
      (octave 5)
      (seq (^ 1) (^ 1) (^ 2) (^ 2) (^ 1) 
           (^ 1) (^ 1) (^ 1) (^ 1) (^ 1) (^ 1) (^ 2) (^ 1)))
      (copy-to (sopranos))

    ;; small correction
    ;; FIXME jagen develop a better interface for this?
    (ss@ (altos) 
      (i@ [4 6] (delete ^) (-- 0 [1 (^ 1)] [1 (^ 0)]))
      (i@ [12 14] (delete ^) (-- 0 [1 (^ 1)] [1 (^ 0)])))
    (ss@ (sopranos) (transpose-diatonic 2)))]
  

SALIERI
Sopranos up to F on the second 'Voca'?

MOZART
Yes, and on 'dictis'.

SALIERI
Yes!

He writes feverishly.

MOZART
And underneath, just violins - 
arpeggio.

He sings the violin figure under the Voca Me (Bars 7,8,9).

@chunk[<the-definitions>
  (define-simple-rewriter voca-ostinato expand-voca-ostinato
    (rhythm 0.5 0.25 0.25 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.25 0.25 0.5 0.5 0.5))
    
  (define-simple-rewriter voca-ostinato-^ expand-voca-ostinato-^
    (seq (^ 1) (^ 3) (^ 5) (^ 8) (^ 7) (^ 6) (^ 5) (^ 4) (^ 3) (^ 2) (^ 3) (^ 4) (^ 5) (^ -2) (^ 1)))]

@chunk[<the-voca>
  (m@ [0 15 (strings)]
    (key c 0 major)
    (octave 4)
    (-- 0 [6.5 (voca-ostinato)] [1.5 (rhythm 0.25 0.25 0.5 0.25 0.25)] [6.5 (voca-ostinato)])
    (-- 0 [6.5 (voca-ostinato-^)] [1.5 (seq (^ 3) (^ 5) (^ 8) (^ 5) (^ 3))] [6.5 (voca-ostinato-^)]))]

MOZART
(speaking)
The descending scale in eighth notes, 
and then back suddenly to the fire 
again.

He sings the ostinato phrase twice.

MOZART
(speaking)
And that's it. Do you have it?

SALIERI
You go fast!

MOZART
(urgently)
Do you have it?

SALIERI
Yes.

MOZART
Then let me hear it. All of it. The 
whole thing from the beginning - 
now!


@chunk[<the-header>

  (i@ [0 100]
    (instrument-map
      [voice . 000/000_Montre_8]
      [trombone . 000/025_Trompette_8]
      [bassoon . 001/055_Voix_Celeste_8]
      [trumpet . 000/069_Quintadena8Viola4]
      [timpani . 000/073_Cornemuse_8]
      [strings . 000/069_Quintadena8]))

  (m@ [0 48 (sopranos)] (instrument voice))
  (m@ [0 48 (altos)] (instrument voice))
  (m@ [0 48 (tenors)] (instrument voice))
  (m@ [0 48 (basses)] (instrument voice))

  (m@ [0 48 (bass-trombone)] (instrument trombone))
  (m@ [0 48 (bassoon-2)] (instrument bassoon))

  (m@ [0 48 (tenor-trombone)] (instrument trombone))
  (m@ [0 48 (bassoon-1)] (instrument bassoon))

  (m@ [0 48 (trumpet)] (instrument trumpet))
  (m@ [0 48 (timpani)] (instrument timpani))

  (m@ [0 48 (strings)] (instrument strings))

  (i@ [0 48] (tempo 86))
]

@chunk[<the-footer>
  (i@ [0 100] 
    (expand-confutatis) (expand-flammis) (expand-voca-ostinato) (expand-voca-ostinato-^) ; expand the vars
    (expand-repeat) (apply-rhythm) ; repeats and rhythms
    (run-transpose-diatonic) (^->note) ; working with scale degrees
    #;(note->midi) #;(d/dt) ; ready to render
    (exact-subdivide 8 0.001)
    )]


@chunk[<*>
  (require "../../common/core.rkt" "../../common/stdlib.rkt" 
         "../../common/coordinate/interval.rkt" "../../common/coordinate/subset.rkt" 
         "../rewriter/stdlib.rkt" 
         "../rewriter/common-practice/lib.rkt"
         "../realizer/electronic/lib.rkt" 
         "../realizer/electronic/linuxsampler/lib.rkt"
         "../realizer/visual/musicxml/lib.rkt")

  <the-definitions>
  
    #;(define sound 
      (perform linuxsampler-performer
       <the-header> 
       (-- 0 [24 <the-confutatis>] [15 <the-voca>])
       <the-footer>))
        
    #;(define file (open-output-file "private/tonart/realizer/electronic/linuxsampler/.test/test.cpp" 
                                   #:exists 'replace))
    #;(displayln sound file)
    #;(close-output-port file)
    
    
    (define xml
      (perform musicxml-performer
       <the-header> 
       (-- 0 [24 <the-confutatis>] [15 <the-voca>])
       <the-footer>))
        
    (define file (open-output-file "private/tonart/realizer/visual/musicxml/.test/test.musicxml" 
                                   #:exists 'replace))
    (displayln xml file)
    (close-output-port file)]
