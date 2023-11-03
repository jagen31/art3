#lang at-exp racket

(define (duration->type dur)
  (match dur)
    [(or 1/8 0.125) 'thirty-second]
    [(or 1/4 0.25) 'sixteenth]
    [(or 1/2 0.5) 'eighth]
    [1 'quarter]
    [2 'half]
    [4 'whole])

(define (compile-note n)
  (syntax-parse n
    [(_ p a o)
     (define ctxt (get-id-ctxt n))
     (syntax-parse (context-ref ctxt #'interval)
       [(_ (_ start*) (_ end*))
        @string-append{
         <note>
           <pitch>
             <step>@(syntax-e #'p)</step>
             <octave>@(syntax-e #'o)</step>
           </pitch>
           <duration>@(- end start)</duration>
           <type>
         </note>}]))

(define (compile-notes notes)
  @string-append{
  <?xml version="1.0" encoding="UTF-8" standalone="no"?>
  <!DOCTYPE score-partwise PUBLIC
      "-//Recordare//DTD MusicXML 4.0 Partwise//EN"
      "http://www.musicxml.org/dtds/partwise.dtd">
  <score-partwise version="4.0">
    <part-list>
      <score-part id="P1">
        <part-name>Music</part-name>
      </score-part>
    </part-list>
    <part id="P1">
      <measure number="1">
        <attributes>
          <divisions>1</divisions>
          <key>
            <fifths>0</fifths>
          </key>
          <time>
            <beats>4</beats>
            <beat-type>4</beat-type>
          </time>
          <clef>
            <sign>G</sign>
            <line>2</line>
          </clef>
        </attributes>
        <note>
          <pitch>
            <step>C</step>
            <octave>4</octave>
          </pitch>
          <duration>4</duration>
          <type>whole</type>
        </note>
  }