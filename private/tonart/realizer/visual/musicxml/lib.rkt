#lang at-exp racket

(require "../../../../common/core.rkt" "../../../../common/stdlib.rkt" 
         "../../../../common/coordinate/interval.rkt" "../../../../common/coordinate/subset.rkt" 
         "../../../../common/coordinate/instant.rkt" "../../../../common/coordinate/switch.rkt" 
         "../../../rewriter/stdlib.rkt" 
         "../../../rewriter/common-practice/lib.rkt" 
         racket/runtime-path
  (for-syntax syntax/id-set syntax/id-table (only-in ee-lib compiled-from) syntax/parse racket/match racket/list racket/string racket/format racket/dict) rsound rsound/envelope sf2-parser)
(provide (all-defined-out))


(begin-for-syntax
  (define (duration->type dur)
    (match dur
      [(or 1/8 0.125) 'thirty-second]
      [(or 1/4 0.25) 'sixteenth]
      [(or 1/2 0.5) 'eighth]
      [1 'quarter]
      [2 'half]
      [4 'whole]))

  (define (compile-note n)
    (syntax-parse n
      [(_ p:id a:number o:number)
       (define ctxt (get-id-ctxt n))
       (syntax-parse (context-ref ctxt #'interval)
         [(_ (_ start*) (_ end*))
          #:do [(define duration (- (syntax-e #'end*) (syntax-e #'start*)))]
          @string-append{
           <note>
             <pitch>
               <step>@(format "~a~a" (string-upcase (symbol->string (syntax-e #'p))) (match (syntax-e #'a) [0 ""] [1 "#"] [-1 "b"]))</step>
               <octave>@(~s (syntax-e #'o))</octave>
             </pitch>
             <duration>@(~s duration)</duration>
             <type>@(~s (duration->type duration))</type>
           </note>}])]))

  (define (compile-score-part voice)
    (define name (symbol->string (syntax-e voice)))
    @string-append{
    <score-part id=@(format "~s" name)>
      <part-name>@|name|</part-name>
    </score-part>
    })

  (define (compile-part voice notes)
    (define name (symbol->string (syntax-e voice)))
    @string-append{
    <part id=@(format "~s" name)>

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
       @(string-join (map compile-note notes) "\n")
       </measure>
    </part> 
    }
  )

  (define (get-voices music)
    (for/fold ([acc (immutable-free-id-set)])
              ([expr music])
      (syntax-parse (context-ref (get-id-ctxt expr) #'subset) 
        [(_ v ...) (free-id-set-union acc (immutable-free-id-set (syntax->list #'(v ...))))]
        [_ (free-id-set-add acc #'default)])))

  (define (partition-by-voice music)
    (for/fold ([acc (make-immutable-free-id-table)])
              ([expr music])
      (define (update i) 
        (free-id-table-update acc i (λ (ns) (append ns (list expr))) (λ () '())))
      (syntax-parse (context-ref (get-id-ctxt expr) #'subset) 
        [(_ v ...)
         (define l (syntax->list #'(v ...)))
         (if (= (length l) 1) (update (car l)) acc)]
        [_ (update #'default)])))

  (define (compile music)

    (define voices (get-voices music))
    (define notes
      (filter (λ (expr) (syntax-parse expr [(head:id _ ...) (free-identifier=? (compiled-from #'head) #'note)])) 
              music))

    (define notes-by-voice (partition-by-voice notes))

    @string-append{
    <?xml version="1.0" encoding="UTF-8" standalone="no"?>
    <!DOCTYPE score-partwise PUBLIC
        "-//Recordare//DTD MusicXML 4.0 Partwise//EN"
        "http://www.musicxml.org/dtds/partwise.dtd">
    <score-partwise version="4.0">
      <part-list>
        @(string-join (map compile-score-part (free-id-set->list voices)) "\n")
      </part-list>

      @(string-join (dict-map notes-by-voice compile-part) "\n")
    </score-partwise>
    }))

(define-performer musicxml-performer
  (syntax-parser
    [(_ ctxt ...)
     #`#,(compile (syntax->list #'(ctxt ...)))]))
