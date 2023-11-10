#lang at-exp racket

(require "../../../../common/core.rkt" "../../../../common/stdlib.rkt" 
         "../../../../common/coordinate/interval.rkt" "../../../../common/coordinate/subset.rkt" 
         "../../../../common/coordinate/instant.rkt" "../../../../common/coordinate/switch.rkt" 
         "../../../rewriter/stdlib.rkt" "../lib.rkt" racket/runtime-path
  (for-syntax syntax/parse racket/match racket/list racket/string racket/dict) rsound rsound/envelope sf2-parser)
(provide (all-defined-out))

;; create a c++ string containing a program which plays the score via linuxsampler
(define-syntax (define-composite-linuxsampler-performer stx)
  (syntax-parse stx
    [(_ n:id {subperformer:id ...})
     #'(begin
         (define-composite-performer n {subperformer ...} [] 
           (λ(clauses)
              #`(begin
                  (define-values (note-statements instruments)
                    (for/fold ([statements '()] [instruments (set)] [t 0] #:result (values (reverse statements) (set->list instruments)))
                              ([expr (list #,@clauses)])
                      (match-define (list t* instrument-name midi-instrument-name statement) expr)
                      (values 
                        (if (= t* t)
                          (cons statement statements)
                          (cons statement (cons (format "std::this_thread::sleep_for(std::chrono::milliseconds(~a));" (inexact->exact (round (* 1000 (- t* t))))) statements)))
                        (set-add instruments (cons instrument-name midi-instrument-name))
                        t*)))

                  @string-append|{
#include<iostream>
#include<chrono>
#include<thread>
#include<filesystem>
#include<linuxsampler/Sampler.h>
#include<linuxsampler/drivers/audio/AudioOutputDeviceFactory.h>

namespace fs = std::__fs::filesystem;

int main() {
    auto sampler = new LinuxSampler::Sampler();
    auto factory = new LinuxSampler::AudioOutputDeviceFactory();

    auto params = std::map<std::string, std::string>();
    params["BUFFERSIZE"] = "2048";
    params["BUFFERS"] = "4";
    auto device = factory->Create("COREAUDIO", params);
|@(string-join 
  (for/list ([iname instruments])
    @string-append|{
auto |@(car iname) = sampler->AddSamplerChannel();
|@(car iname)->SetAudioOutputDevice(device);
|@(car iname)->SetEngineType("SFZ");
|@(car iname)->GetEngineChannel()->PrepareLoadInstrument(
|@(format "(fs::current_path() / \"..\" / \"..\" / \"resources\" / \"sfz\" / \"Jeux14\" / \"~a.sfz\").string().c_str()," (cdr iname))
  0);
|@(car iname)->GetEngineChannel()->LoadInstrument();
  
}|) "\n")
 std::this_thread::sleep_for(std::chrono::milliseconds(1000));
|@(string-join note-statements "\n")
 std::this_thread::sleep_for(std::chrono::milliseconds(1000));
}
}|))))]))


(define-subperformer linuxsampler-midi-subperformer
  (λ(ctxt*)
    (define imap* (context-ref ctxt* #'instrument-map))
    (unless imap* (raise-syntax-error 'midi-subperformer "no instrument map in context"))
    (define imap (syntax-parse imap* [(_ map ...) (syntax->datum #'(map ...))]))
    (define ctxt (sort ctxt* < 
      #:key (λ (stx) (syntax-parse (context-ref (get-id-ctxt stx) #'instant) [(_ result) (syntax-e #'result)]))))
    (for/foldr ([acc '()])
               ([stx ctxt])
      (syntax-parse stx
        [({~datum midi} num:number) 
         (define instrument (context-ref/surrounding ctxt (get-id-ctxt stx) #'instrument))
         (define tempo (context-ref/surrounding ctxt (get-id-ctxt stx) #'tempo))
         (define instant (context-ref (get-id-ctxt stx) #'instant))
         (define switch (context-ref (get-id-ctxt stx) #'switch))
         (unless instrument (raise-syntax-error 'midi-subperformer "no instrument in context for midi" stx))
         (unless tempo (raise-syntax-error 'midi-subperformer "no tempo in context for midi" stx))
         (unless instant (raise-syntax-error 'midi-subperformer (format "this performer requires an instant for all midis, got: ~s" (un-@ stx)) stx))
         (unless switch (raise-syntax-error 'midi-subperformer (format "this performer requires a switch for all midis, got: ~s" (un-@ stx)) stx))
         (syntax-parse #`(#,instrument #,tempo #,instant #,switch)
           [((_ instrument-name:id) (_ tempo*:number) (_ time) (_ on?))
            (define instrument-name* (syntax-e #'instrument-name))
            (cons #`(list 
                      (exact->inexact (/ time (/ tempo* 60)))
                      #,(symbol->string instrument-name*)
                      #,(symbol->string (dict-ref imap instrument-name*))
                      (if on?
                        (format "~a->GetEngineChannel()->SendNoteOn(~a, 80, 0);" '#,instrument-name* num)
                        (format "~a->GetEngineChannel()->SendNoteOff(~a, 80, 0);" '#,instrument-name* num)))
              acc)])]
        [_ acc]))))


(define-composite-linuxsampler-performer linuxsampler-performer {linuxsampler-midi-subperformer})
