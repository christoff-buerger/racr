; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets profiling)
 (export make-profiling-net)
 (import (rnrs) (racr core) (atomic-petrinets query-support)
         (atomic-petrinets user-interface)
         (prefix (racket) r:) ;(prefix (racket base) r:)
         )
 
 (define (make-profiling-net $transitions $influenced $local-places $tokens)
   (define (make-name . i)
     (string->symbol (apply string-append "n" (map number->string (map - i)))))
   (define places (list))
   (define transitions (list))
   ;;; Construct segments:
   (do ((index 0 (+ index 1))) ((= index $transitions))
     (let ((out (list))
           (in (list (:Arc (make-name index) (list (lambda (t) t))))))
       (set! places (cons (:Place (make-name index)) places))
       (do ((offset 1 (+ offset 1))) ((= offset $influenced))
         (let ((name (make-name (mod (+ index offset) $transitions))))
           (set! out (cons (:Arc name (lambda x #t)) out))))
       ; Connect local places:
       (do ((offset 1 (+ offset 1))) ((> offset $local-places))
         (let ((name (make-name index offset)))
           (set! places (cons (:Place name) places))
           (set! in (cons (:Arc name (list (lambda (t) t))) in))
           (set! out (cons (:Arc name (lambda x #t)) out))))
       ; Initialise the transition:
       (set! transitions (cons (:Transition (make-name index) in out) transitions))))
   ;;; Construct Petri net:
   (let ((net (:AtomicPetrinet places transitions)))
     (unless (=valid? net)
       (exception: "Cannot construct Petri net; The net is not well-formed."))
     net))
 
 (define (d)
   ;(r:collect-garbage)
   (display (r:current-memory-use)) (display " byte, ")
   (let ((net (r:time (make-profiling-net 560 20 13 3))))
     (display (r:current-memory-use)) (display " byte, ")
     ;(r:dump-memory-stats)
     net))
 
 (initialise-petrinet-language)
 
 )
