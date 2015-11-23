; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets profiling)
 (export profiling-net profile)
 (import (rnrs) (racr core) (racr testing) (atomic-petrinets analyses) (atomic-petrinets user-interface))
 
 (define (make-profiling-net $transitions $influenced $local-places $tokens)
   (define (make-name . i)
     (string->symbol (apply string-append "n" (map number->string (map - i)))))
   (define (make-local-tokens)
     (let loop ((i $tokens))
       (if (<= i 0) (list (:Token #t)) (cons (:Token #f) (loop (- i 1))))))
   (define (make-control-tokens index)
     (let loop ((i (- $influenced 1 index)))
       (if (<= i 0) (list) (cons (:Token #t) (loop (- i 1))))))
   (define consumers
     (let loop ((i (- $influenced 1)))
       (if (<= i 0) (list) (cons (lambda (t) t) (loop (- i 1))))))
   (define places (list))
   (define transitions (list))
   ;;; Construct segments:
   (do ((index 0 (+ index 1))) ((= index $transitions))
     (let ((out (list))
           (in (list (:Arc (make-name index) consumers))))
       (set! places (cons (apply :Place (make-name index) (make-control-tokens index)) places))
       (do ((offset 1 (+ offset 1))) ((>= offset $influenced))
         (let ((name (make-name (mod (+ index offset) $transitions))))
           (set! out (cons (:Arc name (lambda x (list #t))) out))))
       ; Connect local places:
       (do ((offset 1 (+ offset 1))) ((> offset $local-places))
         (let ((name (make-name index offset)))
           (set! places (cons (apply :Place name (make-local-tokens)) places))
           (set! in (cons (:Arc name (list (lambda (t) t))) in))
           (set! out (cons (:Arc name (lambda x (list #t))) out))))
       ; Initialise the transition:
       (set! transitions (cons (:Transition (make-name index) in out) transitions))))
   ;;; Construct and prepare Petri net:
   (let ((net (:AtomicPetrinet places transitions)))
     (unless (=valid? net) (raise "Cannot construct Petri net; The net is not well-formed."))
     (for-each =enabled? (=transitions net))
     net))
 
 (define profiling-net
   (begin
     (initialise-petrinet-language)
     (include "profiling-net-configuration.scm" (make-profiling-net 560 20 13 3))))
 
 (define (profile $executions)
   (do ((i 0 (+ i 1))) ((>= i $executions))
     (fire-transition! (find =enabled? (=transitions profiling-net))))))