; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution enabled-analysis)
 (export
  specify-enabled-analysis)
 (import (rnrs) (rnrs mutable-pairs) (racr core))
 
 ; Constructor for unique entities internally used by the Petrinet Language
 (define-record-type petrinet-nil-record (sealed #t) (opaque #t))
 (define UNDEFINED (make-petrinet-nil-record)) ; Unique entity indicating undefined values
 
 (define specify-enabled-analysis
   (lambda (specification)
     (with-specification
      specification
      
      (ag-rule
       enabled?
       
       (Arc ; Input arcs are enabled if their source place provides the tokens they consume.
        (lambda (n)
          (let ((bindings
                 (map
                  (lambda (consumer-function)
                    (cons UNDEFINED consumer-function)) (ast-child 'functionlabel n)))
                (num-to-bind (length (ast-child 'functionlabel n))))
            (call/cc
             (lambda (abort-search)
               (ast-for-each-child
                (lambda (i token)
                  (let ((binding?
                         (find
                          (lambda (binding)
                            (and
                             (eq? (car binding) UNDEFINED)
                             ((cdr binding) (ast-child 'value token))))
                          bindings)))
                    (when binding?
                      (set-car! binding? token)
                      (set! num-to-bind (- num-to-bind 1))
                      (when (= num-to-bind 0)
                        (abort-search (map car bindings))))))
                (ast-child 'Token* (att-value 'place n)))
               #f)))))
       
       (Transition ; Transitions are enabled if all their input arcs are enabled.
        (lambda (n)
          (call/cc
           (lambda (abort-search)
             (apply
              append
              (map
               (lambda (in)
                 (let ((bindings? (att-value 'enabled? in)))
                   (unless bindings?
                     (abort-search #f))
                   bindings?))
               (ast-children (ast-child 'In n))))))))
       
       (AtomicPetrinet
        (lambda (n)
          (filter
           (lambda (transition)
             (att-value 'enabled? transition))
           (ast-children (ast-child 'Transition* n))))))))))