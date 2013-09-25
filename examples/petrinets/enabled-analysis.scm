; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets enabled-analysis)
 (export
  specify-enabled-analysis)
 (import (rnrs) (rnrs mutable-pairs) (racr))
 
 ; Constructor for unique entities internally used by the Petrinet Language
 (define-record-type petrinet-nil-record (sealed #t) (opaque #t))
 (define UNDEFINED (make-petrinet-nil-record)) ; Unique entity indicating undefined values
 
 (define specify-enabled-analysis
   (lambda (petrinet-specification)
     (with-specification
      petrinet-specification
      
      (ag-rule
       enabled?
       
       ; An arc whose target is a transition is enabled if its source
       ; place, or any place it is fused with, provides the tokens the arc consumes:
       (Arc
        (lambda (n)
          (let ((bindings
                 (map
                  (lambda (consumer-function)
                    (cons UNDEFINED consumer-function))
                  (ast-child 'functionlabel n)))
                (num-to-bind (length (ast-child 'functionlabel n))))
            (call/cc
             (lambda (abort-search)
               (let loop ((place (att-value 'place n))
                          (places-checked (list (att-value 'place n))))
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
                          (abort-search
                           (map
                            car
                            bindings))))))
                  (ast-child 'Token* place))
                 
                 (let* ((inport-glueing? (cdr (att-value 'find-glueings place)))
                        (fused-place?+ (and inport-glueing? (att-value 'place (att-value 'outport inport-glueing?)))))
                   (when (and inport-glueing? (not (memq fused-place?+ places-checked)))
                     (loop fused-place?+ (cons fused-place?+ places-checked))))
                 
                 (let* ((outport-glueing? (car (att-value 'find-glueings place)))
                        (fused-place?- (and outport-glueing? (att-value 'place (att-value 'inport outport-glueing?)))))
                   (when (and outport-glueing? (not (memq fused-place?- places-checked)))
                     (loop fused-place?- (cons fused-place?- places-checked)))))
               #f)))))
       
       ; A transition is enabled if all the arcs which
       ; have the transition as target are enabled:
       (Transition
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
           (ast-children (ast-child 'Transition* n)))))
       
       (ComposedPetrinet
        (lambda (n)
          (append
           (att-value 'enabled? (ast-child 'Net1 n))
           (att-value 'enabled? (ast-child 'Net2 n))))))))))