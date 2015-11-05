; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (composed-petrinets user-interface)
 (export
  make-petrinet
  make-transition
  compose-petrinets)
 (import (rnrs) (racr core) (petrinets analyses))
 
 (define initialize-places
   (lambda (petrinet)
     (ast-for-each-child
      (lambda (i transition)
        (for-each
         (lambda (arc)
           (unless (att-value 'place arc)
             (rewrite-add
              (ast-child 'Place* petrinet)
              (create-ast
               petrinet-specification
               'Place
               (list
                (ast-child 'place arc)
                (create-ast-list (list)))))))
         (append
          (ast-children (ast-child 'In transition))
          (ast-children (ast-child 'Out transition)))))
      (ast-child 'Transition* petrinet))))
 
 (define-syntax make-petrinet
   (lambda (x)
     (define identifier-list?
       (lambda (l)
         (for-all identifier? l)))
     (syntax-case x ()
       ((_ name (inport ...) (outport ...) ((place start-marking ...) ...) transition ... )
        (and
         (identifier? #'name)
         (identifier-list? #'(inport ...))
         (identifier-list? #'(outport ...))
         (identifier-list? #'(place ...)))
        #`(let ((pn
                 (create-ast
                  petrinet-specification
                  'AtomicPetrinet
                  (list
                   'name
                   (create-ast-list
                    (list
                     (create-ast
                      petrinet-specification
                      'Place
                      (list
                       'place
                       (create-ast-list
                        (list
                         (create-ast petrinet-specification 'Token (list start-marking)) ...)))) ...))
                   (create-ast-list
                    (list
                     transition ...))
                   (create-ast-list
                    (list
                     (create-ast petrinet-specification 'InPort (list 'inport)) ...
                     (create-ast petrinet-specification 'OutPort (list 'outport)) ...))))))
            ; Initialize the places without explicit start marking:
            (initialize-places pn)
            ; Ensure, that the petrinet is well-formed:
            (unless (att-value 'well-formed? pn)
              (throw-petrinets-exception "Cannot construct Petri Net; The Petri Net is not well-formed."))
            ; Return the constructed Petri Net:
            pn)))))
 
 (define-syntax make-transition
   (lambda (x)
     (define identifier-list?
       (lambda (l)
         (for-all identifier? l)))
     (syntax-case x ()
       ((k name ((input-place (variable matching-condition) ...) ...) ((output-place to-produce ...) ...))
        (and
         (identifier? #'name)
         (identifier-list? #'(variable ... ...))
         (identifier-list? #'(input-place ...))
         (identifier-list? #'(output-place ...)))
        #`(create-ast
           petrinet-specification
           'Transition
           (list
            'name
            (create-ast-list
             (list
              (create-ast
               petrinet-specification
               'Arc
               (list
                'input-place
                (list
                 (lambda (variable)
                   matching-condition) ...))) ...))
            (create-ast-list
             (list
              (create-ast
               petrinet-specification
               'Arc
               (list
                'output-place
                (lambda (variable ... ...)
                  (list to-produce ...)))) ...))))))))
 
 (define-syntax compose-petrinets
   (lambda (x)
     (syntax-case x ()
       ((_ net1 net2 (((out-net out-port) (in-net in-port)) ...))
        (for-all identifier? #'(out-net ... out-port ... in-net ... in-port ...))
        #'(let* ((net1* net1)
                 (net2* net2)
                 (pn
                  (create-ast
                   petrinet-specification
                   'ComposedPetrinet
                   (list
                    net1*
                    net2*
                    (create-ast-list
                     (list
                      (create-ast
                       petrinet-specification
                       'Glueing
                       (list
                        (cons 'out-net 'out-port)
                        (cons 'in-net 'in-port))) ...))))))
            ; Ensure, that the composed net is well-formed:
            (unless (att-value 'well-formed? pn)
              (throw-petrinets-exception "Cannot compose Petri Nets; The composed net is not well-formed."))
            ; Return the composed net:
            pn))))))