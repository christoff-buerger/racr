; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets ui)
 (export
  make-petrinet
  make-transition
  petrinets-exception?
  throw-petrinets-exception)
 (import (rnrs) (racr) (petrinets ast))
 
 (define-condition-type petrinets-exception &violation make-petrinets-exception petrinets-exception?)
 
 (define throw-petrinets-exception
   (lambda (message)
     (raise-continuable
       (condition
        (make-petrinets-exception)
        (make-message-condition message)))))
 
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
               petrinet-spec
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
       ((_ ((place start-marking ...) ...) transition ... )
        (identifier-list? #'(place ...))
        #`(let ((pn
                 (create-ast
                  petrinet-spec
                  'Petrinet
                  (list
                   (create-ast-list
                    (list
                     (create-ast
                      petrinet-spec
                      'Place
                      (list
                       'place
                       (create-ast-list
                        (list
                         (create-ast petrinet-spec 'Token (list start-marking)) ...)))) ...))
                   (create-ast-list
                    (list
                     transition ...))))))
            ; Ensure, that the petrinet is well-formed:
            (unless (att-value 'well-formed? pn)
              (throw-petrinets-exception "Cannot construct Petrinet; The Petrinet is not well-formed."))
            ; Initialize the places without explicit start marking:
            (initialize-places pn)
            ; Return the constructed Petrinet:
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
        #`(let ((transition
                 (create-ast
                  petrinet-spec
                  'Transition
                  (list
                   'name
                   (create-ast-list
                    (list
                     (create-ast
                      petrinet-spec
                      'Arc
                      (list
                       'input-place
                       (list
                        (lambda (variable)
                          matching-condition) ...))) ...))
                   (create-ast-list
                    (list
                     (create-ast
                      petrinet-spec
                      'Arc
                      (list
                       'output-place
                       (lambda (variable ... ...)
                         (list to-produce ...)))) ...))))))
            (unless (att-value 'unique-input-places? transition)
              (throw-petrinets-exception "Cannot construct transition; The transition's input places are not unique."))
            transition))))))