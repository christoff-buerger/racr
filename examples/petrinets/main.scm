; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets main)
 (export
  print-marking
  print-enabled
  interpreter-repl)
 (import
  (rnrs)
  (rnrs mutable-pairs)
  (racr)
  (petrinets ast)
  (petrinets name-analysis)
  (petrinets well-formedness-analysis)
  (petrinets enabled-analysis)
  (petrinets data-flow-analyses)
  (petrinets ui))
 
 (define print-marking
   (lambda (petrinet)
     (display "Marking:")
     (ast-for-each-child
      (lambda (i place)
        (display "\n\t")
        (display (ast-child 'name place))
        (display ":")
        (ast-for-each-child
         (lambda (i token)
           (display " ")
           (display (ast-child 'value token)))
         (ast-child 'Token* place)))
      (ast-child 'Place* petrinet))
     (display #\newline)))
 
 (define print-enabled
   (lambda (petrinet)
     (display "Enabled:")
     (for-each
      (lambda (transition)
        (display "\n\t")
        (display (ast-child 'name transition)))
      (att-value 'enabled? petrinet))
     (display #\newline)))
 
 (define interpreter-repl ; BEWARE: REPL is tail recursive!
   (lambda (petrinet)
     (display "================================================================================\n")
     (display "                          Petrinet Interperet REPL                              \n")
     (display "================================================================================\n\n")
     (unless (att-value 'well-formed? petrinet)
       (throw-petrinets-exception "Cannot interpret Petrinet; The given petrinet is not well-formed."))
     (let loop ()
       (print-marking petrinet)
       (print-enabled petrinet)
       (display "\nEnter transition to fire (abort with #f):\n")
       (when
           (let loop ((choice (read)))
             (cond
               ((symbol? choice)
                (let ((to-fire (att-value 'find-transition petrinet choice)))
                  (cond
                    ((not to-fire)
                     (display "\tSelection Error: Unknown transition entered; Enter new value:\n")
                     (loop (read)))
                    ((att-value 'enabled? to-fire)
                     (fire-transition! to-fire)
                     choice)
                    (else
                     (display "\tSelection Error: The entered transition is not enabled; Enter new value:\n")
                     (loop (read))))))
               ((not choice)
                choice)
               (else
                (display "\tSelection Error: Enter symbol or #f to abort:\n")
                (loop (read)))))
         (loop)))
     (display "============================= Interpretation Aborted ===========================\n")
     petrinet))
 
 (define fire-transition!
   (lambda (transition)
     (unless (att-value 'enabled? transition)
       (throw-petrinets-exception "Cannot fire transition; The transition is not enabled."))
     (let* ((argument-list
             (map
              (lambda (token)
                (ast-child 'value token))
              (att-value 'enabled? transition))))
       (for-each
        rewrite-delete
        (att-value 'enabled? transition))
       (ast-for-each-child
        (lambda (i out)
          (for-each
           (lambda (new-token)
             (rewrite-add
              (ast-child 'Token* (att-value 'place out))
              (create-ast
               petrinet-spec
               'Token
               (list
                new-token))))
           (apply
            (ast-child 'functionlabel out)
            argument-list)))
        (ast-child 'Out transition)))))
 
 ; Initialize the Petrinet Language:
 (when (= (specification->phase petrinet-spec) 1)
   (specify-ast)
   (specify-name-analysis)
   (specify-well-formedness-analysis)
   (specify-enabled-analysis)
   (specify-data-flow-analyses)
   (compile-ag-specifications petrinet-spec)))