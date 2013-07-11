; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets main)
 (export
  print-marking
  print-enabled
  interpreter-repl
  fire-transition!
  run-petrinet!)
 (import
  (rnrs)
  (rnrs mutable-pairs)
  (racr)
  (petrinets ast)
  (petrinets access-support)
  (petrinets name-analysis)
  (petrinets composition-analysis)
  (petrinets well-formedness-analysis)
  (petrinets enabled-analysis)
  (petrinets ui))
 
 (define print-marking
   (lambda (petrinet)
     (if (ast-subtype? petrinet 'ComposedPetrinet)
         (begin
           (print-marking (ast-child 'Net1 petrinet))
           (print-marking (ast-child 'Net2 petrinet)))
         (begin
           (display "Marking of [")
           (display (ast-child 'name petrinet))
           (display "]:")
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
           (display #\newline)))))
 
 (define print-enabled
   (lambda (petrinet)
     (if (ast-subtype? petrinet 'ComposedPetrinet)
         (begin
           (print-enabled (ast-child 'Net1 petrinet))
           (print-enabled (ast-child 'Net2 petrinet)))
         (begin
           (display "Enabled of [")
           (display (ast-child 'name petrinet))
           (display "]:")
           (for-each
            (lambda (transition)
              (display "\n\t")
              (display (ast-child 'name transition)))
            (att-value 'enabled? petrinet))
           (display #\newline)))))
 
 (define interpreter-repl
   (lambda (petrinet)
     (display "================================================================================\n")
     (display "                         Petri Net Interpreter REPL                             \n")
     (display "================================================================================\n\n")
     (unless (att-value 'well-formed? petrinet)
       (throw-petrinets-exception "Cannot interpret Petrinet; The given petrinet is not well-formed."))
     (call/cc
      (lambda (abort)
        (let loop ()
          (print-marking petrinet)
          (print-enabled petrinet)
          (display "\nEnter transition to fire (abort with #f):\n")
          (let loop ((choice (read)))
            (cond
              ((and (pair? choice) (symbol? (car choice)) (symbol? (cdr choice)))
               (let* ((subnet? (att-value 'find-subnet petrinet (car choice)))
                      (to-fire? (and subnet? (att-value 'find-transition subnet? (cdr choice)))))
                 (cond
                   ((not subnet?)
                    (display "\tSelection Error: Unknown sub net; Enter new value:\n")
                    (loop (read)))
                   ((not to-fire?)
                    (display "\tSelection Error: Unknown transition entered; Enter new value:\n")
                    (loop (read)))
                   ((att-value 'enabled? to-fire?)
                    (fire-transition! to-fire?))
                   (else
                    (display "\tSelection Error: The entered transition is not enabled; Enter new value:\n")
                    (loop (read))))))
              ((not choice)
               (abort))
              (else
               (display "\tSelection Error: Enter net and transitition pair, e.g., (my-net . my-transition) or #f to abort:\n")
               (loop (read)))))
          (loop))))
     (display "============================= Interpretation Aborted ===========================\n")
     petrinet))
 
 (define run-petrinet!
   (lambda (petrinet)
     (unless (att-value 'well-formed? petrinet)
       (throw-petrinets-exception "Cannot run Petri Net; The given net is not well-formed."))
     (let ((enabled (att-value 'enabled? petrinet)))
       (unless (null? enabled)
         (fire-transition! (car enabled))
         (run-petrinet! petrinet)))))
 
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
   (specify-access-support)
   (specify-name-analysis)
   (specify-composition-analysis)
   (specify-well-formedness-analysis)
   (specify-enabled-analysis)
   (compile-ag-specifications petrinet-spec)))