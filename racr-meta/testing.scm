; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (racr-meta testing)
 (export
  print-ast
  print-specification
  include
  assert-exception
  construct-reevaluation-tests)
 (import (rnrs) (racr-meta core))
 
 ; Given an AST, an association list L of attribute pretty-printers and an output port, print a
 ; human-readable text representation of the AST on the output port. The elements of the association list
 ; L are (attribute-name pretty-printing-function) pairs. Every attribute for which L contains an entry is
 ; printed when the AST node it is associated with is printed. Thereby, the given pretty printing function
 ; is applied to the attribute's value before printing it. Beware: The output port is never closed by this
 ; function - neither in case of an io-exception nor after finishing printing the AST.
 (define (print-ast ast attribute-pretty-printer-list output-port)
   (define (print-indentation i)
     (if (> i 0)
         (begin
           (print-indentation (- i 1))
           (my-display " |"))
         (my-display #\newline)))
   (define (my-display to-display)
     (display to-display output-port))
   (unless (ast-node? ast)
     (throw-exception "Wrong ast argument; Expected AST node."))
   (let loop ((n ast)
              (ast-depth 0)
              (context? #f))
     (cond
       ((and context? (not (symbol->non-terminal? context?))) ; Print terminal
        (print-indentation ast-depth)
        (my-display "> ")
        (my-display (symbol->string (symbol->name context?)))
        (my-display ": ")
        (my-display n))
       ((ast-list-node? n) ; Print list nodes
        (print-indentation ast-depth)
        (print-indentation ast-depth)
        (my-display "-* ")
        (when context?
          (let ((element-type (symbol->string (symbol->name context?)))
                (context-name (symbol->string (symbol->context-name context?))))
            (my-display element-type)
            (unless (string=? (string-append element-type "*") context-name)
              (my-display " (")
              (my-display context-name)
              (my-display ")"))))
        (ast-for-each-child
         (lambda (i n)
           (loop n (+ ast-depth 1) context?))
         n))
       ((ast-bud-node? n) ; Print bud nodes
        (print-indentation ast-depth)
        (print-indentation ast-depth)
        (my-display "-< bud-node >"))
       (else ; Print non-terminal
        (print-indentation ast-depth)
        (print-indentation ast-depth)
        (my-display "-\\ ")
        (my-display (symbol->string (ast-node-type n)))
        (cond
          ((not context?) #f)
          ((symbol->kleene? context?)
           (my-display " (")
           (my-display (symbol->string (symbol->context-name context?)))
           (my-display " element)"))
          ((not (eq? (ast-node-type n) (symbol->context-name context?)))
           (my-display " (")
           (my-display (symbol->string (symbol->context-name context?)))
           (my-display ")")))
        (for-each
         (lambda (att-def)
           (define name (attribute->name att-def))
           (define pretty-printer-entry (assq name attribute-pretty-printer-list))
           (when pretty-printer-entry
             (print-indentation (+ ast-depth 1))
             (my-display "@ ")
             (my-display (symbol->string name))
             (my-display ": ")
             (my-display ((cdr pretty-printer-entry) (att-value name n)))))
         (append
          (if context? (symbol->attributes context?) (list))
          (symbol->attributes (car (ast-rule->production (ast-node-rule n))))))
        (for-each
         (lambda (symb n)
           (loop n (+ ast-depth 1) symb))
         (cdr (ast-rule->production (ast-node-rule n)))
         (ast-children n)))))
   (my-display #\newline))
 
 ; Version of print-ast specialised to pretty print RACR specifications on the current output port.
 (define (print-specification spec)
   (print-ast
    (racr-specification-2-ast-scheme spec)
    (list
     (cons 'derivable
           (lambda (v) (map (lambda (n) (ast-child 'name n)) v)))
     (cons 'local-correct?
           (lambda (v) (and v #t))))
    (current-output-port)))
 
 ; Syntax form expanding to a begin expression containing the forms of a given source code file.
 (define-syntax include
   (lambda (x)
     (define (read-file fn k)
       (define p (open-input-file fn))
       (let loop ((x (read p)))
         (if (eof-object? x)
             (begin (close-port p) '())
             (cons (datum->syntax k x) (loop (read p))))))
     (syntax-case x ()
       ((_ filename default)
        (let ((fn (syntax->datum #'filename)))
          (if (file-exists? fn) #'(include filename) #'default)))
       ((k filename)
        (let ((fn (syntax->datum #'filename)))
          (with-syntax (((expr ...) (read-file fn #'k)))
            #'(begin expr ...)))))))
 
 ; Syntax form used to assert, that the evaluation of some expression fails with an exception.
 (define-syntax assert-exception
   (syntax-rules ()
     ((_ expr)
      (assert-exception condition? expr))
     ((_ exception-type-checker expr)
      (assert
       (call/cc
        (lambda (k)
          (with-exception-handler
           (lambda (exc)
             (if (exception-type-checker exc)
                 (k #t)
                 (raise exc)))
           (lambda ()
             expr
             #f))))))))
 
 ; Function returning two functions that can be used to (1) prepare and (2) perform tests about attribute reevaluation. Such tests are
 ; usefule to ensure that only certain attribute instances must be reevaluated after a rewrite whereas others are not influenced. The first
 ; returned function is used to prepare attribute equations for reevaluation testing. The equation of any attribute to test must be adapted by
 ; using the preparation function. The second function returned is used to test if attribute instances are reevaluated or if their value
 ; already is cached. For that purpose, the test function expects an arbitrary long sequence of attribute names and AST nodes. The respective
 ; attribute instances are the ones that must be reevaluated; the value of any other prepared instance must already be computed. If this is not
 ; the case, the test function throws an exception.
 (define construct-reevaluation-tests
   (lambda ()
     (let* ((reevaluation-table (make-eq-hashtable 50))
            (equation-preparator
             (lambda (att-name equation)
               (lambda (n)
                 (hashtable-update! reevaluation-table att-name (lambda (v) (cons n v)) (list))
                 (equation n))))
            (reevaluation-tester
             (lambda args
               (define evaluation-checker ; Function asserting whether some attribute instance must be reevaluated or not.
                 (lambda (flushed? n att-name)
                   (for-each
                    (lambda (n)
                      (att-value att-name n) ; Force attribute evaluation or cache-hit!
                      (unless (boolean=? flushed? (and (memq n (hashtable-ref reevaluation-table att-name (list))) #t))
                        (assertion-violation 'prepare-rewrite-tests:influence-tester "RACR Test API: Rewrite test failed!" (list att-name n)))
                      (when flushed?
                        (hashtable-update! reevaluation-table att-name (lambda (v) (remq n v)) (list))))
                    (if (list? n) n (list n)))))
               ; Ensure, that the specified influenced attributes are node/attribute name combinations:
               (unless (even? (length args))
                 (assertion-violation 'prepare-rewrite-tests:influence-tester "RACR Test API: Unexpected number of arguments." args))
               ; Ensure, that the specified influenced attributes have been flushed:
               (let loop ((args args))
                 (unless (null? args)
                   (evaluation-checker #t (car args) (cadr args))
                   (loop (cddr args))))
               ; Ensure, that no further attributes have been flushed (i.e., traverse the complete AST and
               ; trigger the evaluation of all its attributes, checking that non is reevaluated):
               (let loop ((n
                           (let loop ((n (car args)))
                             (if (ast-has-parent? n)
                                 (loop (ast-parent n))
                                 n)))
                          (inh-atts (list)))
                 (let* ((prod
                         (ast-rule->production
                          (specification->find-ast-rule
                           (ast-specification n)
                           (ast-node-type n))))
                        (syn-atts (symbol->attributes (car prod))))
                   (for-each
                    (lambda (att)
                      (evaluation-checker #f n (attribute->name att)))
                    (append inh-atts syn-atts))
                   (ast-for-each-child
                    (lambda (i n)
                      (when (ast-node? n)
                        (let ((n* (if (ast-list-node? n) (ast-children n) (list n))))
                          (for-each
                           (lambda (n)
                             (loop n (symbol->attributes (list-ref prod i))))
                           n*))))
                    n))))))
       ; Return the preparation and reevaluation testing functions:
       (values equation-preparator reevaluation-tester)))))