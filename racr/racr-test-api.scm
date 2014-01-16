; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (racr-test-api)
 (export
  assert-exception
  construct-reevaluation-tests)
 (import (rnrs) (racr))
 
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
     (let* ((reevaluation-table (make-hashtable equal-hash equal? 50))
            (equation-preparator
             (lambda (att-name equation)
               (lambda (n)
                 (hashtable-set! reevaluation-table (cons att-name n) #t)
                 (equation n))))
            (reevaluation-tester
             (lambda args
               (define evaluation-checker ; Function asserting whether some attribute instance must be reevaluated or not.
                 (lambda (flushed? n att-name)
                   (for-each
                    (lambda (n)
                      (att-value att-name n) ; Force attribute evaluation or cache-hit!
                      (unless (boolean=? flushed? (hashtable-contains? reevaluation-table (cons att-name n)))
                        (assertion-violation 'prepare-rewrite-tests:influence-tester "RACR Test API: Rewrite test failed!" (list att-name n)))
                      (when flushed?
                        (hashtable-delete! reevaluation-table (cons att-name n))))
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