; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (racr test-api)
 (export
  construct-reevaluation-tests)
 (import (rnrs) (racr))
 
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
             (lambda (invariant . args)
               (define evaluation-checker
                 (lambda (flushed? . args)
                   (unless (null? args)
                     (let ((n (car args))
                           (att-name (cadr args)))
                       (for-each
                        (lambda (n)
                          (att-value att-name n) ; Force attribute evaluation or cache-hit
                          (unless (boolean=? flushed? (hashtable-contains? reevaluation-table (cons att-name n)))
                            (assertion-violation 'prepare-rewrite-tests:influence-tester "RACR Test API: Rewrite test failed!" (list att-name n)))
                          (when flushed?
                            (hashtable-delete! reevaluation-table (cons att-name n))))
                        (if (list? n) n (list n)))
                       (apply evaluation-checker flushed? (cddr args))))))
               (unless (even? (length args))
                 (assertion-violation 'prepare-rewrite-tests:influence-tester "RACR Test API: Unexpected number of arguments." args))
               ; Ensure, that the specified influenced attributes have been flushed:
               (apply evaluation-checker #t args)
               ; Ensure, that no further attributes have been flushed:
               (apply evaluation-checker #f invariant))))
       (values equation-preparator reevaluation-tester)))))