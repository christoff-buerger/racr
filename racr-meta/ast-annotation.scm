; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (racr-meta ast-annotation)
 (export
  undefined-annotation?
  ast-annotation
  ast-annotation-set!
  ast-annotation-remove!)
 (import (rnrs) (rnrs mutable-pairs) (racr-meta core))
 
 (define annotation-table (make-eq-hashtable))
 
 (define (node-annotations n)
   (hashtable-ref annotation-table n (list)))
 
 (define (node-annotations-set! n annotations)
   (if (null? annotations)
       (hashtable-delete! annotation-table n)
       (hashtable-set! annotation-table n annotations)))
 
 (define-record-type undefined-annotation-record (fields) (opaque #t)(sealed #t))
 (define undefined-annotation (make-undefined-annotation-record))
 
 (define (undefined-annotation? n)
   (eq? n undefined-annotation))
 
 #| TODO: Replace by ast-apply?
 (define (ast-weave-annotations node type name value . constraints)
   (define type-checker
     (case type
       ((list-node) ast-list-node?)
       ((bud-node) ast-bud-node?)
       (else (lambda (n) (and (not (ast-list-node? n)) (not (ast-bud-node? n)) (ast-subtype? n type))))))
   (let loop ((n node))
     (when (and (type-checker n) (for-all (lambda (f) (f n)) constraints))
       (ast-annotation-set! n name value))
     (for-each
      (lambda (child)
        (unless (node-terminal? child)
          (loop child)))
      (node-children n))))|#
 
 (define (ast-annotation-set! node name value)
   (when (not (symbol? name))
     (throw-exception
      "Cannot set " name " annotation; "
      "Annotation names must be Scheme symbols."))
   (when (undefined-annotation? value)
     (throw-exception
      "Cannot set " name " annotation; "
      "'undefined-annotation' as value is prohibited."))
   (when (att-in-evaluation? node)
     (throw-exception
      "Cannot set " name " annotation; "
      "There are attributes in evaluation."))
   (let* ((annotations (node-annotations node))
          (entry? (assq name annotations)))
     (if entry?
         (set-cdr! entry? value)
         (node-annotations-set! node (cons (cons name value) annotations)))))
 
 (define (ast-annotation node name)
   (when (att-in-evaluation? node)
     (throw-exception
      "Cannot access " name " annotation; "
      "There are attributes in evaluation."))
   (let ((entry? (assq name (node-annotations node))))
     (if entry? (cdr entry?) undefined-annotation)))
 
 (define (ast-annotation-remove! node name)
   (when (att-in-evaluation? node)
     (throw-exception
      "Cannot remove " name " annotation; "
      "There are attributes in evaluation."))
   (let loop ((current (node-annotations node))
              (previous? #f))
     (cond
       ((null? current)
        undefined-annotation)
       ((eq? (caar current) name)
        (let ((value (cdar current)))
          (if previous?
              (set-cdr! previous? (cdr current))
              (node-annotations-set! node (cdr current)))
          value))
       (else (loop (cdr current) current))))))
