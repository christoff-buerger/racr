; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić, C. Bürger

#!r6rs

(library
 (cjava-racr compositions)
 (export
  specify-compositions
  do-compositions)
 (import (rnrs) (racr core) (cjava-racr exception-api))
 
 (define specify-compositions
   (lambda (specification)
     (with-specification
      specification
      
      (ag-rule
       next-composition
       (CompositionProgram
        (lambda (n)
          (ast-find-child
           (lambda (i n)
             (not (att-value 'exhausted? n)))
           (ast-child 'Composers n)))))
      
      (ag-rule
       exhausted?
       (BindComposer
        (lambda (n)
          (not (att-value 'target n))))))))
 
 (define do-compositions
   (lambda (compilation-unit apply-before-each-composition)
     (let loop ((next-composition (att-value 'next-composition (ast-child 'CompositionProgram compilation-unit))))
       (when next-composition
         (apply-before-each-composition)
         (let ((target (att-value 'target next-composition))
               (source (att-value 'source next-composition)))
           (unless source
             (throw-cjava-racr-exception "Incorrect composition source fragment."))
           (rewrite-subtree target (full-copy source)))
         (loop (att-value 'next-composition (ast-child 'CompositionProgram compilation-unit)))))))
 
 (define full-copy
   (lambda (n)
     (let ((spec (ast-specification n)))
       (with-specification
        spec
        (let loop ((n n))
          (cond
            ((not (ast-node? n))
             n)
            ((ast-list-node? n)
             (create-ast-list
              (map
               loop
               (ast-children n))))
            (else
             (create-ast
              (ast-node-type n)
              (map
               loop
               (ast-children n)))))))))))