; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić, C. Bürger

#!r6rs

(library
 (cjava-racr unparser)
 (export pretty-print)
 (import (rnrs) (racr core))
 
 (define qualified-name->string
   (lambda (qualified-name)
     (fold-left
      (lambda (result symbol)
        (string-append result "." (symbol->string symbol)))
      (symbol->string (car qualified-name))
      (cdr qualified-name))))
 
 (define pretty-print
   (lambda (n out-port)
     (define my-display
       (lambda (m)
         (display m out-port)))
     (define print-indentation
       (lambda (i)
         (when (> i 0)
           (my-display "\t")
           (print-indentation (- i 1)))))
     (let loop ((n n)
                (indent 0))
       (print-indentation indent)
       (let ((type (ast-node-type n)))
         (cond
           ((eq? type 'ClassDeclaration)
            (if (eq? (ast-node-type (ast-parent (ast-parent n))) 'CompilationUnit)
                (my-display "public class ")
                (my-display "public static class "))
            (my-display (symbol->string (ast-child 'name n)))
            (my-display " {\n")
            (ast-for-each-child
             (lambda (i n)
               (loop n (+ indent 1)))
             (ast-child 'Body n))
            (print-indentation indent)
            (my-display "}\n"))
           ((eq? type 'MethodDeclaration)
            (my-display "public static void ")
            (my-display (symbol->string (ast-child 'name n)))
            (my-display "(")
            (ast-for-each-child
             (lambda (i c)
               (loop c 0)
               (unless (= i (ast-num-children (ast-child 'Parameters n)))
                 (my-display ", ")))       
             (ast-child 'Parameters n))
            (my-display ") {\n")
            (ast-for-each-child
             (lambda (i n)
               (loop n (+ indent 1))
               (my-display ";\n"))
             (ast-child 'Body n))
            (print-indentation indent)
            (my-display "}\n"))
           ((eq? type 'FieldDeclaration)
            (if (eq? (ast-node-type (ast-parent (ast-parent n))) 'ClassDeclaration)
                (my-display "public static int ")
                (my-display "int "))
            (my-display (symbol->string (ast-child 'name n)))
            (when (eq? (ast-node-type (ast-parent (ast-parent n))) 'ClassDeclaration)
              (my-display ";\n")))
           ((eq? type 'VariableAssignment)
            (loop (ast-child 'LHand n) 0)
            (my-display " = ")
            (loop (ast-child 'RHand n) 0))
           ((eq? type 'Reference)
            (my-display (qualified-name->string (ast-child 'name n))))
           ((eq? type 'DeclarationHook)
            (my-display "[[")
            (my-display (symbol->string (ast-child 'name n)))
            (my-display "]]\n"))))))))