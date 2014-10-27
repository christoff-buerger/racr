; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr unparser)
 (export pretty-print)
 (import (rnrs) (racr core) (tinycpp-racr exception-api))
 
 (define qualified-name->string
   (lambda (qualified-name)
     (if (pair? qualified-name)
         (fold-left
          (lambda (result prefix)
            (string-append result "::" (symbol->string prefix)))
          (symbol->string (car qualified-name))
          (cdr qualified-name))
         (symbol->string qualified-name))))
 
 (define pretty-print
   (lambda (compilation-unit out-port)
     (define my-display
       (lambda (m)
         (display m out-port)
         #t))
     
     (define print-indentation
       (lambda (i)
         (when (> i 0)
           (my-display "\t")
           (print-indentation (- i 1)))))
     
     (define print-node
       (lambda (n indent)
         (let ((type (ast-node-type n)))
           (cond
             ((eq? type 'ClassDeclaration)
              (print-indentation indent)
              (my-display "class ")
              (my-display (qualified-name->string (ast-child 'name n)))
              (my-display ";\n"))
             ((eq? type 'ClassDefinition)
              (print-indentation indent)
              (my-display "class ")
              (my-display (qualified-name->string (ast-child 'name n)))
              (my-display #\newline)
              (print-indentation indent)
              (my-display "{\n")
              (print-indentation indent)
              (my-display "public:\n")
              (ast-for-each-child
               (lambda (i n)
                 (print-node n (+ indent 1)))
               (ast-child 'Body n))
              (print-indentation indent)
              (my-display "};\n"))
             ((eq? type 'WovenClassDefinition)
              #f)
             ((eq? type 'MethodDeclaration)
              (print-indentation indent)
              (my-display "static void ")
              (my-display (qualified-name->string (ast-child 'name n)))
              (my-display "(")
              (ast-for-each-child
               (lambda (i c)
                 (print-node c 0)
                 (unless (= i (ast-num-children (ast-child 'Parameters n)))
                   (my-display ", ")))       
               (ast-child 'Parameters n))
              (my-display ")\n")
              (print-indentation indent)
              (my-display "{\n")
              (ast-for-each-child
               (lambda (i n)
                 (print-node n (+ indent 1)))
               (ast-child 'Body n))
              (print-indentation indent)
              (my-display "}\n"))
             ((eq? type 'Constructor)
              (print-indentation indent)
              (my-display (qualified-name->string (ast-child 'name n)))
              (my-display "() { /* Added default constructor */ }\n"))
             ((eq? type 'FieldDeclaration)
              (if (eq? (ast-node-type (ast-parent (ast-parent n))) 'ClassDefinition)
                  (begin
                    (print-indentation indent)
                    (my-display "static int "))
                  (my-display "int "))
              (my-display (qualified-name->string (ast-child 'name n)))
              (when (eq? (ast-node-type (ast-parent (ast-parent n))) 'ClassDefinition)
                (my-display ";\n")))
             ((eq? type 'VariableAssignment)
              (print-indentation indent)
              (print-node (ast-child 'LHand n) 0)
              (my-display " = ")
              (print-node (ast-child 'RHand n) 0)
              (my-display ";\n"))
             ((eq? type 'Reference)
              (my-display (qualified-name->string (ast-child 'name n))))
             (else
              (throw-tinycpp-racr-exception
               (string-append "IMPLEMENTATION ERROR: Pretty printing undefined for node of type [" (symbol->string type) "].")))))))
     
     (ast-for-each-child
      (lambda (i n)
        (when (print-node n 0)
          (my-display #\newline)))
      (ast-child 'Body compilation-unit))
     (my-display "int main()\n{\n}\n"))))