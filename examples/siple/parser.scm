; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple parser)
 (export
  construct-parser)
 (import (rnrs) (racr core) (siple exception-api) (siple type) (siple lexer) (siple type-coercion))
 
 (define construct-parser
   (lambda (lexer siple-specification perform-type-coercions?)
     (with-specification
      siple-specification
      (letrec (;;; Parser IO support functions:
               (line 1)
               (column 0)
               (current-token (lexer))
               (read-next-token
                (lambda ()
                  (let ((old-token current-token))
                    (set! current-token (lexer))
                    (set! line (token-line old-token))
                    (set! column (token-column old-token))
                    (token-value old-token))))
               (match-token?
                (lambda (to-match)
                  (eq? (token-type current-token) to-match)))
               (match-token!
                (lambda (to-consume error-message)
                  (if (match-token? to-consume)
                      (read-next-token)
                      (parser-error error-message))))
               (parser-error
                (lambda (message)
                  (throw-siple-exception ; Abort parsing with error message
                   (string-append
                    "Parser Error ("
                    (number->string line)
                    ","
                    (number->string column)
                    ";["
                    (token-value current-token)
                    "]): "
                    message))))
               
               (parse-compilation-unit
                (lambda ()
                  (let loop ((decls (list)))
                    (if (match-token? '*eoi*)
                        (create-ast 'CompilationUnit (list (create-ast-list (reverse decls))))
                        (loop (cons (parse-declaration) decls))))))
               
               (parse-declaration
                (lambda ()
                  (let ((decl
                         (cond
                           ((match-token? 'Procedure) (parse-procedure-decl))
                           ((match-token? 'Var) (parse-variable-decl))
                           (else (parser-error "Malformed program. Unexpected token; Expected declaration.")))))
                    (match-token! 'SEMICOLON "Malformed declaration. Declaration not properly finished; Missing [;].")
                    decl)))
               
               (parse-procedure-decl
                (lambda ()
                  (let ((name #f)
                        (paras (list))
                        (r-type (type-undefined)))
                    (match-token! 'Procedure "Malformed procedure declaration. Missing procedure declaration start delimiter [Procedure].")
                    (set! name (match-token! 'IDENTIFIER "Malformed procedure declaration. Missing name."))
                    (match-token! 'PARENTHESIS-OPEN "Malformed procedure declaration. Missing parameter list start delimiter [(].")
                    (if (match-token? 'Var)
                        (set! paras
                              (let loop ((paras (list (parse-variable-decl))))
                                (if (match-token? 'COMMA)
                                    (begin
                                      (read-next-token) ; Consume the ","
                                      (loop (cons (parse-variable-decl) paras)))
                                    (reverse paras)))))
                    (match-token! 'PARENTHESIS-CLOSE "Malformed procedure declaration. Parameter list not properly finished; Missing [)].")
                    (if (match-token? 'COLON)
                        (begin
                          (read-next-token) ; Consume the ":"
                          (set! r-type (parse-type))))
                    (create-ast 'ProcedureDeclaration (list name (create-ast-list paras) r-type (parse-block))))))
               
               (parse-variable-decl
                (lambda ()
                  (let ((name #f))
                    (match-token! 'Var "Malformed variable declaration. Missing variable declaration start delimiter [Var].")
                    (set! name (match-token! 'IDENTIFIER "Malformed variable declaration. Missing name."))
                    (match-token! 'COLON "Malformed variable declaration. Missing [:].")
                    (create-ast 'VariableDeclaration (list name (parse-type))))))
               
               (parse-block
                (lambda ()
                  (let ((block #f))
                    (match-token! 'Begin "Malformed block. Missing block start delimiter [Begin].")
                    (set! block (parse-block-content (list 'End)))
                    (match-token! 'End "Malformed block. Block not properly finished; Missing [End].")
                    block)))
               
               (parse-block-content
                (lambda (end-delimiters)
                  (let loop ((content (list)))
                    (if (find
                         (lambda (delimiter)
                           (match-token? delimiter))
                         end-delimiters)
                        (create-ast 'Block (list (create-ast-list (reverse content))))
                        (loop (cons (parse-statement) content))))))
               
               (parse-statement
                (lambda ()
                  (if (or (match-token? 'Var) (match-token? 'Procedure))
                      (parse-declaration)
                      (let ((stmt
                             (cond
                               ((match-token? 'Begin)
                                (parse-block))
                               ((match-token? 'If)
                                (let ((cond #f)
                                      (then #f)
                                      (else (list)))
                                  (read-next-token) ; Consume the "If"
                                  (set! cond (parse-expression))
                                  (match-token! 'Then "Malformed if. Missing [Then].")
                                  (set! then (parse-block-content (list 'Else 'Fi)))
                                  (if (match-token? 'Else)
                                      (begin
                                        (read-next-token) ; Consume the "Else"
                                        (set! else (list (parse-block-content (list 'Fi))))))
                                  (match-token! 'Fi "Malformed if. If not properly finished; Missing [Fi].")
                                  (create-ast 'If (list cond then (create-ast-list else)))))
                               ((match-token? 'While)
                                (let ((cond #f)
                                      (body #f))
                                  (read-next-token) ; Consume the "While"
                                  (set! cond (parse-expression))
                                  (match-token! 'Do "Malformed loop. Missing body start delimiter [Do].")
                                  (set! body (parse-block-content (list 'Od)))
                                  (match-token! 'Od "Malformed loop. Loop not properly finished; Missing [Od].")
                                  (create-ast 'While (list cond body))))
                               ((match-token? 'IDENTIFIER)
                                (let ((ref (create-ast 'Reference (list (read-next-token)))))
                                  (if (match-token? 'ASSIGNMENT)
                                      (begin
                                        (read-next-token) ; Consume the ":="
                                        (create-ast 'VariableAssignment (list ref (parse-expression))))
                                      (parse-procedure-call-by-name (create-ast 'Dereference (list ref))))))
                               ((match-token? 'Deref)
                                (let ((l-hand (parse-l-value-expression)))
                                  (match-token! 'ASSIGNMENT "Malformed assignment. Missing [:=].")
                                  (create-ast 'VariableAssignment (list l-hand (parse-expression)))))
                               ((match-token? 'Call)
                                (parse-procedure-call-by-Call))
                               ((match-token? 'Return)
                                (read-next-token) ; Consume the "Return"
                                (if (not (match-token? 'SEMICOLON))
                                    (create-ast 'ProcedureReturn (list (create-ast-list (list (parse-expression)))))
                                    (create-ast 'ProcedureReturn (list (create-ast-list (list))))))
                               ((match-token? 'Write)
                                (read-next-token) ; Consume the "Write"
                                (create-ast 'Write (list (parse-expression))))
                               ((match-token? 'Read)
                                (read-next-token) ; Consume the "Read"
                                (create-ast 'Read (list (parse-l-value-expression))))
                               ((match-token? 'Assert)
                                (read-next-token) ; Consume the "Assert"
                                (create-ast 'Assertion (list (parse-expression))))
                               (else (parser-error "Malformed statement. Unexpected token.")))))
                        (match-token! 'SEMICOLON "Malformed statement. Statement not properly finished; Missing [;].")
                        stmt))))
               
               (parse-procedure-call
                (lambda ()
                  (cond
                    ((match-token? 'IDENTIFIER)
                     (parse-procedure-call-by-name (create-ast 'Dereference (list (create-ast 'Reference (list (read-next-token)))))))
                    (else (parse-procedure-call-by-Call)))))
               
               (parse-procedure-call-by-Call
                (lambda ()
                  (let ((proc-expr #f)
                        (args (list)))
                    (match-token! 'Call "Malformed explicit procedure call. Missing explicit procedure call start delimiter [Call].")
                    (match-token! 'PARENTHESIS-OPEN "Malformed explicit procedure call. Missing [(].")
                    (set! proc-expr (parse-expression))
                    (if (match-token? 'COLON)
                        (begin
                          (read-next-token) ; Consume the ":"
                          (set! args (parse-procedure-call-arguments))))
                    (match-token! 'PARENTHESIS-CLOSE "Malformed explicit procedure call. Explicit procedure call not properly finished; Missing [)].")
                    (create-ast 'ProcedureCall (list proc-expr (create-ast-list args))))))
               
               (parse-procedure-call-by-name
                (lambda (procedure-reference)
                  (let ((args #f))
                    (match-token! 'PARENTHESIS-OPEN "Malformed procedure call. Missing argument list start delimiter [(].")
                    (set! args (parse-procedure-call-arguments))
                    (match-token! 'PARENTHESIS-CLOSE "Malformed procedure call. Argument list not properly finished; Missing [)].")
                    (create-ast 'ProcedureCall (list procedure-reference (create-ast-list args))))))
               
               (parse-procedure-call-arguments
                (lambda ()
                  (if (match-token? 'PARENTHESIS-CLOSE)
                      (list)
                      (let loop ((args (list (parse-expression))))
                        (if (match-token? 'COMMA)
                            (begin
                              (read-next-token) ; Consume the ","
                              (loop (cons (parse-expression) args)))
                            (reverse args))))))
               
               (parse-l-value-expression
                (lambda ()
                  (if (match-token? 'IDENTIFIER)
                      (create-ast 'Reference (list (read-next-token)))
                      (let ((operand #f))
                        (match-token! 'Deref "Malformed l-value expression. Unexpected token; Expected [identifier] or [Deref].")
                        (match-token! 'PARENTHESIS-OPEN "Malformed dereference expression. Missing [(].")
                        (set! operand (parse-l-value-expression))
                        (match-token! 'PARENTHESIS-CLOSE "Malformed dereference expression. Dereference expression not properly finished; Missing [)].")
                        (create-ast 'Dereference (list operand))))))
               
               (parse-expression
                (lambda ()
                  (parse-or-expression)))
               
               (parse-or-expression
                (lambda ()
                  (let loop ((operand1 (parse-and-expression)))
                    (cond
                      ((match-token? 'Or)
                       (read-next-token) ; Consume the "Or"
                       (loop (create-ast 'Or (list operand1 (parse-and-expression)))))
                      (else operand1)))))
               
               (parse-and-expression
                (lambda ()
                  (let loop ((operand1 (parse-eq-expression)))
                    (cond
                      ((match-token? 'And)
                       (read-next-token) ; Consume the "And"
                       (loop (create-ast 'And (list operand1 (parse-eq-expression)))))
                      (else operand1)))))
               
               (parse-eq-expression
                (lambda ()
                  (let loop ((operand1 (parse-rel-expression)))
                    (cond
                      ((match-token? 'EQUAL)
                       (read-next-token) ; Consume the "="
                       (loop (create-ast 'Equal (list operand1 (parse-rel-expression)))))
                      ((match-token? 'NOT-EQUAL)
                       (read-next-token) ; Consume the "#"
                       (loop (create-ast 'Not (list (create-ast 'Equal (list operand1 (parse-rel-expression)))))))
                      (else operand1)))))
               
               (parse-rel-expression
                (lambda ()
                  (let loop ((operand1 (parse-add-expression)))
                    (cond
                      ((match-token? 'LESSER)
                       (read-next-token) ; Consume the "<"
                       (loop (create-ast 'LesserThan (list operand1 (parse-add-expression)))))
                      ((match-token? 'GREATER)
                       (read-next-token) ; Consume the ">"
                       (loop (create-ast 'GreaterThan (list operand1 (parse-add-expression)))))
                      ((match-token? 'LESSER-EQUAL)
                       (read-next-token) ; Consume the "<="
                       (loop (create-ast 'LesserThanEqual (list operand1 (parse-add-expression)))))
                      ((match-token? 'GREATER-EQUAL)
                       (read-next-token) ; Consume the ">="
                       (loop (create-ast 'GreaterThanEqual (list operand1 (parse-add-expression)))))
                      (else operand1)))))
               
               (parse-add-expression
                (lambda ()
                  (let loop ((operand1 (parse-mul-expression)))
                    (cond
                      ((match-token? 'ADDITION)
                       (read-next-token) ; Consume the "+"
                       (loop (create-ast 'Addition (list operand1 (parse-mul-expression)))))
                      ((match-token? 'SUBTRACTION)
                       (read-next-token) ; Consume the "-"
                       (loop (create-ast 'Subtraction (list operand1 (parse-mul-expression)))))
                      (else operand1)))))
               
               (parse-mul-expression
                (lambda ()
                  (let loop ((operand1 (parse-unary-expression)))
                    (cond
                      ((match-token? 'MULTIPLICATION)
                       (read-next-token) ; Consume the "*"
                       (loop (create-ast 'Multiplication (list operand1 (parse-unary-expression)))))
                      ((match-token? 'DIVISION)
                       (read-next-token) ; Consume the "/"
                       (loop (create-ast 'Division (list operand1 (parse-unary-expression)))))
                      (else operand1)))))
               
               (parse-unary-expression
                (lambda ()
                  (cond
                    ((match-token? 'SUBTRACTION)
                     (read-next-token) ; Consume the "-"
                     (create-ast 'UMinus (list (parse-primary-expression))))
                    ((match-token? 'Not)
                     (read-next-token) ; Consume the "Not"
                     (create-ast 'Not (list (parse-primary-expression))))
                    (else (parse-primary-expression)))))
               
               (parse-primary-expression
                (lambda ()
                  (cond
                    ((match-token? 'CONSTANT)
                     (create-ast 'Constant (list (read-next-token))))
                    ((match-token? 'Deref)
                     (let ((operand #f))
                       (read-next-token) ; Consume the "Deref"
                       (match-token! 'PARENTHESIS-OPEN "Malformed dereference expression. Missing [(].")
                       (set! operand (parse-expression))
                       (match-token! 'PARENTHESIS-CLOSE "Malformed dereference expression. Dereference expression not properly finished; Missing [)].")
                       (create-ast 'Dereference (list operand))))
                    ((match-token? 'Address)
                     (let ((name #f))
                       (read-next-token) ; Consume the "Address"
                       (match-token! 'PARENTHESIS-OPEN "Malformed address-of expression. Missing [(].")
                       (set! name (match-token! 'IDENTIFIER "Malformed address-of expression. Missing name of the entity to address."))
                       (match-token! 'PARENTHESIS-CLOSE "Malformed address-of expression. Address-of expression not properly finished; Missing [)].")
                       (create-ast 'Reference (list name))))
                    ((match-token? 'PARENTHESIS-OPEN)
                     (let ((expr #f))
                       (read-next-token) ; Consume the "("
                       (set! expr (parse-expression))
                       (match-token! 'PARENTHESIS-CLOSE "Malformed nested expression. Nested expression not properly finished; Missing [)].")
                       expr))
                    ((match-token? 'IDENTIFIER)
                     (let ((value (create-ast 'Dereference (list (create-ast 'Reference (list (read-next-token)))))))
                       (if (match-token? 'PARENTHESIS-OPEN)
                           (parse-procedure-call-by-name value)
                           value)))
                    ((match-token? 'Call)
                     (parse-procedure-call-by-Call))
                    (else (parser-error "Malformed expression. Unexpected token.")))))
               
               (parse-type
                (lambda ()
                  (cond
                    ((match-token? 'Boolean)
                     (read-next-token) ; Consume the "Boolean"
                     (type-boolean))
                    ((match-token? 'Integer)
                     (read-next-token) ; Consume the "Integer"
                     (type-integer))
                    ((match-token? 'Real)
                     (read-next-token) ; Consume the "Real"
                     (type-real))
                    ((match-token? 'Pointer)
                     (let ((pointed-to-type #f))
                       (read-next-token) ; Consume the "Pointer"
                       (match-token! 'PARENTHESIS-OPEN "Malformed pointer type. Missing [(].")
                       (set! pointed-to-type (parse-type))
                       (match-token! 'PARENTHESIS-CLOSE "Malformed pointer type. Pointer type not properly finished; Missing [)].")
                       (type-pointer pointed-to-type)))
                    ((match-token? 'Procedure)
                     (let ((rtype (type-undefined))
                           (paras (list)))
                       (read-next-token) ; Consume the "Procedure"
                       (match-token! 'PARENTHESIS-OPEN "Malformed procedure type. Missing parameter list start delimiter [(].")
                       (if (not (match-token? 'PARENTHESIS-CLOSE))
                           (set! paras
                                 (let loop ((paras (list (parse-type))))
                                   (if (match-token? 'COMMA)
                                       (begin
                                         (read-next-token) ; Consume the ","
                                         (loop (cons (parse-type) paras)))
                                       (reverse paras)))))
                       (match-token! 'PARENTHESIS-CLOSE "Malformed procedure type. Parameter list not properly finished; Missing [)].")
                       (if (match-token? 'COLON)
                           (begin
                             (read-next-token) ; Consume the ":"
                             (set! rtype (parse-type))))
                       (type-procedure rtype paras)))
                    (else (parser-error "Malformed type. Unexpected token."))))))
        ;;; Return parser function:
        (lambda ()
          (let ((ast (parse-compilation-unit)))
            (when perform-type-coercions?
              (perform-type-coercions ast siple-specification))
            ast)))))))