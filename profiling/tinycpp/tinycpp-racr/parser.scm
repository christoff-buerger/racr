; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr parser)
 (export
  construct-parser)
 (import (rnrs) (racr core) (tinycpp-racr exception-api) (tinycpp-racr lexer))
 
 (define construct-parser
   (lambda (lexer specification)
     (with-specification
      specification
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
                  (throw-tinycpp-racr-exception ; Abort parsing with error message
                   (string-append
                    "Parser Error ("
                    (number->string line)
                    ","
                    (number->string column)
                    ";["
                    (symbol->string (token-type current-token))
                    ","
                    (token-value current-token)
                    "]): "
                    message))))
               
               (global-index 0)
               
               (parse-compilation-unit
                (lambda ()
                  (create-ast
                   'CompilationUnit
                   (list
                    (create-ast-list
                     (reverse
                      (let loop ((body (list)))
                        (if (match-token? 'Class)
                            (begin
                              (set! global-index (+ global-index 1))
                              (loop (cons (parse-class-declaration #f) body)))
                            (begin
                              (match-token! 'Int "Malformed main method.")
                              (unless (string=? (match-token! 'IDENTIFIER "Malformed main method.") "main")
                                (parser-error "Malformed main method."))
                              (match-token! 'PARENTHESIS-OPEN "Malformed main method.")
                              (match-token! 'PARENTHESIS-CLOSE "Malformed main method.")
                              (match-token! 'BRACE-OPEN "Malformed main method.")
                              (match-token! 'BRACE-CLOSE "Malformed main method.")
                              (match-token! '*eoi* "Malformed main method.")
                              body)))))
                    (token-source current-token)))))
               
               (parse-class-declaration
                (lambda (inner-class?)
                  (let ((name #f)
                        (body #f)
                        (result #f))
                    (match-token! 'Class "Malformed class declaration. Missing class declaration start delimiter [class].")
                    (set! name (parse-qualified-name))
                    (when (and inner-class? (pair? name))
                      (parser-error "Malformed class declaration. Inner class declarations cannot have qualified names."))
                    (if (match-token? 'BRACE-OPEN)
                        (begin
                          (read-next-token) ; Consume the "{"
                          (match-token! 'Public "Malformed class definition. Missing public section start delimiter [public].")
                          (match-token! 'COLON "Malformed class definition. Missing public section start delimiter [:].")
                          (set!
                           body
                           (reverse
                            (let loop ((body (list)))
                              (if (not (match-token? 'BRACE-CLOSE))
                                  (loop (cons (parse-class-member-declaration) body))
                                  body))))
                          (read-next-token) ; Consume the "}"
                          (set! result (create-ast 'ClassDefinition (list name global-index (create-ast-list body)))))
                        (if (pair? name)
                            (parser-error "Malformed class definition. Missing class body.")
                            (set! result (create-ast 'ClassDeclaration (list name global-index)))))
                    (match-token! 'SEMICOLON "Malformed class declaration. Missing [;].")
                    result)))
               
               (parse-class-member-declaration
                (lambda ()
                  (cond
                    ((match-token? 'Static)
                     (read-next-token) ; Consume the "static"
                     (cond
                       ((match-token? 'Void)
                        (parse-method-declaration))
                       ((match-token? 'Int)
                        (let ((field (parse-field-declaration)))
                          (match-token! 'SEMICOLON "Malformed field declaration. Missing [;].")
                          field))
                       (else (parser-error "Malformed program. Unexpected token; Expected class member declaration."))))
                    (else (parse-class-declaration #t)))))
               
               (parse-method-declaration
                (lambda ()
                  (let ((name #f)
                        (paras #f)
                        (body #f))
                    (match-token! 'Void "Malformed method declaration. Missing type identifier [void].")
                    (set! name (string->symbol (match-token! 'IDENTIFIER "Malformed method declaration. Missing name.")))
                    (match-token! 'PARENTHESIS-OPEN "Malformed method declaration. Missing parameter list start delimiter [(].")
                    (set!
                     paras
                     (reverse
                      (let loop ((paras (list)))
                        (if (not (match-token? 'PARENTHESIS-CLOSE))
                            (let ((paras (cons (parse-field-declaration) paras)))
                              (if (match-token? 'COMMA)
                                  (begin
                                    (read-next-token) ; Consume the ","
                                    (loop paras))
                                  paras))
                            paras))))
                    (read-next-token) ; Consume the ")"
                    (match-token! 'BRACE-OPEN "Malformed method declaration. Missing method body start delimiter [{].")
                    (set!
                     body
                     (reverse
                      (let loop ((body (list)))
                        (if (match-token? 'BRACE-CLOSE)
                            body
                            (loop (cons (parse-assignment) body))))))
                    (read-next-token) ; Consume the "}"
                    (create-ast 'MethodDeclaration (list name global-index (create-ast-list paras) (create-ast-list body)))))) 
               
               (parse-field-declaration
                (lambda ()
                  (match-token! 'Int "Malformed field declaration. Missing type identifier [int].")
                  (create-ast
                   'FieldDeclaration
                   (list (string->symbol (match-token! 'IDENTIFIER "Malformed field declaration. Missing name.")) global-index))))
               
               (parse-assignment
                (lambda ()
                  (let ((name1 (parse-qualified-name))
                        (result #f))
                    (match-token! 'EQUAL "Malformed assignment. Missing [=].")
                    (set!
                     result
                     (create-ast
                      'VariableAssignment
                      (list
                       (create-ast 'Reference (list name1 global-index))
                       (create-ast 'Reference (list (parse-qualified-name) global-index)))))
                    (match-token! 'SEMICOLON "Malformed assignment. Missing [;].")
                    result)))
               
               (parse-qualified-name
                (lambda ()
                  (let ((name
                         (reverse
                          (let loop ((name (list (string->symbol (match-token! 'IDENTIFIER "Malformed qualified name. Missing identifier.")))))
                            (if (match-token? 'COLON-COLON)
                                (begin
                                  (read-next-token) ; Consume the "::"
                                  (loop (cons (string->symbol (match-token! 'IDENTIFIER "Malformed qualified name. Missing identifier.")) name)))
                                name)))))
                    (if (null? (cdr name))
                        (car name)
                        name)))))
        ;;; Return parser function:
        (lambda ()
          (parse-compilation-unit)))))))