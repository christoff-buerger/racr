; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić, C. Bürger

#!r6rs

(library
 (cjava-racr parser)
 (export
  construct-parser)
 (import (rnrs) (racr core) (cjava-racr exception-api) (cjava-racr lexer))
 
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
                  (throw-cjava-racr-exception ; Abort parsing with error message
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
                  (match-token! 'Public "Malformed class declaration. Missing modifier [public].")
                  (parse-class-decl)))
               
               (parse-class-decl
                (lambda ()
                  (let ((name #f)
                        (body #f))
                    (match-token! 'Class "Malformed class declaration. Missing class declaration start delimiter [class].")
                    (set! name (string->symbol (match-token! 'IDENTIFIER "Malformed class declaration. Missing name.")))
                    (match-token! 'BRACE-OPEN "Malformed class declaration. Missing class body start delimiter [{].")
                    (set!
                     body
                     (reverse
                      (let loop ((body (list)))
                        (if (not (match-token? 'BRACE-CLOSE))
                            (loop (cons (parse-declaration) body))
                            body))))
                    (read-next-token) ; Consume the "}"
                    (create-ast 'ClassDeclaration (list name (create-ast-list body) (token-source current-token))))))
               
               (parse-declaration
                (lambda ()
                  (cond
                    ((match-token? 'SQUAREBRACKET-OPEN)
                     (parse-hook-decl))
                    ((match-token? 'Public) 
                     (read-next-token) ; Consume the "public"
                     (match-token! 'Static "Malformed declaration. Missing modifier [static].")
                     (cond
                       ((match-token? 'Class)
                        (parse-class-decl))
                       ((match-token? 'Void)
                        (parse-method-decl))
                       ((match-token? 'Int)
                        (let ((field (parse-field-decl)))
                          (match-token! 'SEMICOLON "Malformed declaration. Declaration not properly finished; Missing [;].")
                          field))))
                    (else (parser-error "Malformed program. Unexpected token; Expected declaration.")))))
               
               (parse-hook-decl
                (lambda ()
                  (let ((name #f))
                    (match-token! 'SQUAREBRACKET-OPEN "Malformed declaration hook. Missing declaration hook start delimiter [[].")
                    (match-token! 'SQUAREBRACKET-OPEN "Malformed declaration hook. Missing declaration hook start delimiter [[].")
                    (set! name (string->symbol (match-token! 'IDENTIFIER "Malformed declaration hook. Missing name.")))
                    (match-token! 'SQUAREBRACKET-CLOSE "Malformed declaration hook. Missing declaration hook end delimiter []].")
                    (match-token! 'SQUAREBRACKET-CLOSE "Malformed declaration hook. Missing declaration hook end delimiter []].")
                    (create-ast 'DeclarationHook (list name)))))
               
               (parse-method-decl
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
                            (let ((paras (cons (parse-field-decl) paras)))
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
                        (if (not (match-token? 'BRACE-CLOSE))
                            (let ((body (cons (parse-field-or-assignment) body)))
                              (match-token! 'SEMICOLON "Statement not properly finished; Missing [;].")
                              (loop body))
                            body))))
                    (read-next-token) ; Consume the "}"
                    (create-ast 'MethodDeclaration (list name (create-ast-list paras) (create-ast-list body)))))) 
               
               (parse-field-or-assignment
                (lambda ()
                  (if (match-token? 'Int)
                      (parse-field-decl)
                      (parse-assignment))))
               
               (parse-field-decl
                (lambda ()
                  (match-token! 'Int "Malformed field declaration. Missing type identifier [int].")
                  (create-ast
                   'FieldDeclaration
                   (list (string->symbol (match-token! 'IDENTIFIER "Malformed field declaration. Missing name."))))))
               
               (parse-assignment
                (lambda ()
                  (let ((name1 (parse-qualified-name)))
                    (match-token! 'EQUAL "Malformed variable assignment; Missing [=].")
                    (create-ast
                     'VariableAssignment
                     (list
                      (create-ast 'Reference (list name1))
                      (create-ast 'Reference (list (parse-qualified-name))))))))
               
               (parse-qualified-name
                (lambda ()
                  (let ((id (string->symbol (match-token! 'IDENTIFIER "Malformed qualified name. Missing identifier."))))
                    (if (match-token? 'POINT)
                        (begin
                          (read-next-token) ; Consume the "."
                          (cons id (parse-qualified-name)))
                        (list id))))))
        ;;; Return parser function:
        (lambda ()
          (let ((ast (parse-compilation-unit)))
            ast)))))))