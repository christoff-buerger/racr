; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić, C. Bürger

#!r6rs

(library
 (cjava-racr parser-cl)
 (export
  construct-parser-cl)
 (import (rnrs) (racr core) (cjava-racr exception-api) (cjava-racr lexer-cl))
 
 (define construct-parser-cl
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
                    (set! line (token-line-cl old-token))
                    (set! column (token-column-cl old-token))
                    (token-value-cl old-token))))
               (match-token?
                (lambda (to-match)
                  (eq? (token-type-cl current-token) to-match)))
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
                    (token-value-cl current-token)
                    "]): "
                    message))))
               
               (parse-composition-program
                (lambda ()
                  (let loop ((composers (list)))
                    (if (match-token? '*eoi*)
                        (create-ast 'CompositionProgram (list (create-ast-list (reverse composers))))
                        (loop (cons (parse-bind-composer) composers))))))
               
               (parse-bind-composer
                (lambda ()
                  (let ((targetname (list))
                        (sourcename (list)))
                    (match-token! 'Bind "Missing composition operation name.")
                    (set! targetname (parse-qualified-wildcard-name))
                    (set! sourcename (parse-qualified-wildcard-name))
                    (match-token! 'SEMICOLON "Malformed composition program. Missing [;].")
                    (create-ast 'BindComposer (list targetname sourcename)))))
               
               (parse-qualified-wildcard-name
                (lambda ()
                  (let ((id (string->symbol (match-token! 'IDENTIFIER "Malformed qualified wildcard name. Missing identifier/wildcard."))))
                    (if (match-token? 'POINT)
                        (begin
                          (read-next-token) ; Consume the "."
                          (cons id (parse-qualified-wildcard-name)))
                        (list id))))))
        
        ;;; Return parser function:
        (lambda ()
          (let ((ast (parse-composition-program)))
            ast)))))))