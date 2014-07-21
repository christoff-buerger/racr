; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić

#!r6rs

(library
 (cjava-racr parser-cl)
 (export
  construct-parser-cl)
 (import (rnrs) (racr core) (cjava-racr exception-api) (cjava-racr lexer-cl))
 
 (define (str-split str ch)
   (let ((len (string-length str)))
     (letrec ((split
               (lambda (a b)
                 (cond
                   ((>= b len)
                    (if (= a b)
                        (list)
                        (cons (substring str a b) '())))
                   ((char=? ch (string-ref str b))
                    (if (= a b)
                        (split (+ 1 a) (+ 1 b))
                        (cons (substring str a b) (split b b))))
                   (else (split a (+ 1 b)))))))
       (split 0 0))))
 
 (define strings->symbols
   (lambda (strings)
     (map
      (lambda (str)
        (string->symbol str))
      strings)))     
 
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
                    (set! targetname (strings->symbols (str-split (match-token! 'IDENTIFIER "Missing target name.") #\.)))
                    (set! sourcename (strings->symbols (str-split (match-token! 'IDENTIFIER "Missing source name.") #\.)))
                    (match-token! 'SEMICOLON "Malformed composition program. Missing [;].")
                    (create-ast 'BindComposer (list targetname sourcename))))))
        
        
        ;;; Return parser function:
        (lambda ()
          (let ((ast (parse-composition-program)))
            ast)))))))