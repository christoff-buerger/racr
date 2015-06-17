; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr lexer)
 (export
  token-source
  token-line
  token-column
  token-type
  token-value
  construct-lexer)
 (import (rnrs) (tinycpp-racr exception-api))
 
 (define-record-type token
   (fields source line column type value))
 
 (define construct-lexer
   (lambda (input-name input-port tabulator-size)
     (letrec (;;; Lexer IO support functions:
              (line-position 1)
              (column-position 1)
              (my-read-char
               (lambda ()
                 (let ((c (read-char input-port)))
                   (cond
                     ((eof-object? c)
                      c)
                     ((char=? c #\newline)
                      (set! line-position (+ line-position 1))
                      (set! column-position 1)
                      c)
                     ((char=? c #\tab)
                      (set! column-position (+ (+ column-position (- tabulator-size (mod column-position tabulator-size))) 1))
                      c)
                     (else
                      (set! column-position (+ column-position 1))
                      c)))))
              (my-peek-char
               (lambda () (peek-char input-port)))
              (lexer-error
               (lambda (message character)
                 (throw-tinycpp-racr-exception ; Abort lexing with error message
                  (string-append
                   "Lexer Error ("
                   input-name
                   ": "
                   (number->string line-position)
                   ","
                   (number->string column-position)
                   ";["
                   (string character)
                   "]): "
                   message))))
              (new-token
               (lambda (type value)
                 (make-token input-name line-position (- column-position (string-length value)) type value)))
              
              ;;; Complicated token processing functions:
              (is-whitespace
               (lambda (c)
                 (or (char=? c #\space)
                     (char=? c #\newline)
                     (char=? c (integer->char #x000D)) ; Carriage return
                     (char=? c #\tab)
                     (char=? c (integer->char #x0008)) ; Backspace
                     (char=? c (integer->char #x000C))))) ; Formfeed
              
              (read-identifier
               (lambda (id)
                 (let ((c (my-peek-char)))
                   (if (or (char-alphabetic? c) (char-numeric? c))
                       (read-identifier (cons (my-read-char) id))
                       (apply string (reverse id)))))))
       
       ;;; Return lexer function:
       (lambda ()
         (let recognize-token ((c (my-read-char)))
           (cond
             ((eof-object? c)
              (new-token '*eoi* ""))
             ((is-whitespace c)
              (recognize-token (my-read-char)))
             ((and (char=? c #\/) (char=? (my-peek-char) #\/))
              (my-read-char) ; Consume the "/"
              (let loop ()
                (unless (or (eof-object? (my-peek-char)) (char=? (my-peek-char) #\newline))
                  (my-read-char)
                  (loop)))
              (recognize-token (my-read-char)))
             ((and (char=? c #\/) (char=? (my-peek-char) #\*))
              (my-read-char)(my-read-char) ; Consume the "/*"
              (let loop ((was-*? #f))
                (cond
                  ((eof-object? (my-peek-char)) (lexer-error "Unfinished multiline comment." #\space))
                  ((and was-*? (char=? (my-peek-char) #\/)) (my-read-char)) ; Consume the "/"
                  (else (loop (char=? (my-read-char) #\*)))))
              (recognize-token (my-read-char)))
             ((char-alphabetic? c)
              (let ((id (read-identifier (list c))))
                (cond
                  ((string=? id "class")
                   (new-token 'Class id))
                  ((string=? id "public")
                   (new-token 'Public id))
                  ((string=? id "static")
                   (new-token 'Static id))
                  ((string=? id "int")
                   (new-token 'Int id))
                  ((string=? id "void")
                   (new-token 'Void id))
                  (else (new-token 'IDENTIFIER id)))))         
             ((char=? c #\:)
              (if (char=? (my-peek-char) #\:)
                  (begin
                    (my-read-char) ; Consume the ":"
                    (new-token 'COLON-COLON "::"))
                  (new-token 'COLON ":")))
             ((char=? c #\,)
              (new-token 'COMMA ","))
             ((char=? c #\;)
              (new-token 'SEMICOLON ";"))
             ((char=? c #\=)
              (new-token 'EQUAL "="))
             ((char=? c #\()
              (new-token 'PARENTHESIS-OPEN "("))
             ((char=? c #\))
              (new-token 'PARENTHESIS-CLOSE ")"))
             ((char=? c #\{)
              (new-token 'BRACE-OPEN "{"))
             ((char=? c #\})
              (new-token 'BRACE-CLOSE "}"))
             (else
              (lexer-error "Illegal character." c)))))))))