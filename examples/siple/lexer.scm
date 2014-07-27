; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple lexer)
 (export
  token-source
  token-line
  token-column
  token-type
  token-value
  construct-lexer)
 (import (rnrs) (siple exception-api))
 
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
                 (throw-siple-exception ; Abort lexing with error message
                  (string-append
                   "Lexer Error ("
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
              (read-integer ; Read sequence of digits
               (lambda (n)
                 (let ((c (my-peek-char)))
                   (if (char-numeric? c)
                       (read-integer (cons (my-read-char) n))
                       (apply string (reverse n))))))
              (read-number ; Read integer or real
               (lambda (n)
                 (let ((integer-part (read-integer n)))
                   (if (char=? (my-peek-char) #\.)
                       (begin
                         (my-read-char) ; Consume the "."
                         (let ((c (my-read-char)))
                           (if (not (char-numeric? c))
                               (lexer-error "Invalid number. Real numbers must have a decimal place." c)
                               (string-append integer-part (string #\.) (read-integer (list c))))))
                       integer-part))))
              (read-hexadecimal-number ; Read sequence of hexadecimal digits
               (lambda (n)
                 (let ((c (my-peek-char)))
                   (if (or (char-numeric? c)
                           (char=? c #\A)
                           (char=? c #\B)
                           (char=? c #\C)
                           (char=? c #\D)
                           (char=? c #\E)
                           (char=? c #\F))
                       (read-hexadecimal-number (cons (my-read-char) n))
                       (apply string (reverse n))))))
              (read-identifier ; Read sequence of letters and digits
               (lambda (id)
                 (let ((c (my-peek-char)))
                   (if (or (char-alphabetic? c) (char-numeric? c))
                       (read-identifier (cons (my-read-char) id))
                       (apply string (reverse id))))))
              (read-character-value ; Read a character value expression's content
               (lambda ()
                 (let ((c (my-peek-char)))
                   (cond
                     ((is-whitespace c)
                      (lexer-error "Invalid character value. Whitespace characters are not permitted." c))
                     ((char=? c #\\)
                      (my-read-char) ; Consume the "\"
                      (char->integer (read-escape-sequence)))
                     (else
                      (if (eof-object? c)
                          (lexer-error "Invalid character value. Unexpected end-of-file." c)
                          (char->integer (my-read-char))))))))
              (read-string ; Read a string's content
               (lambda (str)
                 (let ((c (my-peek-char)))
                   (if (char=? c #\\)
                       (begin
                         (my-read-char) ; Consume the escape sign "\"
                         (read-string (cons (read-escape-sequence) str)))
                       (if (not (char=? c #\'))
                           (read-string (cons (my-read-char) str))
                           (apply string (reverse str)))))))
              (read-escape-sequence ; Read an escape sequence's content
               (lambda ()
                 (let ((c (my-peek-char)))
                   (cond
                     ((char=? c #\u)
                      (my-read-char) ; Consume the "u"
                      (if (not (char=? (my-peek-char) #\{))
                          (lexer-error "Invalid hexadecimal escape sequence. Missing start delimiter [{]." c)
                          (begin
                            (my-read-char) ; Consume the "{"
                            (let ((num (string->number (read-hexadecimal-number (list)) 16)))
                              (cond
                                ((eq? num #f)
                                 (lexer-error "Invalid hexadecimal escape sequence. Hexadecimal number is missing." c))
                                ((char=? (my-peek-char) #\})
                                 (begin
                                   (my-read-char) ; Consume the "}"
                                   (integer->char num)))
                                (else (lexer-error "Invalid hexadecimal escape sequence. Missing end delimiter [}]." c)))))))
                     ((char=? c #\n)
                      (my-read-char)
                      #\newline)
                     ((char=? c #\r)
                      (my-read-char)
                      (integer->char #x000D)) ; Carriage return
                     ((char=? c #\t)
                      (my-read-char)
                      #\tab)
                     ((char=? c #\b)
                      (my-read-char)
                      (integer->char #x0008)) ; Backspace
                     ((char=? c #\f)
                      (my-read-char)
                      (integer->char #x000C)) ; Formfeed
                     ((char=? c #\')
                      (my-read-char)
                      #\')
                     ((char=? c #\\)
                      (my-read-char)
                      #\\)
                     (else (lexer-error "Unknown escape sequence." c)))))))
       
       ;;; Return lexer function:
       (lambda ()
         (let recognize-token ((c (my-read-char)))
           (cond
             ((eof-object? c)
              (new-token '*eoi* ""))
             ((is-whitespace c)
              (recognize-token (my-read-char)))
             ((char=? c #\%)
              (let consume-one-line-comment ()
                (if (not (char=? (my-peek-char) #\newline))
                    (begin
                      (my-read-char)
                      (consume-one-line-comment))
                    (recognize-token (my-read-char)))))
             ((char-alphabetic? c)
              (let ((id (read-identifier (list c))))
                (cond
                  ((string=? id "Procedure")
                   (new-token 'Procedure id))
                  ((string=? id "Var")
                   (new-token 'Var id))
                  ((string=? id "Begin")
                   (new-token 'Begin id))
                  ((string=? id "End")
                   (new-token 'End id))
                  ((string=? id "If")
                   (new-token 'If id))
                  ((string=? id "Then")
                   (new-token 'Then id))
                  ((string=? id "Else")
                   (new-token 'Else id))
                  ((string=? id "Fi")
                   (new-token 'Fi id))
                  ((string=? id "While")
                   (new-token 'While id))
                  ((string=? id "Do")
                   (new-token 'Do id))
                  ((string=? id "Od")
                   (new-token 'Od id))
                  ((string=? id "Call")
                   (new-token 'Call id))
                  ((string=? id "Return")
                   (new-token 'Return id))
                  ((string=? id "Write")
                   (new-token 'Write id))
                  ((string=? id "Read")
                   (new-token 'Read id))
                  ((string=? id "Assert")
                   (new-token 'Assert id))
                  ((string=? id "Boolean")
                   (new-token 'Boolean id))
                  ((string=? id "Integer")
                   (new-token 'Integer id))
                  ((string=? id "Real")
                   (new-token 'Real id))
                  ((string=? id "Pointer")
                   (new-token 'Pointer id))
                  ((string=? id "Address")
                   (new-token 'Address id))
                  ((string=? id "Deref")
                   (new-token 'Deref id))
                  ((string=? id "Not")
                   (new-token 'Not id))
                  ((string=? id "And")
                   (new-token 'And id))
                  ((string=? id "Or")
                   (new-token 'Or id))
                  ((string=? id "true")
                   (new-token 'CONSTANT id))
                  ((string=? id "false")
                   (new-token 'CONSTANT id))
                  (else (new-token 'IDENTIFIER id)))))
             ((char-numeric? c)
              (new-token 'CONSTANT (read-number (list c))))
             ;            ((char=? c #\')
             ;             (let ((content (read-string (list))))
             ;               (if (char=? (my-peek-char) #\')
             ;                   (begin
             ;                     (my-read-char) ; Consume the terminating "'"
             ;                     (new-token 'STRING content))
             ;                   (lexer-error "Invalid string. String is not appropriate terminated" c))))
             ((char=? c #\:)
              (if (char=? (my-peek-char) #\=)
                  (begin
                    (my-read-char) ; Consume the "="
                    (new-token 'ASSIGNMENT ":="))
                  (new-token 'COLON ":")))
             ((char=? c #\,)
              (new-token 'COMMA ","))
             ((char=? c #\;)
              (new-token 'SEMICOLON ";"))
             ((char=? c #\()
              (new-token 'PARENTHESIS-OPEN "("))
             ((char=? c #\))
              (new-token 'PARENTHESIS-CLOSE ")"))
             ;            ((char=? c #\[)
             ;             (new-token 'BRACKET-OPEN "["))
             ;            ((char=? c #\])
             ;             (new-token 'BRACKET-CLOSE "]"))
             ((char=? c #\+)
              (new-token 'ADDITION "+"))
             ((char=? c #\-)
              (new-token 'SUBTRACTION "-"))
             ((char=? c #\*)
              (new-token 'MULTIPLICATION "*"))
             ((char=? c #\/)
              (new-token 'DIVISION "/"))
             ((char=? c #\=)
              (new-token 'EQUAL "="))
             ((char=? c #\<)
              (if (char=? (my-peek-char) #\=)
                  (begin
                    (my-read-char) ; Consume the "="
                    (new-token 'LESSER-EQUAL "<="))
                  (new-token 'LESSER "<")))
             ((char=? c #\>)
              (if (char=? (my-peek-char) #\=)
                  (begin
                    (my-read-char) ; Consume the "="
                    (new-token 'GREATER-EQUAL ">="))
                  (new-token 'GREATER ">")))
             ((char=? c #\#)
              (new-token 'NOT-EQUAL "#"))
             (else
              (lexer-error "Illegal character." c)))))))))