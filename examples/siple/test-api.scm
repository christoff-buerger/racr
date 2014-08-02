; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple test-api)
 (export
  test-lexer
  test-parser
  test-compiler)
 (import
  (rnrs)
  (racr core)
  (racr testing)
  (siple lexer)
  (siple parser)
  (siple type)
  (siple main))
 
 (define test-lexer
   (lambda (input-file-name)
     (let* ((input-port (open-input-file input-file-name))
            (output-port (open-output-file (string-append input-file-name ".test-results.txt")))
            (my-display
             (lambda (to-display)
               (display to-display output-port))))
       (dynamic-wind
        (lambda () #f)
        (lambda ()
          (display
           (let ((lexer (construct-lexer input-file-name input-port 4)))
             (let lexer-loop ((token (lexer)))
               (if (eq? (token-type token) '*eoi*)
                   "Input successfully processed." ; Finish lexing with success message
                   (begin
                     (my-display (token-type token))
                     (let loop ((n (- 30 (string-length (symbol->string (token-type token))))))
                       (if (= n 0)
                           #t
                           (begin
                             (my-display " ")
                             (loop (- n 1)))))
                     (my-display "Line: ")
                     (let loop ((n (- 5 (string-length (number->string (token-line token))))))
                       (if (= n 0)
                           #t
                           (begin
                             (my-display " ")
                             (loop (- n 1)))))
                     (my-display (token-line token))
                     (my-display " Column: ")
                     (let loop ((n (- 5 (string-length (number->string (token-column token))))))
                       (if (= n 0)
                           #t
                           (begin
                             (my-display " ")
                             (loop (- n 1)))))
                     (my-display (token-column token))
                     (my-display " Value: ")
                     (my-display (token-value token))
                     (my-display #\newline)
                     (lexer-loop (lexer))))))))
        (lambda ()
          (close-port input-port)
          (close-port output-port))))))
 
 (define test-ast
   (lambda (input-file-name perform-type-coercions? attribute-pretty-printer-list)
     (let* ((input-port (open-input-file input-file-name))
            (output-port (open-output-file (string-append input-file-name ".test-results.txt"))))
       (dynamic-wind
        (lambda () #f)
        (lambda ()
          (display
           (let* ((lexer (construct-lexer input-file-name input-port 4))
                  (parser (construct-parser lexer siple-specification perform-type-coercions?))
                  (ast (parser)))
             (print-ast ast attribute-pretty-printer-list output-port)
             "Input successfully processed.")))
        (lambda ()
          (close-port input-port)
          (close-port output-port))))))
 
 (define test-parser
   (lambda (input-file-name)
     (test-ast input-file-name #f (list))))
 
 (define test-compiler
   (lambda (input-file-name)
     (test-ast
      input-file-name
      #t
      (list
       (cons 'main-procedure (lambda (n) (if n (att-value 'dewey-address n) #f)))
       (cons 'declaration (lambda (n) (if n (att-value 'dewey-address n) #f)))
       (cons 'type (lambda (n) (type-pretty-print n)))
       (cons 'local-correct? (lambda (n) (if n #t #f))))))))