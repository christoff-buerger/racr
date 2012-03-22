; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple main)
 (export
  test-lexer
  test-parser
  test-compiler
  interpret)
 (import
  (rnrs)
  (racr)
  (siple type)
  (siple state)
  (siple lexer)
  (siple ast)
  (siple parser)
  (siple access-support)
  (siple name-analysis)
  (siple type-analysis)
  (siple type-coercion)
  (siple well-formedness)
  (siple interpreter))
 
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
           (call/cc
            (lambda (k)
              (let ((lexer (construct-lexer input-file-name input-port 4 k)))
                (let lexer-loop ((token (lexer)))
                  (if (eq? (token-type token) '*eoi*)
                      (k "Input successfully processed.") ; Finish lexing with success message
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
                        (lexer-loop (lexer))))))))))
        (lambda ()
          (close-port input-port)
          (close-port output-port))))))
 
 (define initialize-siple
   (lambda ()
     (if (= (specification-phase siple-specification) 1)
         (begin
           (specify-ast)
           (specify-access-support)
           (specify-name-analysis)
           (specify-type-analysis)
           (specify-type-coercion)
           (specify-well-formedness)
           (compile-ag-specifications siple-specification)))))
 
 (define test-ast
   (lambda (input-file-name attribute-pretty-printer-list)
     (initialize-siple)
     (let* ((input-port (open-input-file input-file-name))
            (output-port (open-output-file (string-append input-file-name ".test-results.txt"))))
       (dynamic-wind
        (lambda () #f)
        (lambda ()
          (display
           (call/cc
            (lambda (k)
              (let* ((lexer (construct-lexer input-file-name input-port 4 k))
                     (parser (construct-parser lexer k))
                     (ast (parser)))
                (perform-type-coercions ast)
                (print-ast ast attribute-pretty-printer-list output-port)
                (k "Input successfully processed."))))))
        (lambda ()
          (close-port input-port)
          (close-port output-port))))))
 
 (define test-parser
   (lambda (input-file-name)
     (test-ast input-file-name (list))))
 
 (define test-compiler
   (lambda (input-file-name)
     (test-ast
      input-file-name
      (list
       (cons 'main-procedure (lambda (n) (if n (att-value 'dewey-address n) #f)))
       (cons 'declaration (lambda (n) (if n (att-value 'dewey-address n) #f)))
       (cons 'type (lambda (n) (type-pretty-print n)))
       (cons 'local-correct? (lambda (n) n))))))
 
 (define interpret
   (lambda (input-file-name)
     (initialize-siple)
     (let* ((input-port (open-input-file input-file-name)))
       (dynamic-wind
        (lambda () #f)
        (lambda ()
          (display
           (call/cc
            (lambda (k)
              (let* ((lexer (construct-lexer input-file-name input-port 4 k))
                     (parser (construct-parser lexer k))
                     (ast (parser)))
                (perform-type-coercions ast)
                (weave-interpreter ast)
                (k (state-std-out ((ast-annotation ast 'interpret)))))))))
        (lambda ()
          (close-port input-port)))))))