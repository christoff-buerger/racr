; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (rnrs mutable-pairs))

(define run
  (lambda ()
    (wiki-to-latex
     "./../../racr-wiki/git" ; Input directory
     "." ; Output directory
     ; Wiki pages to process:
     "Introduction"
     "Architecture"
     "ASTs"
     "Attributes"
     "Rewrites"
     "Annotations"
     "SupportAPI")))

(define wiki-to-latex
  (lambda (input-directory output-directory . pages)
    ; Generate main LaTeX document:
    (let ((racr-manual.tex (string-append output-directory "/racr-manual.tex")))
      (when (file-exists? racr-manual.tex)
        (delete-file racr-manual.tex))
      (with-output-to-file
          racr-manual.tex
        (lambda ()
          (write-string "\\documentclass[a4paper,headsepline,abstracton,11pt,twoside,DIV=12,BCOR=15mm]{scrbook}\n\n")
          (write-string "\\input{preamble}\n\n")
          (write-string "\\begin{document}\n")
          (write-string "\t\\input{title}\n")
          (write-string "\t\\tableofcontents\n")
          (write-string "\t\\listoffigures")
          (for-each
           (lambda (page)
             (write-string "\n\t\\input{")
             (write-string page)
             (write-string "}"))
           pages)
          (write-string "\n\t\\part*{Appendix}")
          (write-string "\n\t\\appendix")
          (write-string "\n\t\\input{sourcecode}")
          (write-string "\n\t\\chapter{MIT License}")
          (write-string "\n\t\\input{./../license.txt}")
          (write-string "\n\t\\clearpage \\addcontentsline{toc}{chapter}{API Index} \\printindex")
          (write-string "\n\\end{document}\n"))))
    ; Transform wiki content:
    (for-each
     (lambda (page)
       (let ((in-file (string-append input-directory "/" page ".wiki"))
             (out-file (string-append output-directory "/" page ".tex")))
         (unless (file-exists? in-file)
           (assertion-violation 'wiki-to-latex "ERROR: Input file does not exist." in-file))
         (when (file-exists? out-file)
           (delete-file out-file))
         (with-input-from-file
             in-file
           (lambda ()
             (with-output-to-file
                 out-file
               process-page)))))
     pages)))

; Beware: Must be valid wiki page satisfying the following conditions:
;  1)  Every link must be named (i.e., [Pagename] is no valid link; instead use [Pagename Pagename])
;  2)  Exclamation marks to prevent autolinking are not supported
;  3)  Only non-numbered lists are permitted; Numbered lists using # are not supported
;  4)  Lists must be terminated by two consecutive new lines. E.g.:
;       * first list item
;       * second
;        * first inner
;       * third
;      
;      Note the empty line before---its the reason this line is not within the list! In contrary the list
;       * first item
;      is malformed, because it is not terminated by two new line characters!
;  5)  Pictures referenced via URL must be proceeded by a wiki comment (on a new line) designating the following
;      text as a link to a picture resource and specifying a scale and file containing the picture. The line after
;      the link must be empty and the next but one is interpreted to be the caption of the figure, whereby bold text
;      suffixed by whitespace is ignored. The syntax is:
;      
;      <wiki:comment>picture(scale,picture-file)</wiki:comment>
;      URL-to-picture-resource-comes-on-next-line-without-indentation
;      
;      *Figure 1* caption comes here
;      
;      The generated LaTeX code expects the referenced picture file to be within its folder.
;      The picture is scaled with the specified scale. It can be a pdf or png.
;  6)  Dividers are not supported
;  7)  Quoting is not supported
;  8)  Tables are not supported
;  9)  HTML is not supported
;  10) Comments are not supported
;  11) +1 Button is not supported
;  12) Gadgets are not supported
;  13) Videos are not supported
;  14) Wiki table of contents are not supported
(define process-page
  (lambda ()
    ; Prepare processing:
    (set! emphasized? #f)
    (set! bold? #f)
    (set! indentation-stack (list 0))
    (set! indentation-flag 0)
    (initialize-ring-buffer)
    ; Process wiki pragmas:
    (do ()
      ((not (match-chars #\#)))
      (do () ((match-chars #\newline)) (shift-ring-buffer 1)))
    (match-chars #\newline)
    ; Process wiki content:
    (let loop ()
      (unless (eof-object? (cadr ring-buffer))
        (or
         (process-racr-api-function-heading)
         (process-headings)
         (process-lists) ; Must be processed before bold text!
         (process-emphasized-text)
         (process-bold-text)
         (process-inline-listing)
         (process-links)
         (process-pictures)
         (process-generic-code-listing)
         (process-scheme-code-listing)
         ; Process ordinary wiki text: 
         (write-char-from-ring-buffer))
        (loop)))))

(define process-racr-api-function-heading
  (lambda ()
    (cond
      ((and indentation-flag (match-string "=== `"))
       (write-string "\\indexfunction{")
       (do ()
         ((match-string "` ===\n\n<code language=\"lisp\">\n") (write-string "}\n"))
         (write-char-from-ring-buffer))
       (write-string "\\begin{lstlisting}[style=racr-api-function-heading-style]\n")
       (do () ((match-string "</code>")) (write-char-from-ring-buffer))
       (write-string "\n\\end{lstlisting}")
       #t)
      (else #f))))

(define process-headings
  (lambda ()
    (cond
      ((and indentation-flag (match-chars #\=))
       (let ((chapter-depth (do ((i 1 (+ i 1))) ((not (char=? (cadr ring-buffer) #\=)) i) (shift-ring-buffer 1)))
             (index-list (list)))
         (match-chars #\space)
         (write-char #\\)
         (if (= chapter-depth 1)
             (write-string "chapter{")
             (do ((i (- chapter-depth 2) (- i 1))) ((= i 0) (write-string "section{")) (write-string "sub")))
         (do ()
           ((match-chars #\space #\=))
           (cond
             ((process-emphasized-text) #t)
             ((process-bold-text) #t)
             ((process-inline-listing) => (lambda (code) (set! index-list (cons code index-list))))
             (else (write-char-from-ring-buffer))))
         (do () ((match-chars #\newline)) (shift-ring-buffer 1))
         (write-string "}\n")
         (for-each
          (lambda (code)
            (write-string "\\index{")
            (write-string (list->string code))
            (write-string "}\n"))
          index-list)
         #t))
      (else #f))))

(define process-emphasized-text
  (lambda ()
    (cond
      ((and (not emphasized?) (match-chars #\_))
       (set! emphasized? #t)
       (write-string "\\emph{")
       #t)
      ((and emphasized? (match-chars #\_))
       (set! emphasized? #f)
       (write-char #\})
       #t)
      (else #f))))

(define process-bold-text
  (lambda ()
    (cond
      ((and (not bold?) (match-chars #\*))
       (set! bold? #t)
       (write-string "\\textbf{")
       #t)
      ((and bold? (match-chars #\*))
       (set! bold? #f)
       (write-char #\})
       #t)
      (else #f))))

(define process-inline-listing
  (lambda ()
    (cond
      ((match-chars #\`)
       (write-string "\\lstinline[style=lisp-style];")
       (let ((result
              (reverse
               (do ((read (list) (cons (write-char-from-ring-buffer) read)))
                 ((match-chars #\`) read)))))
         (write-char #\;)
         result))
      (else #f))))

(define process-links
  (lambda ()
    (cond
      ((match-chars #\[)
       (do () ((match-chars #\space)) (shift-ring-buffer 1))
       (do () ((match-chars #\])) (write-char-from-ring-buffer))
       #t)
      (else #f))))

(define process-pictures
  (lambda ()
    (cond
      ((match-string "\n<wiki:comment>picture(")
       (write-string "\n\\begin{figure}\n    \\includegraphics[scale=")
       (do () ((match-chars #\,)) (write-char-from-ring-buffer))
       (write-string "]{")
       (do ((file-name (list) (cons (write-char-from-ring-buffer) file-name)))
         ((match-string ".")
          (let ((file-name (list->string (reverse file-name))))
            (cond
              ((file-exists? (string-append "./" file-name ".pdf")) (write-string ".pdf"))
              (else (write-string ".png"))))))
       (do () ((match-string ")</wiki:comment>\n")) (shift-ring-buffer 1))
       (do () ((match-chars #\newline #\newline #\*)) (shift-ring-buffer 1))
       (do () ((match-chars #\* #\space)) (shift-ring-buffer 1))
       (write-string "}\n    \\centering\n    \\caption{")
       (do () ((match-chars #\newline)) (write-char-from-ring-buffer))
       (write-string "}\n\\end{figure}\n")
       #t)
      (else #f))))

(define process-generic-code-listing
  (lambda ()
    (cond
      ((match-string "\n{{{\n")
       (write-string "\n\\begin{lstlisting}[style=generic-code]\n")
       (do () ((match-string "\n}}}")) (write-char-from-ring-buffer))
       (write-string "\n\\end{lstlisting}")
       #t)
      (else #f))))

(define process-scheme-code-listing
  (lambda ()
    (cond
      ((match-string "<code language=\"lisp\">\n")
       (write-string "\\begin{lstlisting}[style=lisp-style]\n")
       (do () ((match-string "</code>")) (write-char-from-ring-buffer))
       (write-string "\n\\end{lstlisting}")
       #t)
      (else #f))))

(define process-lists
  (lambda ()
    (cond
      ((and indentation-flag (> indentation-flag 0) (match-chars #\*))
       (when (> indentation-flag (car indentation-stack))
         (write-string "\\begin{itemize}\n")
         (set! indentation-stack (cons indentation-flag indentation-stack))
         (do ((i indentation-flag (- i 1))) ((= i 0)) (write-char #\space)))
       (let loop ((already-indented indentation-flag))
         (when (< indentation-flag (car indentation-stack))
           (do ((i (- (car indentation-stack) already-indented) (- i 1))) ((= i 0)) (write-char #\space))
           (write-string "\\end{itemize}\n")
           (set! indentation-stack (cdr indentation-stack))
           (loop 0))
         (when (= already-indented 0)
           (do ((i indentation-flag (- i 1))) ((= i 0)) (write-char #\space))))
       (write-string "\\item")
       (set! indentation-flag #f)
       #t)
      ((and (> (length indentation-stack) 1) (match-chars #\newline #\newline))
       (write-char #\newline)
       (do () ((= (car indentation-stack) 0))
         (do ((i (car indentation-stack) (- i 1))) ((= i 0)) (write-char #\space))
         (write-string "\\end{itemize}\n")
         (set! indentation-stack (cdr indentation-stack)))
       (write-char #\newline)
       (set! indentation-flag 0)
       #t)
      (else #f))))

(define emphasized? #f)
(define bold? #f)
(define indentation-stack (list 0))
(define indentation-flag 0)

(define ring-buffer
  (let ((end (cons #f #f)))
    (do ((head end (cons #f head)) (i 0 (+ i 1)))
      ((= i 63) (set-cdr! end head) head))))
(define initialize-ring-buffer
  (lambda ()
    (shift-ring-buffer 64)))
(define shift-ring-buffer
  (lambda (to-shift)
    (set!
     ring-buffer
     (do ((pos ring-buffer (cdr pos)) (i to-shift (- i 1)))
       ((= i 0) pos)
       (set-car! (cdr pos) (read-char))))
    ring-buffer))
(define write-char-from-ring-buffer
  (lambda ()
    (let ((char (cadr ring-buffer)))
      (unless (eof-object? char)
        (write-char char)
        (cond
          ((char=? char #\newline)
           (set! indentation-flag 0))
          ((and indentation-flag (char=? char #\space))
           (set! indentation-flag (+ indentation-flag 1)))
          (else (set! indentation-flag #f)))
        (shift-ring-buffer 1))
      char)))

(define match-string
  (lambda (to-match)
    (apply match-chars (string->list to-match))))

(define match-chars
  (lambda to-match
    (and
     ; Check for match:
     (let loop ((to-match to-match)
                (pos (cdr ring-buffer)))
       (if (null? to-match)
           #t
           (if (and (not (eof-object? (car pos))) (char=? (car pos) (car to-match)))
               (loop (cdr to-match) (cdr pos))
               #f)))
     ; Empty matched & refill buffer:
     (shift-ring-buffer (length to-match)))))

(define write-string
  (lambda (string)
    (put-string (current-output-port) string)))
