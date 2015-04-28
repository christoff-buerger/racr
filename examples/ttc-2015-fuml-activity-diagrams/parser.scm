; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-fuml-activity-diagrams parser)
 (export parse)
 (import (rnrs) (ttc-2015-fuml-activity-diagrams language))
 
 (define (parse file)
   (define current-char #f)
   
   (define peek-char ; Return the next character if it satisfies optional constraints and exists.
     (lambda constraints
       (and (not (eof-object? current-char))
            (for-all (lambda (f) (f current-char)) constraints)
            current-char)))
   
   (define read-char ; Similar to peek-char but additionally increments the parsing position.
     (lambda constraints
       (unless (apply peek-char constraints) (raise "Parsing Error!"))
       (set! current-char (read))
       current-char))
   
   (define (char= to-read) ; Construct filter for certain character that can be used by peek- and read-char.
     (lambda (char-read)
       (char=? char-read to-read)))
   
   (define (consume-whitespace)
     (when (peek-char char-whitespace?) (read-char) (consume-whitespace)))
   
   (define (parse-boolean)
     (cond ((peek-char (char= #\f)) (parse-keyword "false") #f)
           (else (parse-keyword "true") #t)))
   
   (define (parse-integer) ; Read sequence of digits.
     (define num
       (let loop ((num (list (read-char char-numeric?))))
         (let ((next-char? (peek-char)))
           (if (and next-char? (char-numeric? next-char?))
               (loop (cons (read-char) num))
               (string->number (apply string (reverse num)))))))
     (consume-whitespace)
     num)
   
   (define (parse-identifier) ; Parse ordinary identifier, i.e., [a-zA-Z][a-zA-Z0-9]*.
     (define id
       (let loop ((id (list (read-char char-alphabetic?))))
         (let ((next-char? (peek-char)))
           (if (and next-char? (or (char-alphabetic? next-char?) (char-numeric? next-char?)))
               (loop (cons (read-char) id))
               (string->symbol (apply string (reverse id)))))))
     (consume-whitespace)
     id)
   
   (define (parse-constant)
     (if (peek-char char-numeric?) (parse-integer) (parse-boolean)))
   
   (define (parse-keyword keyword)
     (string-for-each (lambda (c) (read-char (char= c))) keyword)
     (consume-whitespace))
   
   (define (parse-list f)
     (define elem (f))
     (cond ((peek-char (char= #\,)) (parse-keyword ",") (cons elem (parse-list f)))
           (else (list elem))))
   
   (define (parse-activity)
     (define name #f)(define Variable* (list))(define ActivityNode* (list))(define ActivityEdge* (list))
     (read-char)(consume-whitespace)
     (parse-keyword "activity")
     (set! name (parse-identifier))
     (when (peek-char (char= #\())
       (parse-keyword "(")
       (unless (peek-char (char= #\)))
         (set! Variable* (append Variable* (parse-list parse-input))))
       (parse-keyword ")"))
     (parse-keyword "{")
     (unless (peek-char (char= #\n))
       (set! Variable* (append Variable* (parse-list parse-local))))
     (parse-keyword "nodes")
     (parse-keyword "{")
     (unless (peek-char #\})
       (set! ActivityNode* (parse-list parse-node)))
     (parse-keyword "}")
     (parse-keyword "edges")
     (parse-keyword "{")
     (unless (peek-char #\})
       (set! ActivityEdge* (parse-list parse-edge)))
     (parse-keyword "}")
     (parse-keyword "}")
     (unless (peek-char) (raise "Parsing Error!"))
     (:Activity name Variable* ActivityNode* ActivityEdge*))
   
   (define (parse-input)
     (define type (parse-type))
     (:Variable (parse-identifier) type (list)))
   
   (define (parse-local)
     (define type (parse-type))
     (define name (parse-identifier))
     (parse-keyword "=")
     (:Variable name type (parse-constant)))
   
   (define (parse-type)
     (cond ((peek-char (char= #\b)) (parse-keyword "bool") Boolean)
           (else (parse-keyword "int") Integer)))
   
   (define (parse-node)
     (define name #f)
     (define expressions (list))
     (define type
       (cond
         ((peek-char (char= #\i))
          (parse-keyword "initial")
          :InitialNode)
         ((peek-char (char= #\f))
          (read-char)
          (cond
            ((peek-char (char= #\i))
             (parse-keyword "inal")
             :FinalNode)
            (else
             (parse-keyword "ork")
             :ForkNode)))
         ((peek-char (char= #\j))
          (parse-keyword "join")
          :JoinNode)
         ((peek-char (char= #\d))
          (parse-keyword "decision")
          :DecisionNode)
         ((peek-char (char= #\m))
          (parse-keyword "merge")
          :MergeNode)
         ((peek-char (char= #\a))
          (parse-keyword "action")
          :ExecutableNode)))
     (set! name (parse-identifier))
     (when (and (eq? type :ExecutableNode) (peek-char (char= #\c)))
       (parse-keyword "comp")
       (parse-keyword "{")
       (set! expressions (parse-list parse-expression))
       (parse-keyword "}"))
     (consume-edges)
     (if (eq? type :ExecutableNode)
         (type name expressions)
         (type name)))
   
   (define (parse-expression)
     (define asignee (parse-identifier))
     (parse-keyword "=")
     (if (peek-char (char= #\!))
         (begin (parse-keyword "!") (:UnaryExpression asignee not (parse-identifier)))
         (let* ((operand1 (parse-identifier))
                (operator (parse-binary-operator)))
           (:BinaryExpression asignee operator operand1 (parse-identifier)))))
   
   (define (parse-binary-operator)
     (cond
       ((peek-char (char= #\=))
        (parse-keyword "==") =)
       ((peek-char (char= #\&))
        (parse-keyword "&&") &&)
       ((peek-char (char= #\|))
        (parse-keyword "||") //)
       ((peek-char (char= #\<))
        (read-char)
        (if (peek-char (char= #\=))
            (begin (parse-keyword "=") <=)
            (begin (consume-whitespace) <)))
       ((peek-char (char= #\>))
        (read-char)
        (if (peek-char (char= #\=))
            (begin (parse-keyword "=") >=)
            (begin (consume-whitespace) >)))))
   
   (define (consume-edges)
     (when (peek-char (char= #\i))
       (parse-keyword "in")
       (parse-keyword "(")
       (parse-list parse-identifier)
       (parse-keyword ")"))
     (when (peek-char (char= #\o))
       (parse-keyword "out")
       (parse-keyword "(")
       (parse-list parse-identifier)
       (parse-keyword ")")))
   
   (define (parse-edge)
     (define name #f)(define source #f)(define target #f)(define guard #f)
     (parse-keyword "flow")
     (set! name (parse-identifier))
     (parse-keyword "from")
     (set! source (parse-identifier))
     (parse-keyword "to")
     (set! target (parse-identifier))
     (when (peek-char (char= #\[))
       (parse-keyword "[")
       (set! guard (parse-identifier))
       (parse-keyword "]"))
     (if guard (:ControlFlow source target guard) (:ActivityEdge source target)))
   
   (with-input-from-file file parse-activity)))