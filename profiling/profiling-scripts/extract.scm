; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (profiling-scripts extract)
 (export add-padding sort-rows ps:!= ps:== ps:< ps:> ps:<= ps:>= ps:min ps:max ps:MIN ps:MAX
         initialise unique-row? update-local-extremum update-global-extremum discard-row)
 (import (rnrs))

 (define (add-padding s)
   (string-append (make-string (- 19 (string-length s)) #\space) s))
 
 (define (ps:!= s1 s2)
   (not (ps:== s1 s2)))
 (define ps:== string=?)
 (define ps:< string<?)
 (define ps:> string>?)
 (define ps:<= string<=?)
 (define ps:>= string>=?)
 (define ps:min string<?)
 (define ps:max string>?)
 (define ps:MIN string<?)
 (define ps:MAX string>?)

 (define local-extrema #f)
 (define global-extrema #f)
 (define last-row-processed #f)
 
 (define (initialise column-count)
   (set! local-extrema (make-vector column-count))
   (do ((i (- column-count 1) (- i 1))) ((< i 0))
     (vector-set! local-extrema i (make-hashtable equal-hash equal?)))
   (set! global-extrema (make-vector column-count))
   (do ((i (- column-count 1) (- i 1))) ((< i 0))
     (vector-set! global-extrema i (make-eq-hashtable)))
   (set! last-row-processed (make-vector column-count "")))

 (define (sort-rows column-count rows)
   (vector-sort!
    (lambda (row-1 row-2)
      (define date-1 (vector-ref row-1 0))
      (define date-2 (vector-ref row-2 0))
      (or
       (ps:< date-1 date-2)
       (and
        (ps:== date-1 date-2)
        (let loop ((i 1))
          (cond
            ((>= i column-count) #f)
            ((ps:< (vector-ref row-1 i) (vector-ref row-2 i)) #t)
            ((ps:> (vector-ref row-1 i) (vector-ref row-2 i)) #f)
            (else (loop (+ i 1))))))))
    rows)
   rows)

 (define (unique-row? row)
   (define copy?
     (let loop ((i (- (vector-length row) 2)))
       (cond
         ((< i 0) #t)
         ((ps:!= (vector-ref row i) (vector-ref last-row-processed i)) #f)
         (else (loop (- i 1))))))
   (set! last-row-processed row)
   (not copy?))
 
 (define (update-local-extremum row i operator)
   (define updated? #f)
   (define new-value (vector-ref row i))
   (hashtable-update!
    (vector-ref local-extrema i)
    (let ((row-extremum-key (vector-map (lambda (e) e) row)))
      (vector-set! row-extremum-key 0 operator)
      (vector-set! row-extremum-key i operator)
      (vector-set! row-extremum-key (- (vector-length row-extremum-key) 1) operator)
      row-extremum-key)
    (lambda (hashed)
      (define old-value? (and (not (eq? hashed row)) (vector-ref hashed i)))
      (cond
        ((not old-value?)
         (set! updated? #t)
         row)
        ((operator new-value old-value?)
         (set! updated? #t)
         (discard-extremum hashed)
         row)
        (else hashed)))
    row)
   updated?)

 (define (update-global-extremum row i operator)
   (define updated? #f)
   (define new-value (vector-ref row i))
   (hashtable-update!
    (vector-ref global-extrema i)
    operator
    (lambda (hashed)
      (define old-value? (and (not (eq? hashed row)) (vector-ref (car hashed) i)))
      (cond
        ((not old-value?)
         (set! updated? #t)
         (list row))
        ((ps:== new-value old-value?)
         (set! updated? #t)
         (cons row hashed))
        ((operator new-value old-value?)
         (set! updated? #t)
         (for-each discard-extremum hashed)
         (list row))
        (else hashed)))
    row)
   updated?)

 (define (discard-extremum row)
   (define last-index (- (vector-length row) 1))
   (define local-extrema-left (- (vector-ref row last-index) 1))
   (vector-set! row last-index local-extrema-left)
   (when (= local-extrema-left 0)
     (discard-row row)))

 (define (discard-row row)
   (vector-set! row (- (vector-length row) 1) #f)))