; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (profiling-scripts extract)
 (export add-padding ps:!= ps:== ps:< ps:> ps:<= ps:>= ps:min ps:max ps:MIN ps:MAX
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
 (define date-column #f)
 (define last-row-processed #f)

 (define (initialise i measurement-date-column)
   (set! local-extrema (make-vector i))
   (do ((i (- i 1) (- i 1))) ((< i 0))
     (vector-set! local-extrema i (make-hashtable equal-hash equal?)))
   (set! global-extrema (make-vector i))
   (do ((i (- i 1) (- i 1))) ((< i 0))
     (vector-set! global-extrema i (make-eq-hashtable)))
   (set! date-column measurement-date-column)
   (set! last-row-processed (make-vector i "")))

 (define (unique-row? v)
   (define copy?
     (let loop ((i (- (vector-length v) 2)))
       (cond
         ((< i 0) #t)
         ((not (ps:== (vector-ref v i) (vector-ref last-row-processed i))) #f)
         (else (loop (- i 1))))))
   (set! last-row-processed v)
   (not copy?))
 
 (define (update-local-extremum v i operator)
   (define updated? #f)
   (define new-value (vector-ref v i))
   (hashtable-update!
    (vector-ref local-extrema i)
    (let ((v-extremum-key (vector-map (lambda (e) e) v)))
      (vector-set! v-extremum-key i operator)
      (vector-set! v-extremum-key date-column operator)
      v-extremum-key)
    (lambda (hashed)
      (define old-value? (and (not (eq? hashed v)) (vector-ref hashed i)))
      (cond
        ((not old-value?)
         (set! updated? #t)
         v)
        ((operator new-value old-value?)
         (set! updated? #t)
         (discard-extremum hashed)
         v)
        (else hashed)))
    v)
   updated?)

 (define (update-global-extremum v i operator)
   (define updated? #f)
   (define new-value (vector-ref v i))
   (hashtable-update!
    (vector-ref global-extrema i)
    operator
    (lambda (hashed)
      (define old-value? (and (not (eq? hashed v)) (vector-ref (car hashed) i)))
      (cond
        ((not old-value?)
         (set! updated? #t)
         (list v))
        ((ps:== new-value old-value?)
         (set! updated? #t)
         (cons v hashed))
        ((operator new-value old-value?)
         (set! updated? #t)
         (for-each discard-extremum hashed)
         (list v))
        (else hashed)))
    v)
   updated?)

 (define (discard-extremum v)
   (define last-index (- (vector-length v) 1))
   (define local-extrema-left (- (vector-ref v last-index) 1))
   (vector-set! v last-index local-extrema-left)
   (when (= local-extrema-left 0)
     (discard-row v)))

 (define (discard-row v)
   (vector-set! v (- (vector-length v) 1) #f)))