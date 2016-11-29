; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (profiling-scripts extract)
 (export ps:!= ps:== ps:< ps:> ps:<= ps:>= ps:min ps:max ps:MIN ps:MAX
         initialise update-local-extremum update-global-extremum discard-row)
 (import (rnrs))

 (define (ps:!= s1 s2)
   (not (string=? s1 s2)))
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

 (define (initialise i)
   (set! local-extrema (vector i))
   (do ((i (- i 1) (- i 1))) ((< i 0))
     (vector-set! local-extrema i (make-hashtable equal-hash equal?)))
   (set! global-extrema (vector i #f)))

 (define (update-local-extremum v i operator)
   (define updated? #f)
   (define new-value (vector-ref v i))
   (hashtable-update!
    (vector-ref local-extrema i)
    (let ((v-extremum-key (vector-map (lambda (e) e) v)))
      (vector-set! v-extremum-key i #f)
      v-extremum-key)
    (lambda (hashed)
      (cond
        ((eq? hashed v)
         (set! updated? #t)
         v)
        ((operator new-value (vector-ref hashed i))
         (set! updated? #t)
         (discard-extremum hashed)
         v)
        (else hashed)))
    v)
   updated?)

 (define (update-global-extremum v i operator)
   (define extremum (vector-ref global-extrema i))
   (cond
     ((not extremum)
      (vector-set! global-extrema i v)
      #t)
     ((operator (vector-ref v i) (vector-ref extremum i))
      (discard-extremum extremum)
      (vector-set! global-extrema i v)
      #t)
     (else #f)))

 (define (discard-extremum v)
   (define last-index (- (vector-length v) 1))
   (define local-extrema-left (- (vector-ref v last-index) 1))
   (vector-set! v last-index local-extrema-left)
   (when (= local-extrema-left 0)
     (discard-row v)))

 (define (discard-row v)
   (vector-set! v (- (vector-length v) 1) #f)))
