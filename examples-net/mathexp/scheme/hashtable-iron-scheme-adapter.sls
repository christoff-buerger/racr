; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: D. Langner, C. Bürger

(library
 (hashtable-iron-scheme-adapter)
 (export
  hashtable-ref*
  hashtable-set!*
  hashtable-delete!*
  hashtable-contains?*
  hashtable-entries*)
 (import (rnrs) (rnrs mutable-pairs))
 
 (define-record-type nil-record (sealed #t) (opaque #t))
 (define nil (make-nil-record))
 
 (define hashtable-ref*
   (lambda (h k d)
     (hashtable-ref h (if (null? k) nil k) d)))
 
 (define hashtable-set!*
   (lambda (h k v)
     (hashtable-set! h (if (null? k) nil k) v)))
 
 (define hashtable-delete!*
   (lambda (h k)
     (hashtable-delete! h (if (null? k) nil k))))
 
 (define hashtable-contains?*
   (lambda (h k)
     (hashtable-contains? h (if (null? k) nil k))))
 
 (define hashtable-entries*
   (lambda (h)
     (let-values
         (((kv vv) (hashtable-entries h)))
       (values
        (vector-map
         (lambda (e)
           (if (null? e) nil e))
         kv)
        vv)))))
