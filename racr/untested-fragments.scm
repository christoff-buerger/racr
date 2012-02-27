#!r6rs

(define-syntax ast-fold-left-children
   (lambda (x)
     (syntax-case x ()
       ((_ f start-value n b ...)
        #'(let ((f* f)
                (result start-value))
            (ast-for-each-child
              (lambda (result i child)
                (set! result (f* result i child)))
              n
              b ...)
             result)))))

(define-syntax ast-map-children
  (lambda (x)
    (syntax-case x ()
      ((_ f n b ...)
       #'(let ((f* f))
           (reverse
            (ast-fold-left-children
             (lambda (result i child)
               (cons (f* i child) result))
             (list)
             n
             b ...)))))))