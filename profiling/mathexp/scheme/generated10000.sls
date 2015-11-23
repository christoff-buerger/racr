
(library
 (generated)
 (export tree)
 (import (rnrs) (racr core))
 (define tree
  (lambda (spec)
   (with-specification spec
    (create-ast 'MulExp (list
     (create-ast 'AddExp (list
      (create-ast 'AddExp (list
       (create-ast 'AddExp (list
        (create-ast 'MulExp (list
         (create-ast 'AddExp (list
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "y"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "y"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "j"))
                    (create-ast 'Number (list 1.0))))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Const (list "d"))))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'Const (list "q"))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "u"))))
                (create-ast 'Const (list "n"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'Const (list "v"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Const (list "y"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "m"))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "e"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "y"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "r"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "n"))))
                (create-ast 'Number (list 2.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'Number (list 3.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "q"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Const (list "d"))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "e"))))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "o"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 7.0))))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "k"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'Const (list "j"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "p"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "j"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "l"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "q"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "b"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 1.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "e"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 8.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Const (list "c"))))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "d"))))
                  (create-ast 'Number (list 8.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "c"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "n"))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Const (list "d"))))
                (create-ast 'Const (list "p"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "m"))))))
                (create-ast 'Number (list 5.0))))))
              (create-ast 'AddExp (list
               (create-ast 'Const (list "z"))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "t"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "j"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "p"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "s"))))
                (create-ast 'Number (list 5.0))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Const (list "q"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "b"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'Number (list 7.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "i"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Const (list "w"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 6.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'Number (list 2.0))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'Const (list "d"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'Const (list "k"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 3.0))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "k"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "v"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "w"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Const (list "i"))))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "p"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "n"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Const (list "x"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "l"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "o"))))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'Number (list 3.0))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "r"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 4.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Const (list "d"))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Const (list "v"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "t"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "i"))))))))))))))))))
         (create-ast 'MulExp (list
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Const (list "t"))))
                (create-ast 'Number (list 3.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Number (list 8.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Const (list "e"))))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 7.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "u"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "f"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "b"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Number (list 6.0))))))))
                (create-ast 'Const (list "v"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "e"))
                   (create-ast 'Number (list 8.0))))))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Number (list 1.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "l"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "h"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "v"))))
                (create-ast 'Number (list 7.0))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "s"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'Const (list "v"))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'Const (list "v"))))
                (create-ast 'Const (list "o"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Number (list 7.0))))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Const (list "o"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "i"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "m"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Const (list "l"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 8.0))))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'Number (list 5.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "w"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Const (list "c"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "n"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "j"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "x"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Const (list "b"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 7.0))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "b"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'Const (list "w"))))
                (create-ast 'Const (list "w"))))))
              (create-ast 'AddExp (list
               (create-ast 'Const (list "w"))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "b"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'Number (list 4.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "u"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'Number (list 6.0))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "u"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "n"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "k"))))
                (create-ast 'Const (list "c"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "a"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'Const (list "a"))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "v"))
                (create-ast 'Const (list "z"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "w"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "j"))
                   (create-ast 'Const (list "e"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Number (list 2.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "z"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "q"))))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 9.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "o"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "g"))
                (create-ast 'Number (list 5.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "v"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "b"))))))
                (create-ast 'Number (list 8.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 2.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "w"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 9.0))
                    (create-ast 'Const (list "j"))))))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'Const (list "c"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'Number (list 3.0))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "g"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 6.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Const (list "w"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 1.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Const (list "p"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "o"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Const (list "l"))))))))))))))))))
        (create-ast 'AddExp (list
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 6.0))))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'Const (list "w"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'Const (list "p"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Const (list "q"))))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Const (list "b"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "n"))
                    (create-ast 'Const (list "u"))))))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "i"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "u"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "a"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Number (list 1.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "m"))))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'Const (list "r"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Const (list "e"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 7.0))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "u"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "x"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "o"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "f"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 5.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "l"))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "e"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "e"))
                   (create-ast 'Const (list "f"))))
                  (create-ast 'Const (list "e"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'Const (list "a"))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Const (list "g"))))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'Const (list "a"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "h"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "y"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Const (list "k"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "s"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "f"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "h"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Const (list "d"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "d"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Number (list 9.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "h"))))
                  (create-ast 'Const (list "k"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "m"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "g"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Const (list "q"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 8.0))))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'Const (list "a"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Number (list 2.0))))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Const (list "a"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "g"))
                (create-ast 'Const (list "v"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "k"))))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "p"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "t"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Const (list "t"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "y"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "a"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "i"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "s"))))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'Const (list "j"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "r"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Const (list "t"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 8.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Const (list "d"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "j"))))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "j"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "p"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'Const (list "u"))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "k"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Number (list 2.0))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "o"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "r"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "s"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "k"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'Const (list "a"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "i"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "s"))))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "t"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "h"))))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "e"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Const (list "d"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "y"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Number (list 7.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Const (list "y"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "a"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "b"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "y"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "f"))))
                (create-ast 'Number (list 2.0))))))))))))))))
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "n"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Const (list "h"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'Const (list "d"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "f"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'Const (list "a"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "h"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "d"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 6.0))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'Const (list "o"))))
                (create-ast 'Number (list 8.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "t"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 5.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "z"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "v"))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Number (list 8.0))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Number (list 6.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'Const (list "j"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'Const (list "t"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 2.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'Const (list "i"))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "s"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "s"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "p"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "f"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Const (list "c"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 6.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 9.0))))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'Const (list "j"))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "n"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "a"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "a"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Number (list 5.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 5.0))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'Const (list "d"))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "f"))))))
               (create-ast 'Number (list 8.0))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "c"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Const (list "f"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Const (list "z"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Const (list "g"))))
                  (create-ast 'Const (list "i"))))))
                (create-ast 'Number (list 3.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 1.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "u"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "i"))))))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "n"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 9.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "z"))
                   (create-ast 'Const (list "p"))))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'Const (list "w"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "k"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "s"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "p"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "d"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Number (list 9.0))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "f"))))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'Const (list "x"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "e"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "q"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "k"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 8.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 4.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "f"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "f"))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "b"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "b"))))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "s"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 8.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'Const (list "d"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "z"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "i"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 9.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "l"))))
                (create-ast 'Number (list 4.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "v"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 6.0))))))))))))))))))))))))
       (create-ast 'AddExp (list
        (create-ast 'MulExp (list
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'Number (list 9.0))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "c"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Const (list "b"))))
                (create-ast 'Number (list 3.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "k"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "f"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "w"))))
                (create-ast 'Const (list "n"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "y"))))
                (create-ast 'Const (list "o"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'Const (list "k"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Const (list "e"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "s"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "h"))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "o"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "j"))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Number (list 8.0))))))
                 (create-ast 'Const (list "n"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "t"))
                (create-ast 'Const (list "c"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Number (list 6.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "s"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'Number (list 3.0))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Const (list "k"))))
                (create-ast 'Number (list 6.0))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "b"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Number (list 3.0))))))))
                (create-ast 'Const (list "r"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Number (list 8.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "v"))))
                  (create-ast 'Const (list "t"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "l"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "t"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 4.0))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "s"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "c"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "q"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "y"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Const (list "b"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "o"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'Number (list 6.0))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "u"))
                (create-ast 'Const (list "n"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "x"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Const (list "q"))))))
               (create-ast 'Number (list 5.0))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "v"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "u"))
                   (create-ast 'Const (list "a"))))
                  (create-ast 'Const (list "m"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "o"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "e"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 1.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "s"))))
                (create-ast 'Const (list "t"))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'Number (list 1.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'Const (list "d"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "x"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "b"))))
                  (create-ast 'Const (list "s"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "z"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "b"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "q"))))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Const (list "y"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 2.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Number (list 3.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "z"))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "u"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "m"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 4.0))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'Const (list "g"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Number (list 9.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "n"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 8.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "h"))
                    (create-ast 'Number (list 3.0))))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "g"))
                    (create-ast 'Number (list 9.0))))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "p"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Const (list "a"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 5.0))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "t"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "f"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "o"))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Const (list "k"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "l"))
                   (create-ast 'Const (list "s"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Const (list "k"))))))
                 (create-ast 'Const (list "s"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "m"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Const (list "r"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "d"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 7.0))))))))))))))))))
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 6.0))))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Const (list "t"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "t"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 2.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "l"))
                   (create-ast 'Const (list "u"))))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "j"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "a"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "d"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Const (list "q"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "m"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "k"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Const (list "b"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "x"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "m"))))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 2.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Const (list "t"))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 7.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "g"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 6.0))))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "r"))))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "h"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "r"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "a"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'Number (list 8.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "u"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'Number (list 3.0))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "c"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 8.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "c"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "d"))
                    (create-ast 'Number (list 6.0))))))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "i"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Number (list 2.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Const (list "a"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 6.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'Const (list "d"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "j"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 4.0))))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "h"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Const (list "o"))))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'Number (list 7.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "l"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "e"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 3.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "a"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "h"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Const (list "m"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "t"))))
                (create-ast 'Const (list "r"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "v"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "k"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "c"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Const (list "y"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "v"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "h"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Const (list "i"))))))
                 (create-ast 'Const (list "z"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "a"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "j"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "q"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "u"))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Const (list "f"))))
                (create-ast 'Const (list "w"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Const (list "l"))))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Const (list "v"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Const (list "r"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 6.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'Const (list "t"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "h"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Const (list "z"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'Const (list "a"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "v"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "q"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "k"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 2.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Number (list 8.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "q"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "j"))))))))))))))))))))
        (create-ast 'AddExp (list
         (create-ast 'AddExp (list
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "k"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "d"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "h"))
                (create-ast 'Const (list "k"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "q"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Const (list "u"))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'Const (list "b"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Const (list "t"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "u"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "p"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 4.0))
                    (create-ast 'Const (list "w"))))
                   (create-ast 'Const (list "f"))))))
                 (create-ast 'Const (list "e"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "t"))))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Const (list "k"))))
               (create-ast 'Number (list 9.0))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "t"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "c"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Const (list "o"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "w"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "y"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Const (list "u"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "i"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "k"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "q"))))
                (create-ast 'Number (list 4.0))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "m"))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Const (list "b"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'Const (list "n"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 8.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'Const (list "c"))))
                (create-ast 'Const (list "y"))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'Const (list "h"))
               (create-ast 'Number (list 6.0))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "d"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "k"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "x"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "g"))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'Number (list 3.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Const (list "i"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 8.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'Number (list 8.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Const (list "x"))))
                (create-ast 'Const (list "m"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "f"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'Const (list "s"))))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "n"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Const (list "k"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 5.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "f"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "c"))
                (create-ast 'Const (list "f"))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'Const (list "e"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "c"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "z"))))
                (create-ast 'Number (list 2.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Number (list 7.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "h"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "k"))))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "q"))
                    (create-ast 'Const (list "r"))))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "x"))
                    (create-ast 'Const (list "m"))))))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'Const (list "s"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'Const (list "k"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "e"))))))
                (create-ast 'Const (list "r"))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Const (list "w"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'Const (list "a"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "p"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "o"))))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Const (list "g"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'Const (list "y"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "y"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "m"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 3.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "p"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "o"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "o"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "d"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "p"))))))))))))))))))))
         (create-ast 'AddExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "u"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "k"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 3.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Const (list "z"))))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "i"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "d"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Number (list 8.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "r"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Number (list 3.0))))))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Const (list "b"))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "i"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "l"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'Const (list "s"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 9.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "l"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "g"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Const (list "h"))))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Const (list "k"))))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "p"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'Number (list 1.0))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 1.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "n"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "s"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "k"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Number (list 6.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 9.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "i"))))
                  (create-ast 'Const (list "k"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "s"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Const (list "s"))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "f"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 2.0))))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Const (list "r"))))
                (create-ast 'Const (list "y"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "c"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "m"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "e"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "d"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 1.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "l"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "w"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Number (list 3.0))))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Const (list "d"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "g"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "b"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 3.0))))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Number (list 4.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "x"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 9.0))))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "p"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "f"))))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "b"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "t"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Const (list "c"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 2.0))
                    (create-ast 'Number (list 8.0))))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "z"))
                    (create-ast 'Number (list 6.0))))))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'Number (list 3.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Number (list 3.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'Const (list "u"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "s"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "y"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "a"))
                (create-ast 'Const (list "j"))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "h"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "o"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Const (list "i"))))
                  (create-ast 'Number (list 7.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "m"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "j"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "z"))
                   (create-ast 'Const (list "q"))))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 2.0))))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "t"))))
                (create-ast 'Const (list "y"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "a"))))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'Const (list "i"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 1.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "n"))))))))
               (create-ast 'Number (list 1.0))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Const (list "z"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "c"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "b"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "g"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'Const (list "b"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Const (list "z"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "m"))))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "a"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "t"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "k"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "z"))))))))))))))))))))))))))
      (create-ast 'AddExp (list
       (create-ast 'AddExp (list
        (create-ast 'MulExp (list
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "n"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "i"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 1.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'Number (list 8.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "u"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 9.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "u"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'Const (list "w"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "x"))
                    (create-ast 'Const (list "u"))))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "o"))
                   (create-ast 'Number (list 5.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 1.0))))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 1.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "l"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "l"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Const (list "p"))))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'Number (list 5.0))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "m"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "m"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "o"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 1.0))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "j"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "f"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "v"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'Const (list "l"))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 9.0))))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "v"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Number (list 6.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "f"))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "y"))))))
                (create-ast 'Const (list "n"))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "o"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 2.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "j"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Const (list "f"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 1.0))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'MulExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 2.0))
                    (create-ast 'Number (list 1.0))))
                   (create-ast 'Number (list 7.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 1.0))))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "w"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Const (list "b"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "k"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "l"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "s"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "q"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "s"))))
                  (create-ast 'Const (list "x"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "e"))))
                (create-ast 'Const (list "s"))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "r"))))))
               (create-ast 'Number (list 9.0))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "h"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'Const (list "z"))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Const (list "s"))))
                  (create-ast 'Const (list "l"))))))
                (create-ast 'Number (list 5.0))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "b"))))
                (create-ast 'Const (list "e"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "v"))))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "u"))
                   (create-ast 'Const (list "m"))))
                  (create-ast 'Number (list 5.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Number (list 2.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Const (list "e"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Const (list "z"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "p"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "i"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Number (list 3.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "m"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "l"))))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'Const (list "p"))))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "c"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "c"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "w"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "r"))))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 5.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'Const (list "z"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "m"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Const (list "j"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 8.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "s"))))
                (create-ast 'Number (list 1.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "w"))))
                (create-ast 'Number (list 9.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "k"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'Const (list "r"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "m"))))
                (create-ast 'Number (list 4.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "p"))))
                (create-ast 'Const (list "b"))))))))))))))))
         (create-ast 'AddExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'Number (list 4.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "m"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Const (list "q"))))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 8.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "x"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "d"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Number (list 7.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "c"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'Const (list "p"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "k"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "i"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "o"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "n"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Const (list "t"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 5.0))))))
                 (create-ast 'Number (list 3.0))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "o"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "b"))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "o"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "l"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "a"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'Const (list "f"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "k"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 1.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'Const (list "o"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "n"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Number (list 7.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Const (list "w"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "d"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "l"))))
                (create-ast 'Const (list "h"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 5.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "d"))))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "l"))
                   (create-ast 'Const (list "a"))))))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "d"))))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Const (list "a"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "w"))))
                  (create-ast 'Const (list "f"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 9.0))))))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 6.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "o"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "o"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Const (list "h"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "q"))))
                (create-ast 'Number (list 3.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Const (list "z"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "t"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Const (list "h"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "h"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "m"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "y"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "b"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Const (list "q"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 5.0))))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "v"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "p"))))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Const (list "d"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "f"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 6.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Number (list 1.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 8.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "u"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 3.0))))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Const (list "e"))))
                  (create-ast 'Number (list 5.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "h"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "n"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "w"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 4.0))))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Number (list 8.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'Number (list 8.0))))))))))))))))))))
        (create-ast 'MulExp (list
         (create-ast 'AddExp (list
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Const (list "x"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "t"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "j"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "o"))
                   (create-ast 'Number (list 1.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Const (list "f"))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Const (list "i"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "n"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "s"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Const (list "u"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Const (list "a"))))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'Const (list "u"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "q"))))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'Const (list "p"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 1.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "z"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 9.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "h"))))))
                 (create-ast 'Const (list "y"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'Const (list "o"))))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Const (list "t"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "s"))))))
                (create-ast 'Number (list 6.0))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "s"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "b"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Const (list "q"))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "p"))
                (create-ast 'Number (list 3.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "b"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'Number (list 7.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "i"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "q"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "g"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Const (list "d"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "e"))))
                (create-ast 'Const (list "o"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "i"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Number (list 1.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "a"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "o"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "m"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "f"))
                    (create-ast 'Number (list 4.0))))))
                  (create-ast 'Const (list "w"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "q"))))))
                (create-ast 'Number (list 4.0))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "v"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'Number (list 4.0))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "z"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'Const (list "b"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Const (list "o"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "q"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'Const (list "l"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "u"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "g"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Number (list 6.0))))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "r"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "m"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'Const (list "y"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Number (list 6.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "v"))))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "y"))))))
                (create-ast 'Const (list "y"))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 4.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "o"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "g"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 5.0))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "e"))))
                (create-ast 'Const (list "k"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "d"))))
                  (create-ast 'Number (list 5.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "l"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "f"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "n"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "f"))
                (create-ast 'Const (list "q"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Const (list "j"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "a"))))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 5.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "x"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "a"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Number (list 1.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "t"))))
                (create-ast 'Number (list 7.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Number (list 2.0))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "j"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "d"))
                    (create-ast 'Number (list 2.0))))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Const (list "x"))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "k"))))))))))))))))))
         (create-ast 'MulExp (list
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "v"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 3.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "c"))
                (create-ast 'Const (list "h"))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "b"))))
                (create-ast 'Const (list "i"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "a"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 3.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "e"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'Number (list 9.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "g"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 3.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "f"))))
                (create-ast 'Const (list "j"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Number (list 6.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 3.0))
                    (create-ast 'Const (list "h"))))
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 8.0))
                    (create-ast 'Const (list "x"))))))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Number (list 6.0))))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "m"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Const (list "o"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Number (list 8.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "i"))
                (create-ast 'Number (list 2.0))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'Const (list "s"))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'Const (list "b"))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Const (list "k"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "n"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Const (list "o"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Number (list 8.0))))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "m"))))))
                (create-ast 'Const (list "q"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "o"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 7.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Number (list 3.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "e"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "p"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "a"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "n"))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 7.0))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'Number (list 1.0))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'Const (list "s"))))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'MulExp (list
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 3.0))
                    (create-ast 'Number (list 3.0))))
                   (create-ast 'Const (list "g"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 3.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Number (list 8.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "l"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Const (list "q"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "j"))))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "e"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "d"))))
                  (create-ast 'Const (list "k"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "a"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "y"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Const (list "j"))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "t"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "j"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Const (list "d"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "y"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "e"))))))
                (create-ast 'Number (list 6.0))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "v"))
                (create-ast 'Const (list "b"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Const (list "r"))))))))
               (create-ast 'Const (list "a"))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'Const (list "n"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Const (list "b"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "t"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'Const (list "z"))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "c"))
                (create-ast 'Number (list 6.0))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Number (list 5.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "t"))))
                  (create-ast 'Const (list "g"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "n"))))
                (create-ast 'Const (list "x"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 4.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Const (list "q"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'Number (list 3.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "c"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 1.0))))))))
                (create-ast 'Const (list "i"))))))))))))))))))))
       (create-ast 'MulExp (list
        (create-ast 'MulExp (list
         (create-ast 'MulExp (list
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "j"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Const (list "e"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "q"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Const (list "w"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Const (list "g"))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "s"))))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "t"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 3.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "x"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "d"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 8.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "q"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Const (list "w"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 8.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "i"))))
                (create-ast 'Number (list 8.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "m"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "f"))))))
                 (create-ast 'Number (list 4.0))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Const (list "p"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Const (list "x"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Const (list "u"))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Number (list 4.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Const (list "n"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 8.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "a"))))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "u"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "p"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "x"))))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Number (list 1.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "p"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Const (list "a"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "q"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'MulExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "s"))
                    (create-ast 'Number (list 4.0))))
                   (create-ast 'Number (list 5.0))))))
                 (create-ast 'Const (list "t"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Const (list "x"))))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "l"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "b"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'Const (list "p"))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "a"))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Const (list "r"))))))
              (create-ast 'AddExp (list
               (create-ast 'Const (list "d"))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "h"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "c"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "e"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "m"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "g"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "t"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "e"))))
                (create-ast 'Number (list 1.0))))
               (create-ast 'Const (list "z"))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "d"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'Number (list 2.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Const (list "s"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'Number (list 9.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "r"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "o"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "c"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "t"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'Number (list 2.0))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "n"))))
                (create-ast 'Number (list 8.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "b"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "h"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 5.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 1.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "p"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "s"))))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "h"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'Const (list "l"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "i"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "r"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "g"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "i"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "k"))))
                (create-ast 'Const (list "i"))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "r"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "v"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Number (list 4.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "s"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 1.0))))))))))))))))))))
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Const (list "c"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "v"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "u"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Number (list 9.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Const (list "z"))))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "u"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 5.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "d"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "x"))))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "r"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'Const (list "i"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "e"))
                   (create-ast 'Number (list 9.0))))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 1.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Number (list 2.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "y"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "j"))
                    (create-ast 'Number (list 9.0))))
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 3.0))
                    (create-ast 'Number (list 1.0))))))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Const (list "h"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'Const (list "i"))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "q"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "f"))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Const (list "d"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'Const (list "z"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 1.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "m"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "l"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "e"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "m"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "j"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 7.0))
                    (create-ast 'Const (list "r"))))
                   (create-ast 'Const (list "l"))))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Const (list "t"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "m"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 6.0))))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "a"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "s"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "t"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Const (list "g"))))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "o"))
                   (create-ast 'Const (list "y"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 3.0))))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 4.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Const (list "s"))))))
                 (create-ast 'Const (list "d"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "m"))))))
                 (create-ast 'Const (list "y"))))
                (create-ast 'Number (list 1.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Const (list "p"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'Number (list 7.0))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'Const (list "v"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Const (list "c"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "j"))
                   (create-ast 'Const (list "f"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "n"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "e"))
                   (create-ast 'Const (list "s"))))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Number (list 6.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Number (list 6.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "b"))))
                  (create-ast 'Const (list "w"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Number (list 7.0))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "n"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "y"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "r"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'Number (list 5.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "j"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "y"))))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 7.0))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "j"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'Const (list "n"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "v"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "l"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "j"))
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "x"))
                    (create-ast 'Const (list "a"))))))))
                 (create-ast 'Number (list 2.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'Const (list "u"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "i"))))
                (create-ast 'Const (list "x"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "n"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Const (list "v"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "h"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "t"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "g"))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 2.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "c"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "k"))))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Const (list "r"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "x"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'Number (list 5.0))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "v"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 7.0))))))))))))))))))))
        (create-ast 'AddExp (list
         (create-ast 'AddExp (list
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Const (list "c"))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Number (list 1.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "u"))
                   (create-ast 'Number (list 9.0))))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "o"))))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Const (list "h"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "g"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "b"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Const (list "u"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "o"))
                (create-ast 'Const (list "o"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "y"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "m"))))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "s"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "k"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "c"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Const (list "z"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Number (list 6.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'Const (list "i"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 3.0))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "e"))
                   (create-ast 'Const (list "k"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "n"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Const (list "j"))))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "y"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "q"))
                   (create-ast 'Const (list "x"))))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Const (list "o"))))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'AddExp (list
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 2.0))
                    (create-ast 'Const (list "r"))))
                   (create-ast 'Number (list 3.0))))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "g"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'Const (list "f"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Const (list "k"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'Number (list 1.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'Const (list "h"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 2.0))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "u"))
                   (create-ast 'Const (list "a"))))))))
                (create-ast 'Number (list 2.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "z"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 9.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "l"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Const (list "k"))))
                  (create-ast 'Number (list 9.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "l"))))
                (create-ast 'Const (list "x"))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Const (list "i"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "x"))
                (create-ast 'Const (list "a"))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Const (list "x"))))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'Const (list "k"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "t"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Const (list "u"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "h"))))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "g"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "p"))))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "b"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "e"))
                   (create-ast 'Number (list 5.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "w"))
                    (create-ast 'Number (list 2.0))))))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "h"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "q"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 2.0))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "m"))
                (create-ast 'Const (list "k"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'Const (list "m"))))
                (create-ast 'Number (list 1.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "v"))))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "f"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "t"))))
                  (create-ast 'Const (list "b"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Const (list "p"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "l"))
                    (create-ast 'Const (list "m"))))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Number (list 6.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 1.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'Const (list "j"))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "d"))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "q"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "z"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 4.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "z"))
                   (create-ast 'Const (list "t"))))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "c"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "b"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "i"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Const (list "p"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Number (list 9.0))))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Number (list 6.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "n"))))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "b"))))
                (create-ast 'Const (list "h"))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "t"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "w"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 9.0))))))))))))))))))
         (create-ast 'MulExp (list
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "k"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 7.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "p"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Const (list "h"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Number (list 1.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Const (list "q"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Const (list "v"))))
                  (create-ast 'Const (list "p"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Const (list "d"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "x"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Const (list "j"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "p"))))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'Const (list "b"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "v"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "s"))))
                (create-ast 'Const (list "g"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Const (list "z"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "x"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 6.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "n"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Const (list "i"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'Const (list "t"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Const (list "h"))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "m"))))))))
                (create-ast 'Const (list "w"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Number (list 7.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'Number (list 3.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "b"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "b"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "s"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "e"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "i"))))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Const (list "t"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "y"))))
                (create-ast 'Number (list 9.0))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'Const (list "f"))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "v"))))
                (create-ast 'Number (list 1.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "h"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 2.0))
                    (create-ast 'Number (list 2.0))))))
                  (create-ast 'Const (list "j"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Const (list "s"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "o"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Const (list "c"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 9.0))
                    (create-ast 'Number (list 4.0))))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Number (list 6.0))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "n"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "b"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Const (list "v"))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'Number (list 9.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Number (list 6.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "x"))))))
                 (create-ast 'Const (list "c"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "e"))
                   (create-ast 'Const (list "n"))))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "c"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "q"))))))
                 (create-ast 'Const (list "r"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Const (list "p"))))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "l"))))))
                (create-ast 'Const (list "q"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Const (list "b"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Number (list 6.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "p"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 9.0))))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "u"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "q"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "f"))
                   (create-ast 'Number (list 2.0))))))
                 (create-ast 'Const (list "y"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "x"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "z"))))))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "d"))
                (create-ast 'Number (list 8.0))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Const (list "v"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 8.0))))))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 3.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Const (list "s"))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 6.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Number (list 6.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Const (list "k"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 8.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "m"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 1.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 3.0))
                    (create-ast 'Const (list "r"))))
                   (create-ast 'Const (list "g"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Const (list "j"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "g"))
                (create-ast 'Number (list 3.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 9.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "h"))))))))))))))))))))))))))
     (create-ast 'MulExp (list
      (create-ast 'MulExp (list
       (create-ast 'AddExp (list
        (create-ast 'AddExp (list
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "l"))))
                (create-ast 'Const (list "d"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Number (list 1.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "s"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'Const (list "n"))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "i"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 1.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "o"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 2.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "n"))))))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "l"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "z"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Const (list "y"))))
                (create-ast 'Const (list "k"))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Const (list "t"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "q"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "k"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 3.0))))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 3.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "z"))
                   (create-ast 'Number (list 3.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 2.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Const (list "u"))))
               (create-ast 'Number (list 5.0))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'Const (list "x"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 1.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "a"))
                (create-ast 'Const (list "h"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Number (list 7.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "z"))))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "h"))))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "s"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "q"))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "t"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "p"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "p"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'Const (list "r"))))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "g"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "d"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Const (list "h"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "p"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "u"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "q"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'Number (list 3.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Const (list "m"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 3.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "h"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "c"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Const (list "k"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "j"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Number (list 1.0))))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "s"))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Const (list "s"))))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 1.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Number (list 7.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Const (list "r"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "s"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Number (list 2.0))))))
                 (create-ast 'Number (list 2.0))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "b"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'Number (list 1.0))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'Const (list "z"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "j"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 9.0))))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Const (list "w"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "l"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "g"))))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Const (list "v"))))
                  (create-ast 'Number (list 5.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 1.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 3.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'Number (list 3.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 4.0))))))))))))))))))
         (create-ast 'AddExp (list
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Const (list "t"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "e"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "d"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 8.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "z"))
                   (create-ast 'AddExp (list
                    (create-ast 'MulExp (list
                     (create-ast 'Const (list "h"))
                     (create-ast 'Const (list "d"))))
                    (create-ast 'Number (list 4.0))))))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "g"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'Number (list 2.0))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "w"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "b"))))))
                (create-ast 'Number (list 2.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'Number (list 6.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'Number (list 3.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "n"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "t"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "t"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "d"))
                (create-ast 'Number (list 7.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "v"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "u"))))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "g"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "m"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'Const (list "c"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "y"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 4.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "v"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Const (list "v"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "j"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Number (list 2.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 2.0))
                    (create-ast 'Const (list "l"))))))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Number (list 1.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Const (list "a"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 7.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "y"))))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "c"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "s"))))))
               (create-ast 'Number (list 9.0))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "e"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "o"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 1.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'Number (list 3.0))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Const (list "v"))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "h"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 7.0))
                    (create-ast 'Number (list 7.0))))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 5.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "h"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "a"))))
                  (create-ast 'Const (list "q"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "b"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Const (list "h"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "z"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "l"))
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 5.0))
                    (create-ast 'Number (list 6.0))))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "c"))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "n"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Const (list "f"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Number (list 7.0))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "m"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Const (list "a"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Const (list "e"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Const (list "u"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "q"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 3.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Number (list 9.0))))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "g"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "y"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "j"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "l"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 3.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'Const (list "w"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 4.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "o"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "b"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 1.0))))))))))))))))))))
        (create-ast 'AddExp (list
         (create-ast 'MulExp (list
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "m"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 8.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'Number (list 1.0))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'Number (list 8.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Const (list "h"))))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 5.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Const (list "w"))))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "k"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "h"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "w"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'Const (list "h"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "i"))))
                (create-ast 'Const (list "h"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Number (list 5.0))))))))
                (create-ast 'Const (list "d"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "i"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "s"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'Number (list 9.0))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "b"))))))
                 (create-ast 'Const (list "y"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 4.0))))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "n"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 7.0))))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'MulExp (list
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "b"))
                    (create-ast 'Number (list 3.0))))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Number (list 9.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "l"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Const (list "y"))))))
                 (create-ast 'Const (list "b"))))
                (create-ast 'Const (list "d"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Const (list "c"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "y"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "n"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'Const (list "v"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Const (list "z"))))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Const (list "a"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 5.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "c"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "u"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Number (list 5.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "j"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "f"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'Number (list 3.0))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Const (list "q"))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "p"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "x"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "p"))))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "k"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "i"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'Const (list "m"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "m"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "o"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "p"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "x"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'Const (list "u"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "x"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "q"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "v"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 8.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "g"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "d"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "f"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "i"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 5.0))))))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "s"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Number (list 2.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'Number (list 4.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "i"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 5.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Number (list 3.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "p"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "u"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Number (list 1.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "n"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "w"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Const (list "m"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "h"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Const (list "u"))))
                  (create-ast 'Number (list 3.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "z"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'Number (list 6.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'Const (list "t"))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'Const (list "w"))))))))))))))))
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'Const (list "k"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Const (list "n"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "m"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Const (list "y"))))
                (create-ast 'Number (list 1.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "u"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "j"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "g"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Number (list 5.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "i"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 9.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'Number (list 5.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Const (list "b"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "y"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Const (list "w"))))
                (create-ast 'Number (list 8.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Number (list 3.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "b"))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "y"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'Number (list 3.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "t"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "i"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Const (list "o"))))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "k"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "n"))))
                (create-ast 'Number (list 7.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "y"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 1.0))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'Const (list "x"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "n"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "o"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "t"))
                (create-ast 'Number (list 4.0))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "y"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 5.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "d"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Number (list 6.0))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Const (list "a"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "k"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Const (list "a"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "j"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Const (list "p"))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 7.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Number (list 6.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "w"))))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'Const (list "j"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "s"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Const (list "d"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "m"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Number (list 6.0))))
                  (create-ast 'Const (list "h"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "n"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "k"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Const (list "o"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "f"))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "i"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "e"))
                   (create-ast 'Const (list "s"))))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "y"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "j"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Number (list 3.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "c"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Const (list "d"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 2.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'Const (list "c"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Number (list 7.0))))))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Const (list "r"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'Number (list 6.0))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "j"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "j"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "k"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Const (list "v"))))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "q"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "k"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 5.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 6.0))))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Number (list 8.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "s"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 4.0))))))))))))))))))))))))
       (create-ast 'AddExp (list
        (create-ast 'MulExp (list
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "n"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'Number (list 9.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "e"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Const (list "q"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "m"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 2.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "n"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Number (list 1.0))))))
              (create-ast 'AddExp (list
               (create-ast 'Const (list "b"))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'Const (list "j"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Number (list 3.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Const (list "t"))))
                (create-ast 'Number (list 7.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "v"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "g"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Const (list "l"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 2.0))))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "s"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "p"))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Const (list "m"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'Number (list 4.0))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Const (list "m"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "b"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Const (list "j"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "c"))))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "p"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Const (list "m"))))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 3.0))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'Number (list 5.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'Number (list 5.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "a"))))
                (create-ast 'Const (list "z"))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "c"))
                (create-ast 'Const (list "v"))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "v"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "c"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "d"))))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "o"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "b"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Number (list 9.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 9.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "d"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'Const (list "o"))))
                (create-ast 'Number (list 9.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "o"))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 6.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "i"))))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "f"))))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "f"))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "i"))
                (create-ast 'Const (list "m"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "s"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "u"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Const (list "m"))))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "l"))))
                  (create-ast 'Number (list 5.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "s"))))))
                 (create-ast 'Const (list "z"))))
                (create-ast 'Number (list 6.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "i"))))))
                (create-ast 'Const (list "r"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Number (list 7.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Const (list "t"))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Number (list 3.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Const (list "k"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'Const (list "b"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "h"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "o"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "t"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "f"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Const (list "p"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "o"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'Const (list "q"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "t"))))
                (create-ast 'Const (list "s"))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "c"))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'AddExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "u"))
                    (create-ast 'Number (list 3.0))))
                   (create-ast 'Number (list 1.0))))))
                 (create-ast 'Const (list "w"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Number (list 6.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Number (list 7.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "d"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "z"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "q"))))
                (create-ast 'Const (list "c"))))))))))))))))
         (create-ast 'AddExp (list
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'Const (list "u"))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "s"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "s"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Const (list "c"))))
                (create-ast 'Number (list 4.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'MulExp (list
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 4.0))
                    (create-ast 'Number (list 6.0))))
                   (create-ast 'Const (list "d"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "f"))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "y"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "x"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "h"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Const (list "z"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "z"))))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'Const (list "h"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "g"))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "g"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "q"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "g"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'Const (list "u"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "a"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 3.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "n"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "q"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "t"))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "h"))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "f"))
                    (create-ast 'Const (list "m"))))))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "a"))))
                  (create-ast 'Number (list 7.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Const (list "f"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "y"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "e"))))))
                 (create-ast 'Const (list "l"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "t"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "b"))))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Number (list 8.0))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Const (list "k"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Const (list "a"))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "n"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Const (list "u"))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Const (list "f"))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Const (list "h"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "m"))))
                (create-ast 'Const (list "q"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 7.0))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "t"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "y"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Const (list "d"))))
                (create-ast 'Const (list "f"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Const (list "z"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "p"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Const (list "l"))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "j"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Const (list "v"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "k"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Const (list "c"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Const (list "d"))))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "h"))
                (create-ast 'Number (list 8.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Number (list 2.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Const (list "j"))))
                  (create-ast 'Const (list "b"))))))
                (create-ast 'Number (list 9.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'Const (list "r"))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'Number (list 3.0))))))
              (create-ast 'MulExp (list
               (create-ast 'Const (list "p"))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "m"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Const (list "p"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "s"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "u"))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "a"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Const (list "e"))))
                (create-ast 'Const (list "w"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'Number (list 7.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "t"))))
                (create-ast 'Const (list "o"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Number (list 8.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'Const (list "v"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "k"))))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "j"))))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "n"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "d"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 1.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "n"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "t"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "l"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "u"))))))))))))))))))))
        (create-ast 'MulExp (list
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "q"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "j"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "j"))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "d"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "l"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "a"))))
                (create-ast 'Number (list 6.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Const (list "z"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "h"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "f"))))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Const (list "c"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 2.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "m"))))
                (create-ast 'Const (list "s"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "a"))
                (create-ast 'Const (list "n"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'Const (list "n"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Const (list "r"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "o"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 9.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Number (list 9.0))))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "i"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Const (list "w"))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'Number (list 8.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "a"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "h"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "b"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "p"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "c"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'Const (list "v"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 6.0))))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "g"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Const (list "u"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Number (list 6.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "w"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "b"))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Const (list "k"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "p"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "q"))))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'Const (list "e"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Const (list "n"))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "k"))))
                (create-ast 'Const (list "w"))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'Const (list "g"))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "a"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "b"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "h"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "k"))))))
               (create-ast 'Number (list 1.0))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'Number (list 2.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "j"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "o"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'Const (list "z"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Const (list "h"))))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "c"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'Const (list "t"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 2.0))))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "b"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "a"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "w"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'Const (list "x"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "x"))
                (create-ast 'Number (list 3.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "g"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 7.0))))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'Const (list "c"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'Const (list "r"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Const (list "w"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Const (list "m"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'MulExp (list
                     (create-ast 'Const (list "j"))
                     (create-ast 'Number (list 9.0))))
                    (create-ast 'Const (list "l"))))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "w"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "m"))))))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "i"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 7.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Number (list 6.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 3.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "c"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Const (list "j"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "d"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "u"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 9.0))))))))))))))))))))
         (create-ast 'MulExp (list
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "p"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Const (list "r"))))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "t"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 5.0))))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Const (list "j"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "r"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "g"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "m"))
                    (create-ast 'Const (list "k"))))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "e"))))))
                (create-ast 'Number (list 8.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 6.0))))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Const (list "x"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Const (list "h"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'Const (list "j"))))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Const (list "z"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "g"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "h"))))
                (create-ast 'Const (list "o"))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'Number (list 2.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 5.0))))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Const (list "t"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'Number (list 1.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "n"))))
                (create-ast 'Const (list "m"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Const (list "x"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "b"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "z"))))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Const (list "q"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "e"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "e"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "d"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'Const (list "a"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "j"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "q"))
                    (create-ast 'Number (list 9.0))))))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "p"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 4.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Const (list "n"))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "n"))
                (create-ast 'Number (list 8.0))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "v"))))
                (create-ast 'Const (list "l"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'Const (list "q"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "z"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "c"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 6.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Const (list "r"))))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "n"))))
                (create-ast 'Number (list 4.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Number (list 8.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Const (list "d"))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 7.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "y"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "p"))))
                (create-ast 'Number (list 1.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'Const (list "k"))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "c"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "f"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'Const (list "l"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "p"))))))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Const (list "k"))))))))
                (create-ast 'Const (list "o"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "r"))))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Number (list 4.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "q"))))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'Number (list 7.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "g"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "j"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Number (list 9.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "n"))))
                (create-ast 'Number (list 6.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "v"))))
                (create-ast 'Const (list "q"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "v"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "x"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Const (list "t"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "p"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 3.0))))))))))))))))))))))))
      (create-ast 'AddExp (list
       (create-ast 'AddExp (list
        (create-ast 'MulExp (list
         (create-ast 'AddExp (list
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "g"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'Number (list 8.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Number (list 1.0))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'Number (list 4.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'Number (list 2.0))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "o"))
                   (create-ast 'Const (list "f"))))))
                 (create-ast 'Number (list 1.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 7.0))))))
                 (create-ast 'Const (list "l"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 1.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'Const (list "j"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "g"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "a"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 2.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Const (list "w"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Number (list 5.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Const (list "r"))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 3.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "s"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 3.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "b"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'Const (list "q"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Number (list 6.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Const (list "d"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "z"))
                    (create-ast 'Const (list "o"))))))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'Const (list "e"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 1.0))))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "u"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Const (list "u"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "q"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "h"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 2.0))))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "l"))
                   (create-ast 'Number (list 6.0))))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "l"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 9.0))))))
                 (create-ast 'Const (list "e"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "b"))))
                (create-ast 'Const (list "d"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "h"))))))
                (create-ast 'Number (list 4.0))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'Const (list "r"))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "d"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "u"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "q"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 8.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "m"))
                (create-ast 'Const (list "k"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "s"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Const (list "w"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "h"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "g"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "u"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'Number (list 5.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "z"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Number (list 6.0))))))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'Const (list "x"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "p"))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "a"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Number (list 9.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "w"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "t"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'Const (list "s"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "f"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "v"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Const (list "w"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 2.0))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "b"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "h"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "i"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 5.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "y"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 8.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 1.0))))))))))))))))))
         (create-ast 'AddExp (list
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "i"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "h"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "r"))))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Const (list "f"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "d"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "y"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'Const (list "b"))))
                (create-ast 'Number (list 8.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "v"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 1.0))
                    (create-ast 'Number (list 4.0))))))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Const (list "l"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "q"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "y"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "y"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 9.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "q"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "s"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'Const (list "n"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'Const (list "z"))))
                (create-ast 'Const (list "j"))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "i"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'Number (list 4.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'Const (list "k"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "q"))))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Const (list "j"))))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 7.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Const (list "n"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "m"))))
                (create-ast 'Number (list 1.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "a"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "l"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "i"))
                    (create-ast 'Number (list 5.0))))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 8.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'Number (list 2.0))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Number (list 3.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "a"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "s"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 8.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 9.0))
                    (create-ast 'Number (list 8.0))))))
                  (create-ast 'Const (list "s"))))))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Const (list "s"))))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Const (list "z"))))
                (create-ast 'Number (list 6.0))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "x"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'Const (list "p"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "p"))))
                  (create-ast 'Const (list "x"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "e"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "y"))))
                (create-ast 'Const (list "a"))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "v"))))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 9.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "f"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "j"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "r"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Number (list 8.0))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "y"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'Const (list "l"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 4.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "k"))))))))
                (create-ast 'Const (list "o"))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "f"))
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 4.0))
                    (create-ast 'Const (list "m"))))))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "c"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "k"))))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 5.0))))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "b"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "e"))))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "k"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "m"))))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Number (list 8.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "x"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "w"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'Const (list "w"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "f"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 3.0))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "u"))
                   (create-ast 'Const (list "j"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "u"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "r"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'Number (list 4.0))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Const (list "f"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "d"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "f"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "x"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "z"))))))))))))))))))))))))
        (create-ast 'AddExp (list
         (create-ast 'AddExp (list
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "d"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'Const (list "d"))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "p"))))
                (create-ast 'Const (list "i"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "u"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "e"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "i"))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'Const (list "i"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "t"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "r"))))))
                 (create-ast 'Const (list "d"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "c"))))
                (create-ast 'Const (list "g"))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "t"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "h"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "e"))))
                (create-ast 'Number (list 8.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "z"))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "c"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'Const (list "o"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Number (list 5.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "b"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 4.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "r"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Const (list "d"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'Const (list "p"))))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 1.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Const (list "j"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'Number (list 5.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "i"))
                    (create-ast 'Const (list "v"))))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "s"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Number (list 4.0))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'Const (list "j"))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 4.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "a"))))
                (create-ast 'Number (list 8.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "k"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 8.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 3.0))))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "h"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "o"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Number (list 6.0))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 1.0))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'Const (list "f"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "t"))))))
                (create-ast 'Const (list "o"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Const (list "c"))))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'Const (list "q"))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "r"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "k"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "f"))
                    (create-ast 'Number (list 4.0))))
                   (create-ast 'Const (list "r"))))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 5.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "s"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "k"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'Const (list "j"))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Number (list 2.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "n"))
                (create-ast 'Number (list 6.0))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "v"))
                (create-ast 'Const (list "b"))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'Const (list "x"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "f"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "u"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 2.0))))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "q"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "w"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "c"))))
                (create-ast 'Number (list 5.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "u"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'Number (list 6.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 1.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Const (list "h"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "o"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "z"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "h"))))))))))))))))))
         (create-ast 'AddExp (list
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "r"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 3.0))
                    (create-ast 'Const (list "m"))))))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Number (list 6.0))))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Const (list "u"))))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "u"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "c"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'Const (list "c"))))))
                 (create-ast 'Const (list "x"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "m"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Const (list "m"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Number (list 8.0))))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "k"))
                (create-ast 'Number (list 7.0))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "v"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Const (list "h"))))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "k"))))
                 (create-ast 'Const (list "e"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "v"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 9.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 4.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Number (list 6.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Number (list 4.0))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "a"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'Number (list 3.0))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "u"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 9.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "m"))))
                 (create-ast 'Const (list "l"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Number (list 9.0))))
                  (create-ast 'Const (list "n"))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "e"))
                   (create-ast 'Number (list 6.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Const (list "e"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "b"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "i"))))
                (create-ast 'Number (list 2.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "z"))))
                (create-ast 'Number (list 1.0))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Const (list "j"))))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'Number (list 2.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 7.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "q"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Number (list 7.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Const (list "z"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 4.0))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Const (list "i"))))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Const (list "f"))))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "p"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'Number (list 6.0))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "v"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "f"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'Const (list "v"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "t"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "h"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "k"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 8.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 7.0))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "c"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Const (list "o"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "x"))))
                (create-ast 'Number (list 1.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Const (list "l"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "z"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'Const (list "h"))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Number (list 8.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Const (list "j"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Number (list 6.0))))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "m"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "f"))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "j"))
                    (create-ast 'Const (list "g"))))))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Number (list 5.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'Const (list "b"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Const (list "r"))))))
                 (create-ast 'Const (list "v"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "n"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "t"))))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "e"))
                   (create-ast 'Const (list "t"))))
                  (create-ast 'Const (list "d"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "s"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "p"))))))))
               (create-ast 'Const (list "v"))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "v"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "t"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "r"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "i"))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Const (list "t"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "o"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "k"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Const (list "h"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Number (list 6.0))))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "q"))))))
                (create-ast 'Const (list "p"))))))))))))))))))))
       (create-ast 'MulExp (list
        (create-ast 'AddExp (list
         (create-ast 'AddExp (list
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "y"))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "u"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'Const (list "x"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "x"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "w"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'Const (list "k"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "n"))
                (create-ast 'Number (list 3.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Const (list "t"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Number (list 6.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Const (list "d"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "d"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "e"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 5.0))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "g"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Const (list "q"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "g"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "m"))))
                  (create-ast 'AddExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 9.0))
                    (create-ast 'Number (list 3.0))))
                   (create-ast 'Const (list "m"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "t"))))
                  (create-ast 'Const (list "f"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "m"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "d"))))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 3.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "x"))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 1.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "n"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "d"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "r"))
                   (create-ast 'Const (list "z"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'Const (list "n"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "o"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "o"))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Const (list "r"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 4.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'Const (list "f"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Const (list "d"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "j"))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "o"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Const (list "w"))))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 4.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'Number (list 5.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "m"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "c"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 7.0))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Const (list "h"))))))
                 (create-ast 'Const (list "g"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'AddExp (list
                    (create-ast 'Const (list "w"))
                    (create-ast 'Number (list 7.0))))))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "o"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Const (list "t"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'Const (list "s"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Const (list "d"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Const (list "w"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 9.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "e"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Const (list "g"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "i"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "a"))))
                (create-ast 'Number (list 9.0))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Const (list "d"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Const (list "o"))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "t"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "p"))
                    (create-ast 'Number (list 1.0))))
                   (create-ast 'Const (list "i"))))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "c"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Const (list "j"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "f"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "u"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "a"))
                   (create-ast 'Number (list 3.0))))))
                 (create-ast 'Number (list 1.0))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Const (list "s"))))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "l"))
                   (create-ast 'Const (list "h"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "l"))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 1.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "t"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "w"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "k"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 1.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Const (list "o"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "k"))
                   (create-ast 'Const (list "e"))))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Const (list "n"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "y"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 6.0))))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Const (list "g"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 2.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Const (list "s"))))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Const (list "l"))))
                  (create-ast 'Const (list "k"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Const (list "s"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "c"))
                   (create-ast 'Const (list "m"))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'Const (list "t"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 1.0))))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 3.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "n"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "w"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Number (list 8.0))))))))))))))))
         (create-ast 'AddExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 1.0))))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "l"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Const (list "t"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "q"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "w"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "m"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "s"))))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "z"))))))
               (create-ast 'Number (list 3.0))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'Const (list "i"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "l"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'Number (list 9.0))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "p"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "j"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "i"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "j"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "f"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "y"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "h"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Const (list "o"))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 1.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Const (list "x"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'Const (list "t"))))))
                (create-ast 'Const (list "i"))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "t"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Number (list 1.0))))))
                 (create-ast 'Const (list "n"))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'Const (list "a"))))
                (create-ast 'Const (list "t"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "h"))))
                (create-ast 'Number (list 1.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 4.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Const (list "y"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Const (list "n"))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "p"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'Number (list 3.0))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Const (list "q"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Const (list "m"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'Number (list 5.0))))))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Const (list "z"))))))))
                (create-ast 'Number (list 8.0))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "e"))))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "f"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "k"))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "o"))))
                  (create-ast 'AddExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "l"))
                    (create-ast 'Number (list 6.0))))
                   (create-ast 'Const (list "x"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 4.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'Number (list 3.0))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "p"))
                   (create-ast 'Const (list "i"))))
                  (create-ast 'Const (list "w"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "z"))
                  (create-ast 'Const (list "k"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 6.0))))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'Const (list "e"))))))))
                (create-ast 'Number (list 6.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'Const (list "f"))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Number (list 6.0))))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Const (list "b"))))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "r"))))
                  (create-ast 'Const (list "g"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'MulExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 2.0))
                    (create-ast 'Const (list "s"))))
                   (create-ast 'Const (list "q"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "o"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "f"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Number (list 9.0))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'Const (list "x"))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "f"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "w"))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Const (list "g"))))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Const (list "t"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "o"))
                 (create-ast 'Number (list 1.0))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Const (list "v"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "i"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 1.0))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Const (list "y"))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Const (list "a"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 6.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "w"))))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Const (list "k"))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'Const (list "o"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "x"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 9.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'Const (list "a"))))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'Const (list "v"))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 5.0))
                (create-ast 'Number (list 9.0))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Number (list 1.0))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Const (list "e"))))
               (create-ast 'Const (list "m"))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "s"))))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "i"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "a"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'Const (list "j"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Const (list "s"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 4.0))
                    (create-ast 'Number (list 9.0))))
                   (create-ast 'Const (list "c"))))
                  (create-ast 'Const (list "x"))))))))))))))))))))))
        (create-ast 'AddExp (list
         (create-ast 'MulExp (list
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Const (list "d"))))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "h"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "n"))
                   (create-ast 'Number (list 7.0))))
                  (create-ast 'Const (list "p"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "f"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'Const (list "h"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 5.0))))
                  (create-ast 'Const (list "d"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "p"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "b"))
                 (create-ast 'Const (list "f"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Const (list "f"))))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Const (list "b"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "n"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "v"))))))
                (create-ast 'Const (list "e"))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 4.0))
                (create-ast 'Const (list "e"))))))))))
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "c"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Const (list "t"))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'Const (list "e"))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "o"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 7.0))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Number (list 6.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 2.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "e"))))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "y"))
                   (create-ast 'Number (list 2.0))))
                  (create-ast 'Const (list "a"))))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "j"))))
                (create-ast 'Const (list "l"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "c"))))
                (create-ast 'Number (list 4.0))))))))))))
           (create-ast 'AddExp (list
            (create-ast 'AddExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "q"))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "s"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 3.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "u"))
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 8.0))
                    (create-ast 'Const (list "d"))))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "a"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 1.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Const (list "m"))))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "a"))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "d"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 1.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "t"))
                   (create-ast 'Const (list "j"))))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 7.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'Number (list 3.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Number (list 1.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "r"))))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "f"))
                   (create-ast 'Const (list "t"))))))))
                (create-ast 'Number (list 2.0))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "p"))
                  (create-ast 'Const (list "k"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "y"))))
                (create-ast 'Const (list "t"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Const (list "d"))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "s"))))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "n"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Number (list 6.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "m"))
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 4.0))
                    (create-ast 'Number (list 5.0))))))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Const (list "a"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'Const (list "r"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "e"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 4.0))))))))))))))))))
          (create-ast 'AddExp (list
           (create-ast 'AddExp (list
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'Const (list "l"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'AddExp (list
                    (create-ast 'Number (list 8.0))
                    (create-ast 'Number (list 4.0))))
                   (create-ast 'Const (list "q"))))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'Const (list "k"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "s"))))))
                (create-ast 'Const (list "f"))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'Number (list 1.0))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "r"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "o"))
                   (create-ast 'Const (list "a"))))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Const (list "t"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Number (list 6.0))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "g"))
                    (create-ast 'Number (list 6.0))))))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "g"))
                   (create-ast 'Const (list "t"))))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'Const (list "d"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "f"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Number (list 4.0))))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "i"))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Const (list "j"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'AddExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 1.0))
                    (create-ast 'Const (list "p"))))
                   (create-ast 'Number (list 5.0))))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 7.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "s"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Const (list "c"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 6.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'Const (list "w"))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 2.0))
                (create-ast 'Const (list "f"))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'Number (list 7.0))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Number (list 2.0))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Const (list "n"))))))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "p"))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "v"))
                   (create-ast 'Const (list "p"))))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Number (list 2.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "d"))
                   (create-ast 'Number (list 6.0))))
                  (create-ast 'Const (list "g"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 5.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "t"))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "a"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "f"))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "x"))
                    (create-ast 'Number (list 8.0))))))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "v"))
                 (create-ast 'Number (list 5.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "y"))
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "b"))
                   (create-ast 'Const (list "a"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "i"))))))
                 (create-ast 'Const (list "q"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "s"))))))
                 (create-ast 'Const (list "w"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "d"))
                  (create-ast 'Const (list "w"))))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "w"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 8.0))))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Const (list "b"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "t"))
                 (create-ast 'Number (list 6.0))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'Const (list "o"))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "n"))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "a"))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Number (list 5.0))))))))))))))))))
         (create-ast 'MulExp (list
          (create-ast 'AddExp (list
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Const (list "u"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "k"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 4.0))))))
                 (create-ast 'Number (list 3.0))))))
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "h"))
                   (create-ast 'Number (list 8.0))))
                  (create-ast 'Const (list "r"))))
                 (create-ast 'Const (list "u"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 7.0))
                   (create-ast 'Const (list "e"))))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "h"))
                (create-ast 'Number (list 4.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 5.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "a"))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Number (list 5.0))))
                 (create-ast 'Const (list "k"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Number (list 4.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "n"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Number (list 4.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Const (list "p"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "x"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 1.0))
                   (create-ast 'Number (list 6.0))))))
                 (create-ast 'Number (list 3.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'Const (list "z"))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "x"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 2.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "e"))
                 (create-ast 'Number (list 2.0))))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 7.0))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "p"))))
                 (create-ast 'Number (list 9.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'Number (list 5.0))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "s"))
                   (create-ast 'Number (list 3.0))))
                  (create-ast 'Number (list 1.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 4.0))))
                 (create-ast 'Number (list 2.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Number (list 4.0))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Number (list 4.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "u"))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "c"))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "d"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "w"))
                  (create-ast 'Const (list "z"))))))
                (create-ast 'Const (list "l"))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "s"))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'Const (list "f"))))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Const (list "w"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "y"))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'Const (list "r"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "t"))
                  (create-ast 'Const (list "z"))))
                 (create-ast 'Number (list 8.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "s"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 5.0))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "s"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 5.0))
                   (create-ast 'MulExp (list
                    (create-ast 'Number (list 4.0))
                    (create-ast 'Number (list 6.0))))))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Const (list "x"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "q"))
                  (create-ast 'Number (list 3.0))))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "r"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'Const (list "h"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 7.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "e"))))
                (create-ast 'Number (list 9.0))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 9.0))
                (create-ast 'Const (list "k"))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "o"))))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Const (list "f"))))
                (create-ast 'Const (list "z"))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 2.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'Number (list 1.0))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "x"))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "b"))
                  (create-ast 'Const (list "r"))))))
                (create-ast 'Const (list "g"))))))))))))))
          (create-ast 'MulExp (list
           (create-ast 'MulExp (list
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 6.0))
                 (create-ast 'Const (list "p"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 9.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 8.0))
                  (create-ast 'Const (list "d"))))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "q"))
                   (create-ast 'Const (list "n"))))
                  (create-ast 'Const (list "y"))))
                 (create-ast 'Const (list "h"))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "n"))
                 (create-ast 'Const (list "p"))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 2.0))
                  (create-ast 'Number (list 5.0))))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "f"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Number (list 3.0))))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "c"))
                  (create-ast 'Number (list 2.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'Number (list 2.0))))))))))))
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "r"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Const (list "b"))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 7.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 7.0))
                  (create-ast 'Number (list 6.0))))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "l"))
                  (create-ast 'Const (list "i"))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 9.0))
                  (create-ast 'Number (list 8.0))))
                 (create-ast 'Const (list "j"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "l"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "j"))))))))
               (create-ast 'AddExp (list
                (create-ast 'Number (list 3.0))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 9.0))))))))))))
            (create-ast 'MulExp (list
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 3.0))
                  (create-ast 'Const (list "c"))))
                 (create-ast 'Number (list 2.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Number (list 8.0))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "f"))
                  (create-ast 'Const (list "v"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 3.0))
                   (create-ast 'Number (list 4.0))))))))))))
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "u"))
                 (create-ast 'Number (list 6.0))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Number (list 6.0))
                (create-ast 'Number (list 2.0))))))))
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "e"))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "g"))
                  (create-ast 'Const (list "h"))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "j"))
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "m"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 4.0))
                   (create-ast 'Const (list "o"))))))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "q"))
                   (create-ast 'Number (list 5.0))))))))))))
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'Const (list "b"))
                (create-ast 'Const (list "k"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 6.0))
                   (create-ast 'Number (list 8.0))))))
                 (create-ast 'Const (list "x"))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "m"))
                 (create-ast 'Number (list 1.0))))))))))))))
           (create-ast 'MulExp (list
            (create-ast 'MulExp (list
             (create-ast 'MulExp (list
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "v"))
                  (create-ast 'Const (list "w"))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 3.0))
                 (create-ast 'Number (list 8.0))))))
               (create-ast 'MulExp (list
                (create-ast 'Const (list "y"))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "q"))
                 (create-ast 'Const (list "d"))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "j"))
                  (create-ast 'Const (list "b"))))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 2.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 4.0))
                 (create-ast 'Number (list 1.0))))))
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "h"))
                  (create-ast 'Number (list 3.0))))
                 (create-ast 'AddExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Number (list 4.0))))
                  (create-ast 'Const (list "p"))))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 5.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'Const (list "h"))))))))))))
             (create-ast 'MulExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "h"))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'Const (list "f"))))
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "i"))
                  (create-ast 'Const (list "e"))))
                 (create-ast 'Const (list "r"))))
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Number (list 7.0))))
                 (create-ast 'Number (list 9.0))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'Number (list 8.0))
                (create-ast 'Const (list "c"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Number (list 3.0))))
                (create-ast 'AddExp (list
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 6.0))
                  (create-ast 'Const (list "o"))))
                 (create-ast 'Const (list "o"))))))))))))
            (create-ast 'AddExp (list
             (create-ast 'AddExp (list
              (create-ast 'MulExp (list
               (create-ast 'MulExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 8.0))
                 (create-ast 'Const (list "g"))))
                (create-ast 'Const (list "q"))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'AddExp (list
                  (create-ast 'Const (list "o"))
                  (create-ast 'AddExp (list
                   (create-ast 'Number (list 8.0))
                   (create-ast 'Const (list "z"))))))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "s"))
                 (create-ast 'AddExp (list
                  (create-ast 'Number (list 4.0))
                  (create-ast 'Const (list "x"))))))))))
              (create-ast 'AddExp (list
               (create-ast 'MulExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "j"))
                 (create-ast 'MulExp (list
                  (create-ast 'AddExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Const (list "x"))))
                  (create-ast 'Number (list 9.0))))))
                (create-ast 'MulExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "u"))
                  (create-ast 'Const (list "j"))))
                 (create-ast 'Number (list 7.0))))))
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 1.0))
                 (create-ast 'Const (list "x"))))
                (create-ast 'Const (list "b"))))))))
             (create-ast 'AddExp (list
              (create-ast 'AddExp (list
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "g"))
                 (create-ast 'MulExp (list
                  (create-ast 'Const (list "e"))
                  (create-ast 'Const (list "b"))))))
                (create-ast 'AddExp (list
                 (create-ast 'Number (list 9.0))
                 (create-ast 'Number (list 6.0))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "c"))
                 (create-ast 'Number (list 9.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'Number (list 8.0))))))))
              (create-ast 'MulExp (list
               (create-ast 'AddExp (list
                (create-ast 'MulExp (list
                 (create-ast 'Number (list 7.0))
                 (create-ast 'MulExp (list
                  (create-ast 'MulExp (list
                   (create-ast 'MulExp (list
                    (create-ast 'Const (list "z"))
                    (create-ast 'Const (list "t"))))
                   (create-ast 'Const (list "i"))))
                  (create-ast 'MulExp (list
                   (create-ast 'Number (list 9.0))
                   (create-ast 'Number (list 6.0))))))))
                (create-ast 'AddExp (list
                 (create-ast 'Const (list "p"))
                 (create-ast 'Const (list "l"))))))
               (create-ast 'AddExp (list
                (create-ast 'AddExp (list
                 (create-ast 'MulExp (list
                  (create-ast 'Number (list 1.0))
                  (create-ast 'MulExp (list
                   (create-ast 'Const (list "i"))
                   (create-ast 'Const (list "a"))))))
                 (create-ast 'Number (list 5.0))))
                (create-ast 'MulExp (list
                 (create-ast 'Const (list "z"))
                 (create-ast 'Number (list 5.0))))))))))))))))))))))))))))))))
