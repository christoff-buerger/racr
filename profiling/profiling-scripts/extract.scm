; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (profiling-scripts extract)
 (export filter-tables ps:< ps:> ps:<= ps:>= ps:== ps:!= ps:MIN ps:MAX ps:min ps:max)
 (import (rnrs) (rnrs mutable-pairs))

 ; Find a string within a list of strings. Return its index if found and unique, otherwise #f.
 (define (find-index s l)
   (let loop ((l l) (i 0) (index -1))
     (cond
       ((null? l)
        (and (>= index 0) index))
       ((string=? (car l) s)
        (if (>= index 0) #f (loop (cdr l) (+ i 1) i)))
       (else
        (loop (cdr l) (+ i 1) index)))))
 
 ; Apply a function on a port. The port is closed when terminating.
 (define (process-stream port f)
   (dynamic-wind
    (lambda () #f)
    (lambda () (f port))
    (lambda () (close-port port))))

 ; Abort with an exception due to some error.
 (define (raise-error . rationales)
   (raise
    (condition
     (make-error)
     (make-message-condition
      (apply
       string-append
       "Extraction failed:"
       (map
        (lambda (rationale)
          (string-append "\n  " rationale))
        rationales))))))
 
 ; Abort with an exception due to an extractor configuration error.
 (define (raise-extractor-configuration-error . rationales)
   (apply raise-error "The given extractor configuration is malformed." rationales))

 ; Abort with an exception due to a header configuration error.
 (define (raise-header-configuration-error . rationales)
   (apply raise-error "The given header configuration is malformed." rationales))

 ; Comparator functions for value comparison and extrema checking:
 (define (ps:< s1 s2)
   (string<? s1 s2))
 (define (ps:> s1 s2)
   (string>? s1 s2))
 (define (ps:<= s1 s2)
   (string<=? s1 s2))
 (define (ps:>= s1 s2)
   (string>=? s1 s2))
 (define (ps:== s1 s2)
   (string=? s1 s2))
 (define (ps:!= s1 s2)
   (not (ps:== s1 s2)))
 (define (ps:MIN s1 s2)
   (string<? s1 s2))
 (define (ps:MAX s1 s2)
   (string>? s1 s2))
 (define (ps:min s1 s2)
   (string<? s1 s2))
 (define (ps:max s1 s2)
   (string>? s1 s2))
 
 ; Is a given extraction operator a value comparator?
 (define (value-comparator? e)
   (memq e (list ps:!= ps:== ps:< ps:> ps:<= ps:>=)))

 ; Is a given extraction operator a row independent extrema?
 (define (row-independent-extrema? e)
   (memq e (list ps:MIN ps:MAX)))

 ; Is a given extraction operator a row specific extrema?
 (define (row-specific-extrema? e)
   (memq e (list ps:min ps:max)))
 
 ; Is a given extraction operator a row independent or specific extrema?
 (define (extrema? e)
   (or (row-independent-extrema? e) (row-specific-extrema? e)))
 
 (define cell-size ; Length of columns.
   22)

 (define cell-delimeter ; Character separating adjacent cells of columns.
   #\|)

 (define min-cell-value ; Lexically smallest value a cell can have.
   (make-string cell-size #\x0))
 
 (define max-cell-value ; Lexically biggest value a cell can have.
   (make-string cell-size #\xFFFFF))

 (define header-separator-cell ; Content of cells separating table headers from actual data rows.
   (make-string cell-size #\-))

 ; Construct a cell-content string matching the cell size of tables (#f if to big cell).
 (define (make-padded-cell cell)
   (define padding (- cell-size 1 (string-length cell)))
   (and
    (>= padding 0)
    (string-append (make-string padding #\space) cell " ")))
 
 ; Construct a header-string matching the cell size of tables (#f if to big header).
 (define (make-padded-header header)
   (define padding (- cell-size 1 (string-length header)))
   (and
    (>= padding 0)
    (string-append " " header (make-string padding #\space))))
 
 ; Value comparator extractor that accepts any input.
 (define neutral-comparator-checker
   (lambda _ #t))

 ; Given a list of extractors, construct a function checking if a cell satisfies the
 ; value comparators of the given list. Aborts with an exception in case of invalid extractors.
 ; Returns the neutral value comparator checker if the list contains no value comparators.
 (define (make-column-comparator-checker extractors)
   (define (make-column-comparator-checker extractors pending-comparator? continuation)
     (if (null? extractors)
         (if pending-comparator?
             (raise-extractor-configuration-error
              "Orphan comparision value missing value comparator:" pending-comparator?)
             continuation)
         (let ((elem (car extractors))
               (rest (cdr extractors)))
           (cond
             (pending-comparator?
              (when (not (value-comparator? elem))
                (raise-extractor-configuration-error
                 "Orphan comparision value missing value comparator:" pending-comparator?))
              (make-column-comparator-checker
               rest
               #f
               (if (eq? continuation neutral-comparator-checker)
                   (lambda (cell-value) ; First value comparator of extractors defines whole...
                     (elem cell-value pending-comparator?)) ; ...checker for now,...
                   (lambda (cell-value) ; ...until further value comparators...
                     (and ; ...also...
                      (elem cell-value pending-comparator?) ; ...have to be checked...
                      (continuation cell-value)))))) ; ...and so on.
             ((string? elem)
              (let ((comparison-value (make-padded-cell elem)))
                (unless comparison-value
                  (raise-extractor-configuration-error "To big comparison value."))
                (make-column-comparator-checker rest comparison-value continuation)))
             (else
              (when (value-comparator? elem)
                (raise-extractor-configuration-error
                 "Orphan value comparator missing comparison value."))
              (unless (extrema? elem)
                (raise-extractor-configuration-error "Unknown extractor."))
              (make-column-comparator-checker rest #f continuation))))))
   (make-column-comparator-checker (reverse extractors) #f neutral-comparator-checker))

 ; Extrema extractor that discards any input (treats it as if no extrema).
 (define neutral-extrema-checker
   (lambda _ 0))

 ; Emit the data-cells of source tables considering row-extraction operators. Only data-rows
 ; selected by the given extraction operators will be printed cell-wise on the standard output,
 ; each cell terminated by a new line. Source tables are checked for structural integrity
 ; throughout processing. An exception is thrown in case of errors.
 (define (filter-tables headers extractors . source-files)
   ;;; Initialize row-size and -structure metrics:
   (define column-count ; Number of columns per row.
     (length headers))
   (define date-column ; Column number of measurement dates.
     (find-index "Date" headers))
   (define status-column ; Column number of measurement statuses.
     (find-index "Status" headers))
   (define header-title ; Column title string (first line of tables).
     ; Abort with exception in case of header configuration errors.
     (if (and date-column status-column (= date-column 0))
         (fold-left
          (lambda (result header)
            (define padded-header (make-padded-header header))
            (unless padded-header
              (raise-header-configuration-error "To big header cell."))
            (if result
                (string-append result (string cell-delimeter) padded-header)
                padded-header))
          #f
          headers)
         (raise-header-configuration-error "Date or Status column missing.")))
   (define header-separator ; Colum title separator string (second line of tables).
     (do ((i 1 (+ i 1))
          (ruler header-separator-cell (string-append ruler "+" header-separator-cell)))
       ((>= i column-count) ruler)))

   ;;; Initialize table-size and -processing metrics:
   (define row-count ; Total number of rows considering all source tables.
     (fold-left
      (lambda (counter file)
        (+
         counter
         (process-stream
          (open-input-file file)
          (lambda (port)
            (do ((i 0 (+ i 1)))
              ((eof-object? (get-line port)) i))))))
      0
      source-files))
   (define rows ; Vector of table-rows. Each row is a vector of its cells or '#f' when unselected.
     (make-vector row-count #f))
   (define processed ; Total number of table-rows (header and data) processed so far.
     0)
   
   ;;; Configure row pool allocator for fast row allocation:
   (define row-length ; Size of row vectors (number of columns + extrema counter).
     (+ column-count 1))
   (define row-pool-size ; Size of row pool allocator.
     (if (>= row-count 3)
         (if (< (* row-count row-length) 65535)
             (floor (/ row-count 3)) ; About 1/3 of rows to process...
             (floor (/ 65535 row-length))) ; ...but maximal (2^16)-1 (-1: consider vector-tagging).
         1)) ; Ensure there is at least a single row allocated.
   (define row-pool ; Row pool allocator: list of unused, but already allocated, rows.
     (make-vector row-pool-size #f))
   (define row-pool-next-free ; Index of next available unused row in pool.
     row-pool-size)
   ; Get a new row, preferred without reallocation.
   (define make-row
     (let ((reallocation-start (floor (/ (* 2 row-pool-size) 3))))
       (lambda ()
         (when (= row-pool-next-free row-pool-size) ; If the pool is exhausted...
           (do ((i reallocation-start (+ i 1))) ; ...allocate new rows within last third of pool.
             ((>= i row-pool-size))
             (vector-set! row-pool i (make-vector row-length)))
           (set! row-pool-next-free reallocation-start))
         (let ((row (vector-ref row-pool row-pool-next-free)))
           (set! row-pool-next-free (+ row-pool-next-free 1))
           row))))
   ; Add row to the pool of unused rows so it can be reused by later allocations.
   (define (free-row row)
     (when (> row-pool-next-free 0)
       (do ((i 0 (+ i 1))) ((>= i column-count))
         (vector-set! row i #f))
       (set! row-pool-next-free (- row-pool-next-free 1))
       (vector-set! row-pool row-pool-next-free row)))

   ; Reduce the number of extrema a row satisfies. Discard and free the row if no extrema is left.
   (define (discard-extremum row-index)
     (define row (vector-ref rows row-index))
     (define new-extrema-count (- (vector-ref row column-count) 1))
     (if (= new-extrema-count 0)
         (begin (vector-set! rows row-index #f) (free-row row))
         (vector-set! row column-count new-extrema-count)))

   ;;; Value comparators processing:
   ; Satisfies a given row all value comparators defined for all its columns?
   ; Abort with exception in case of extractor configuration errors.
   (define value-comparator-checker
     (if (= (length extractors) column-count)
         (let loop ((i (- (length extractors) 1))
                    (continuation neutral-comparator-checker)
                    (extractors (reverse extractors)))
           (if (null? extractors)
               continuation
               (let ((column-checker (make-column-comparator-checker (car extractors))))
                 (loop
                  (- i 1)
                  (cond
                    ((eq? column-checker neutral-comparator-checker) ; Column has no comparators,...
                     continuation) ; ...thus do nothing for it.
                    ((eq? continuation neutral-comparator-checker) ; First comparators,...
                     (lambda (row) ; ...define whole extractor (replace neutral checker) for now...
                       (column-checker (vector-ref row i))))
                    (else ; ...until further columns with comparators...
                     (lambda (row)
                       (and ; ...also...
                        (column-checker (vector-ref row i)) ; ...have to be checked,...
                        (continuation row))))) ; ...and so on.
                  (cdr extractors)))))
         (raise-extractor-configuration-error
          "Wrong number of extractor configurations."
          "Expected one extractor configuration per column.")))

   ;;; Row independent extrema processing:
   ; Check if a certain cell of a row is a certain row independent extrema (updates extrema caches).
   (define (check-XXX-extrema row column ps:XXX XXX-values XXX-indices)
     (define extrema-value (vector-ref XXX-values column))
     (define extrema-indices (vector-ref XXX-indices column))
     (define cell (vector-ref row column))
     (cond
       ((ps:== cell extrema-value)
        (vector-set! XXX-indices column (cons processed extrema-indices))
        1)
       ((ps:XXX cell extrema-value)
        (for-each discard-extremum extrema-indices)
        (vector-set! XXX-values column cell)
        (vector-set! XXX-indices column (list processed))
        1)
       (else
        0)))
   ; Check if a certain cell of a row is a row independent minimum (updates extrema caches).
   (define check-MIN-extrema
     (let ((MIN-values (make-vector column-count max-cell-value))
           (MIN-indices (make-vector column-count (list))))
       (lambda (row column)
         (check-XXX-extrema row column ps:MIN MIN-values MIN-indices))))
   ; Check if a certain cell of a row is a row independent maximum (updates extrema caches).
   (define check-MAX-extrema
     (let ((MAX-values (make-vector column-count min-cell-value))
           (MAX-indices (make-vector column-count (list))))
       (lambda (row column)
         (check-XXX-extrema row column ps:MAX MAX-values MAX-indices))))

   ;;; Row specific extrema processing:
   ; Check if a certain cell of a row is a certain row specific extrema (updates extrema caches).
   (define (check-xxx-extrema row column ps:xxx row-key->xxx-value/indices)
     (define cell (vector-ref row column))
     (define result 1)
     (hashtable-update!
      row-key->xxx-value/indices
      (let ((row-key (vector-map (lambda (e) e) row)))
        (vector-set! row-key date-column #f) ; Ignore the date column.
        (vector-set! row-key column #f) ; Ignore the column checked for a row specific extrema.
        (vector-set! row-key column-count #f) ; Ignore the row's extrema count.
        row-key)
      (lambda (hashed)
        (if (eq? hashed #f)
            (cons cell (list processed))
            (let ((extrema-value (car hashed))
                  (extrema-indices (cdr hashed)))
              (cond
                ((ps:== cell extrema-value)
                 (set-cdr! hashed (cons processed extrema-indices))
                 hashed)
                ((ps:xxx cell extrema-value)
                 (for-each discard-extremum extrema-indices)
                 (cons cell (list processed)))
                (else
                 (set! result 0)
                 hashed)))))
      #f)
     result)
   ; Check if a certain cell of a row is a row specific minimum (updates extrema caches).
   (define check-min-extrema
     (let ((min-value/indices (make-vector column-count)))
       (do ((i (- column-count 1) (- i 1))) ((< i 0))
         (vector-set! min-value/indices i (make-hashtable equal-hash equal?)))
       (lambda (row column)
         (check-xxx-extrema row column ps:min (vector-ref min-value/indices column)))))
   ; Check if a certain cell of a row is a row specific maximum (updates extrema caches).
   (define check-max-extrema
     (let ((max-value/indices (make-vector column-count)))
       (do ((i (- column-count 1) (- i 1))) ((< i 0))
         (vector-set! max-value/indices i (make-hashtable equal-hash equal?)))
       (lambda (row column)
         (check-xxx-extrema row column ps:max (vector-ref max-value/indices column)))))

   ; Check a given row for row independent and specific extrema, return how many the row has and
   ; update the respective extrema caches accordingly (if the row introduces new extrema or
   ; satisfies existing). Respective updates also discard rows with outdated extrema as required.
   (define row-extrema-checker
     (let ((ps:???/checker-list
            (list
             (cons ps:MIN check-MIN-extrema)
             (cons ps:MAX check-MAX-extrema)
             (cons ps:min check-min-extrema)
             (cons ps:max check-max-extrema))))
       (let loop ((i 0)
                  (continuation neutral-extrema-checker)
                  (extractors extractors))
         (if (null? extractors)
             continuation
             (let ((column-extrema-checker
                    (let loop ((continuation continuation)
                               (ps:???/checker-list ps:???/checker-list))
                      (if (null? ps:???/checker-list)
                          continuation
                          (loop
                           (if (memq (car (car ps:???/checker-list)) (car extractors))
                               (let ((checker (cdr (car ps:???/checker-list))))
                                 (lambda (row)
                                   (+ (checker row i)
                                      (continuation row))))
                               continuation)
                           (cdr ps:???/checker-list))))))
               (loop (+ i 1) column-extrema-checker (cdr extractors)))))))
   
   ;;; Row processing:
   ; Check given row if it should be kept or discarded and incorporate it accordingly, including
   ; any required maintenance of the given and previously processed rows (like updating extrema
   ; caches, discarding outdated rows, setting the given row's extrema counter etc).
   (define process-row
     (let ((no-extrema-extractors? (not (find (lambda (l) (memp extrema? l)) extractors))))
       (lambda (row)
         (define keep-row?
           (and
            (value-comparator-checker row)
            (or no-extrema-extractors?
                (let ((extrema (row-extrema-checker row)))
                  (and (> extrema 0) extrema)))))
         (if keep-row?
             (begin
               (vector-set! row column-count keep-row?)
               (vector-set! rows processed row))
             (free-row row))
         (set! processed (+ processed 1)))))

   ;;; Read in source tables. While doing so, check their structure and apply extraction operators:
   (fold-left ; Process each source table and track...
    (lambda (previously-processed source-file) ; ...number of lines proceeding the current table.
      ; Abort with an exception due to a source table error.
      (define (raise-table-error . rationales)
        (apply
         raise-error
         (string-append
          "Source table ["
          source-file
          "] is malformed on line ["
          (number->string (+ (- processed previously-processed) 1))
          "].")
         rationales))
      
      ;;; Open input stream for the current source table and process it:
      (process-stream
       (open-input-file source-file)
       (lambda (port)
         ;;; Process table header (check existence and structure):
         (when (or (port-eof? port) (not (string=? (get-line port) header-title)))
           (raise-table-error "Malformed header."))
         (set! processed (+ processed 1))
         (when (or (port-eof? port) (not (string=? (get-line port) header-separator)))
           (raise-table-error "Malformed header separator."))
         (set! processed (+ processed 1))
         
         ;;; Process table rows (check structure, apply extraction operators and store extracted):
         (let loop ((row-position 0) (row (make-row)))
           (cond
             ((port-eof? port)
              (unless (= row-position 0)
                (raise-table-error "Preemtive end of table.")))
             ((= row-position column-count)
              (unless (char=? (read-char port) #\newline)
                (raise-table-error "Overlong table row."))
              (process-row row)
              (loop 0 (make-row)))
             (else
              (let* ((cell (get-string-n port cell-size))
                     (cell-length (string-length cell)))
                (when ; Check cell structure, i.e., ensure...
                    (or
                     (not (= cell-length cell-size)) ; ...a complete cell was read,...
                     (do ((cell-position 0 (+ cell-position 1)))
                       ((or
                         (= cell-position cell-size)
                         (let ((char (string-ref cell cell-position)))
                           (or
                            (char=? char #\tab) ; ...free of tabulators and...
                            (char=? char #\newline) ; ...unexpected row and...
                            (char=? char cell-delimeter)))) ; ...cell delimeters...
                        (< cell-position cell-size))) ; ...within its content.
                     (and ; Also ensure, that it is followed by the cell-delimeter if...
                      (< row-position (- column-count 1)) ; ...it is not the last cell of the row.
                      (not (char=? (read-char port) cell-delimeter))))
                  (raise-table-error
                   (string-append
                    "Malformed cell (cell number "
                    (number->string (+ row-position 1))
                    ").")))
                (vector-set! row row-position cell)
                (loop (+ row-position 1) row)))))))
      processed)
    0
    source-files)
   
   ;;; Sort all rows and mark duplicates:
   (vector-sort!
    (lambda (row-1 row-2) ; Return #t if, and only if, row-1 has to be moved before row-2.
      (cond
        ((not row-1)
         #f)
        ((not row-2)
         #t)
        (else
         (let ((date-1 (vector-ref row-1 0))
               (date-2 (vector-ref row-2 0)))
           (or
            (ps:< date-1 date-2)
            (and
             (ps:== date-1 date-2)
             (let loop ((i 1))
               (cond
                 ((>= i column-count)
                  (vector-set! row-1 column-count #f) ; Mark as duplicate.
                  #f)
                 ((ps:< (vector-ref row-1 i) (vector-ref row-2 i))
                  #t)
                 ((ps:> (vector-ref row-1 i) (vector-ref row-2 i))
                  #f)
                 (else (loop (+ i 1)))))))))))
    rows)
   
   ;;; Emit extracted rows to the standard output, each cell on a new line (stream of cells):
   (process-stream
    (current-output-port)
    (lambda (port)
      (vector-for-each
       (lambda (row)
         (when (and row (vector-ref row column-count))
           (do ((i 0 (+ i 1)))
             ((= i column-count))
             (put-string port (vector-ref row i))
             (newline port))))
       rows)))))
