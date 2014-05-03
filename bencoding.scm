(define-library (bencoding)
  (import (scheme base))
  (export bencoding-parse bencoding-construct
          bencoding-parse-bytevector bencoding-construct-bytevector)
  (begin

    (define-syntax rlet1
      (syntax-rules ()
        ((_ var expr body0 body1 ...)
         (let ((var expr)) body0 body1 ... var))))

    (define (call-with-output-string procedure)
      (let ((ostr (open-output-string)))
        (call-with-port ostr
          (lambda(x)
            (procedure x)
            (get-output-string x)))))

    (define (sort! obj less<) ;; merge sort
      (define temp (make-vector (vector-length obj)))
      (define (inner-sort left right)
        (unless (>= left right)
          (let ((mid (quotient (+ left right) 2)))
            (inner-sort left mid)
            (inner-sort (+ mid 1) right)
            (vector-copy! temp left obj left (+ mid 1))
            (do ((i (+ mid 1) (+ i 1))
                 (j right (- j 1)))
                ((> i right))
              (vector-set! temp i (vector-ref obj j)))
            (let ((i left) (j right))
              (do ((k left (+ k 1)))
                  ((> k right))
                (if (less< (vector-ref temp i) (vector-ref temp j))
                    (begin (vector-set! obj k (vector-ref temp i))
                           (set! i (+ i 1)))
                    (begin (vector-set! obj k (vector-ref temp j))
                           (set! j (- j 1)))))))))
      (inner-sort 0 (- (vector-length obj) 1))
      obj)

    (define (bencoding-parse-string first-ch port)
      (let ((size
             (string->number
              (call-with-output-string
                (lambda(out)
                  (write-char (integer->char first-ch) out)
                  (unless
                      (eqv? 58 (do ((ch (read-u8 port) (read-u8 port)))
                                   ((not (<= 48 ch 57)) ch)
                                 (write-char (integer->char ch) out)))
                    (error "Invalid bencoding string in bencoding-parse")))))))
        (read-bytevector size port)))

    (define (bencoding-parse-integer port)
      (string->number
       (call-with-output-string
         (lambda(out)
           (let ((ch (read-u8 port)))
             (if (eqv? 45 ch)
                 (begin
                   (write-char (integer->char ch) out)
                   (let ((ch (read-u8 port)))
                     (if (<= 49 ch 57)
                         (write-char (integer->char ch) out)
                         (error "Invalid bencoding integer in bencoding-parse"))))
                 (if (<= 49 ch 57)
                     (write-char (integer->char ch) out)
                     (error "Invalid bencoding integer in bencoding-parse")))
             (unless
                 (eqv? 101 (do ((ch (read-u8 port) (read-u8 port)))
                               ((not (<= 48 ch 57)) ch)
                             (write-char (integer->char ch) out)))
               (error "Invalid bencoding integer in bencoding-parse")))))))

    (define (bencoding-parse-dictionary result port depth)
      (let ((key (bencoding-parse-object port depth)))
        (if (eof-object? key)
            (list->vector (reverse result))
            (begin
              (when (not (bytevector? key))
                (error "Invalid bencoding dictionary in bencoding-parse"))
              (let ((value (bencoding-parse-object port depth)))
                (bencoding-parse-dictionary
                 (cons (cons (utf8->string key) value) result)
                 port
                 depth))))))

    (define (bencoding-parse-list result port depth)
      (let ((elm (bencoding-parse-object port depth)))
        (if (eof-object? elm)
            (reverse result)
            (bencoding-parse-list (cons elm result) port depth))))

    (define (bencoding-parse-object port depth)
      (let ((ch (read-u8 port)))
        (cond
         ((eof-object? ch) '())
         ((eqv? ch 100) (bencoding-parse-dictionary '() port (+ 1 depth)))
         ((eqv? ch 101) (if (zero? depth)
                            (error "Invalid bencoding in bencoding-parse")
                            (eof-object)))
         ((eqv? ch 105) (bencoding-parse-integer port))
         ((eqv? ch 108) (bencoding-parse-list '() port (+ 1 depth)))
         ((<= 48 ch 57) (bencoding-parse-string ch port))
         (error "Invalid bencoding type in bencoding-parse" ch))))

    (define (bencoding-parse port)
      (bencoding-parse-object port 0))

    (define (bytevector< x y)
      (let* ((x-size (bytevector-length x))
             (y-size (bytevector-length y))
             (limit (if (< x-size y-size) x-size y-size))
             (ind (do ((i 0 (+ i 1)))
                      ((or (>= i limit)
                           (not (= (bytevector-u8-ref x i)
                                   (bytevector-u8-ref y i))))
                       i))))
        (if (= ind limit)
            (< x-size y-size)
            (< (bytevector-u8-ref x ind)
               (bytevector-u8-ref y ind)))))

    (define (key< x y)
      (let* ((x1 (car x))
             (y1 (car y))
             (x2 (if (string? x1) (string->utf8 x1) x1))
             (y2 (if (string? y1) (string->utf8 y1) y1)))
        (bytevector< x2 y2)))

    (define (bencoding-construct-dictionary obj port)
      (write-u8 100 port)
      (vector-for-each (lambda(x)
                         (bencoding-construct (car x) port)
                         (bencoding-construct (cdr x) port))
                       (sort! (vector-copy obj) key<))
      (write-u8 101 port))

    (define (bencoding-construct-list obj port)
      (write-u8 108 port)
      (for-each (lambda(x) (bencoding-construct x port)) obj)
      (write-u8 101 port))

    (define (bencoding-construct-integer obj port)
      (write-u8 105 port)
      (write-bytevector (string->utf8 (number->string obj)) port)
      (write-u8 101 port))

    (define (bencoding-construct-string obj port)
      (write-bytevector
       (string->utf8 (number->string (bytevector-length obj)))
       port)
      (write-u8 58 port)
      (write-bytevector obj port))

    (define (bencoding-construct obj port)
      (cond
       ((vector? obj) (bencoding-construct-dictionary obj port))
       ((list? obj) (bencoding-construct-list obj port))
       ((integer? obj) (bencoding-construct-integer obj port))
       ((string? obj) (bencoding-construct-string (string->utf8 obj) port))
       ((bytevector? obj) (bencoding-construct-string obj port))
       (else (error "Attempt invalid object convert to bencoding" obj))))

    (define (bencoding-construct-bytevector obj)
      (call-with-port (open-output-bytevector)
        (lambda(port)
          (bencoding-construct obj port)
          (get-output-bytevector port))))

    (define (bencoding-parse-bytevector bv)
      (call-with-port (open-input-bytevector bv)
        bencoding-parse))

    ))
