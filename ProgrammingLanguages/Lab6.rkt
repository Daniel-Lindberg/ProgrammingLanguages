
(define filter-out-mults$
  (lambda (num strm)
    (if (= (modulo (car strm) num) 0)
        (filter-out-mults$ num ((cdr strm)))
        (cons (car strm) (lambda()(filter-out-mults$ num ((cdr strm))))))))


(define sieve$
    (lambda (strm)
      (if (null? strm)
          '()
          (cons (car strm) (lambda()(sieve$ (filter-out-mults$ (car strm) strm)))))))


 (define stol$
    (lambda (n) 
      (take$ n (sieve$ (int-builder$ 2)))))

  


;;Number counting stream
(define int-builder$
  (lambda (x) 
    (cons x (lambda ()
       (int-builder$ (+ x 1))))))

;;Gets a certain number of returns from a stream
(define take$
    (lambda (m s)
      (if (or (= m 0) (null? s))
          '()
          (cons (car s) (take$ (- m 1) ((cdr s)))))))
    