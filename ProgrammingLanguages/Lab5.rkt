
(define int-builder 
  (lambda (x) 
    (if (= x 1)
        '()
        (append (int-builder (- x 1)) (list x)))))

(define filter-out-mults
  (lambda (num 1st)
    (if (null? 1st)
        '()
        (if (= (modulo (car 1st) num) 0)
            (filter-out-mults num (cdr 1st))
            (cons (car 1st) (filter-out-mults num (cdr 1st)))))))

(define sieve
    (lambda (lst)
      (if (null? lst)
          '()
          (cons (car lst) (sieve (filter-out-mults (car lst) (cdr lst)))))))

(define primes (lambda (n) (sieve (int-builder n))))

(define stol
    (lambda (m)
      (let ((lst (primes (* m 20))))
         (take m lst))))

(define take 
    (lambda (m lst)
      (if (= m 0)
	'()
	(cons (car lst) (take (- m 1) (cdr lst))))))
;; Above is lab5