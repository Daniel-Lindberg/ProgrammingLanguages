(define escape
      (lambda ()
         (set! halt (call/cc (lambda (k) k)))
         0))


;Tail Recursion implementation of multiply (HINT BROWNIES)
(define multiply
  (lambda (l accm)
    (if (null? l)
        (* 1 accm)
        (if (zero? (car l))
            (halt halt)
            (multiply (cdr l) (* (car l) accm))))))

(define tester
  (lambda () 
    (let ((result (call/cc (lambda (k) (set! halt k) '())))) 
      (define multiply
        (lambda (l accm)
          (if (null? l)
              (* 1 accm)
              (if (zero? (car l))
                  (halt 0)
                  (multiply (cdr l) (* (car l) accm)))))))))

(define int-builder$
  (lambda (x) 
    (cons x (lambda ()
       (int-builder$ (+ x 1))))))