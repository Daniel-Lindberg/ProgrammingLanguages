(define follow
  (lambda (lst x)
    (if (null? lst)
        '()
        (if (eq? (car lst) x)
            (follow (cdr lst) x)
            (cons (car lst) (follow (cdr lst) x))))))

(define followF
  (lambda (lst x accm)
    (if (null? lst)
        accm
        (if (eq? (car lst) x)
            (followF (cdr lst) x accm)
            (followF (cdr lst) x (append accm (list (car lst))))))))


(define follow$
  (lambda (lst x)
    (if (null? lst)
        '()
        (if (eq? (car lst) x)
            (follow$ (cdr lst) x);put extra parenthesis around (cdr lst) to evaluate it if using streams
            (cons (car lst) (lambda() (follow$ (cdr lst) x))))))) ;; same down here

(define int-builder$
  (lambda (x) 
    (cons x (lambda ()
       (int-builder$ (+ x 1))))))

(define take$
    (lambda (m s)
      (if (or (= m 0) (null? s))
          '()
          (cons (car s) (take$ (- m 1) ((cdr s)))))))

(define call/cc call-with-current-continuation)

(define succ
  (lambda ()
    (let ((a (call/cc (lambda (k) (cons 0 k)))))
      (cons (+ (car a) 1) (cdr a)))))

(define first (succ))