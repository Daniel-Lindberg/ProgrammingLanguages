(define escape
      (lambda ()
         (set! halt (call/cc (lambda (k) k)))
         0))

(define call/cc call-with-current-continuation)

(define halt (call/cc (lambda (k) k)))

(define multiply
  (lambda (l)
    (let ((halt (call/cc (lambda (k) (set! halt k) k))))
          (if (procedure? halt)
              (if (null? l)
                  (halt 1)
                  (if (zero? (car l))
                      (halt 0)
                      (* (car l) (multiply (cdr l)))))
              halt))))

;(define tester
 ; (lambda () 
  ;  (let ((result (call/cc (lambda (k) (set! halt k) '())))) 
   ;   (define multiply
    ;    (lambda (l accm)
     ;     (if (null? l)
      ;        (* 1 accm)
       ;       (if (zero? (car l))
        ;          (halt 0)
         ;         (multiply (cdr l) (* (car l) accm)))))))))

;;part one



(define pl
  (lambda (l)
    (let ((halt (call/cc (lambda (k) (set! halt k) k))))
      (if (procedure? halt)
          (if (null? l)
              (halt l)
              (begin (display (car l)) (pl (cdr l)))))
      )))

(define main
  (lambda lst
    (map (lambda (x) (create pl x)) lst)
    (demand-tokens)))

(define demand-tokens
  (lambda ()
    (let ((temp (demand)))
         (if (null? temp) '() (cons temp (demand-tokens))))))

(define create 
  (lambda (func arg)
    (let ((consume (call/cc (lambda (k) (set! consume k) k))))
      (add-to-round-robin (consume))
      (func args))))

(define add-to-round-robin
  (lambda (k)
;;Puts the continuation in list producer, 
;;then calls front of producer which should be consume
    ;;It sounds like Franco wants us to call demand almost
    (demand k)))

;;WARNING below code won't work because producer is not created
;;Don't know if I should create a global variable and be like Producer = '()


(define demand
  (lambda (continuation)
    (cons producer continuation))
     (car producer))

(define step-and-swap
  (lambda (value)
   ((let ((consume (call/cc (lambda (k) (set! consume k) k)))))
      (remove-from-round-robin)
      (append k produce))))

(define remove-from-round-robin
  (lambda (k)
    (cdr proudce)
    (if (null? k)
        ()
        (k))))
        
       
