(define call/cc call-with-current-continuation)

(define cell11 '())  ;; row 1 columns 1-4
(define cell12 '())
(define cell13 '())
(define cell14 '())

(define cell21 '())  ;; row 2 columns 1-4
(define cell22 '())
(define cell23 '())
(define cell24 '())

(define cell31 '())  ;; row 3 columns 1-4
(define cell32 '())
(define cell33 '())
(define cell34 '())

(define cell41 '())  ;; row 4 columns 1-4
(define cell42 '())
(define cell43 '())
(define cell44 '())

;; implementation of amb
(define amb-fail (lambda () (error 'no-solution)))

(define-syntax amb
  (syntax-rules ()
    ((amb alt ...)
     (let ((prev-amb-fail amb-fail))
       (call/cc 
         (lambda (sk)
           (call/cc 
             (lambda (fk)
               (set! amb-fail (lambda ()
                                (set! amb-fail prev-amb-fail)
                                (fk 'fail)))
               (sk alt)))
            ...
           (prev-amb-fail)))))))

;; forces pred to be true
(define assert (lambda (pred) (if (not pred) (amb))))

;; check whether an element of one given list is a member of any 
;; of the other given lists
(define distinct?
  (lambda (o l)
    (if (null? l)
    #t
    (if (= (car l) o)
            #f
            (distinct? o (cdr l))))))

(define magic-squares
  (lambda ()
    (set! cell11 (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
     (set! cell12 (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
    (assert (distinct? cell12 (list cell11)))
    (set! cell13 (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
    (assert (distinct? cell13 (list cell12 cell11)))
    (assert (< (+ cell11 cell12 cell13) 34))
    (set! cell14 (- 34 (+ cell11 cell12 cell13)))
    (assert (distinct? cell14 (list cell13 cell12 cell11)))
    (set! cell21 (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
    (assert (distinct? cell21 (list cell14 cell13 cell12 cell11)))
    (set! cell31 (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
    (assert (distinct? cell31 (list cell21 cell14 cell13 cell12 cell11)))
    (assert (< (+ cell11 cell21 cell31) 34))
    (set! cell41 (- 34 (+ cell11 cell21 cell31)))
    (assert (distinct? cell41 (list cell31 cell21 cell14 cell13 cell12 cell11)))
    (set! cell22 (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
    (assert (distinct? cell22 (list cell41 cell31 cell21 cell14 cell13 cell12 cell11)))
    (set! cell32 (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
    (assert (distinct? cell32 (list cell22 cell41 cell31 cell21 cell14 cell13 cell12 cell11)))
    (assert (< (+ cell12 cell22 cell32) 34))
    (set! cell42 (- 34 (+ cell12 cell22 cell32)))
    (assert (distinct? cell42 (list cell32 cell22 cell41 cell31 cell21 cell14 cell13 cell12 cell11)))
    (assert (< (+ cell41 cell32 cell14) 34))
    (set! cell23 (- 34 (+ cell41 cell32 cell14)))
    (assert (distinct? cell23 (list cell42 cell32 cell22 cell41 cell31 cell21 cell14 cell13 cell12 cell11)))
    (assert (< (+ cell23 cell32 cell22) 34))
    (set! cell33 (- 34 (+ cell23 cell32 cell22)))
    (assert (distinct? cell33 (list cell23 cell42 cell32 cell22 cell41 cell31 cell21 cell14 cell13 cell12 cell11)))
    (assert (< (+ cell11 cell14 cell41) 34))
    (set! cell44 (- 34 (+ cell11 cell14 cell41)))
    (assert (distinct? cell44 (list cell33 cell23 cell42 cell32 cell22 cell41 cell31 cell21 cell14 cell13 cell12 cell11)))
    (assert (= (+ cell11 cell22 cell33 cell44) 34))
    (assert (< (+ cell21 cell22 cell23) 34))
    (set! cell24 (- 34 (+ cell21 cell22 cell23)))
    (assert (distinct? cell24 (list cell44 cell33 cell23 cell42 cell32 cell22 cell41 cell31 cell21 cell14 cell13 cell12 cell11)))
    (assert (< (+ cell31 cell32 cell33) 34))
    (set! cell34 (- 34 (+ cell31 cell32 cell33)))
    (assert (distinct? cell34 (list cell24 cell44 cell33 cell23 cell42 cell32 cell22 cell41 cell31 cell21 cell14 cell13 cell12 cell11)))
    (assert (= (+ cell14 cell24 cell34 cell44) 34))
    (assert (< (+ cell41 cell42 cell44) 34))
    (set! cell43 (- 34 (+ cell41 cell42 cell44)))
    (assert (distinct? cell43 (list cell34 cell24 cell44 cell33 cell23 cell42 cell32 cell22 cell41 cell31 cell21 cell14 cell13 cell12 cell11)))
    (assert (= (+ cell13 cell23 cell33 cell43) 34))
    (print cell11 cell12 cell13 cell14 cell21 cell22 cell23 cell24 cell31 cell32 cell33 cell34 cell41 cell42 cell43 cell44)))

(define print
  (lambda (cell11 cell12 cell13 cell14 cell21 cell22 cell23 cell24 cell31 cell32 cell33 cell34 cell41 cell42 cell43 cell44)
    (display "Row 1: ")
    (display cell11)(display " ")
    (display cell12)(display " ")
    (display cell13)(display " ")
    (display cell14)(display " ")(newline)
    (display "Row 2: ")
    (display cell21)(display " ")
    (display cell22)(display " ")
    (display cell23)(display " ")
    (display cell24)(display " ")(newline)
    (display "Row 3: ")
    (display cell31)(display " ")
    (display cell32)(display " ")
    (display cell33)(display " ")
    (display cell34)(display " ")(newline)
    (display "Row 4: ")
    (display cell41)(display " ")
    (display cell42)(display " ")
    (display cell43)(display " ")
    (display cell44)(display " ")(newline))) 