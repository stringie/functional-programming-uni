#lang racket

(define (automorphic? n) 
    (define (get-digit-length x) 
        (if (= x 0) 0 (+ 1 (get-digit-length (quotient x 10)))))
    (= (remainder (- (* n n) n) (expt 10 (get-digit-length n))) 0))

;;(automorphic? 1)

(define (series x n) 
    (if (= n 0)
        1
        (+ 1 (* x (series x (- n 1))))))

(series 2 9)