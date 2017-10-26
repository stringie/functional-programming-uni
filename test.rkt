#lang racket
;;;functions
(define (maximum xs) (apply max xs))
(define (divides? k n) (= (remainder n k) 0))
(define (remove n) 
    (filter (lambda (x) (not (divides? n x))) (range 1 11)))
(define (flatmap proc xs) 
    (apply append (map proc xs)))
(define (sieve-of-eratosthenes n) 
    (define (sieve xs) 
        (if (null? xs) 
            null
            (cons (car xs) (sieve (filter (lambda (x) (not (divides? (car xs) x))) (cdr xs))))))
    (sieve (range 2 (+ n 1))))
(define (prime-factors n)
    (map (lambda (x) (cons x (maximum (filter (lambda (z) (divides? (expt x z) n)) (range 1 n))))) (filter (lambda (x) (divides? x n)) (sieve-of-eratosthenes n))))
;;;functions


(define (prime-factors-readable n)
    (define (max-divisor-power divisor divident) (maximum (filter (lambda (x) (divides? (expt divisor x) divident)) (range 1 (+ divident 1)))))
    (define (get-factor-primes number) (filter (lambda (x) (divides? x number)) (sieve-of-eratosthenes number)))
    (map (lambda (prime-factor) (cons prime-factor (max-divisor-power prime-factor n))) (get-factor-primes n)))
