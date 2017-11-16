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

(define (remove-duplicates x) 
    (define (remove x a) 
        (cond [(null? x) null]
            [(= (car x) a) (remove (cdr x) a)] 
            [else (cons (car x) (remove (cdr x) a))]))
    (if (null? x) null (cons (car x) (remove-duplicates (remove (cdr x) (car x))))))
;;;functions

;;; (- (char->integer (string-ref "ab3d" 2)) (char->integer #\0))