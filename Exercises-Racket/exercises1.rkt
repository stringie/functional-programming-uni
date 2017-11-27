#lang racket

(define (bc n k) 
    (if (= k 0) 
        1 
        (* (/ n k) (bc (- n 1) (- k 1)))))


(define (reverse-digits n) 
    (define (reverse n acc) 
    (if [= n 0]
        acc
        (reverse (quotient n 10) (+ (* 10 acc) (remainder n 10)))))
    (reverse n 0))

(define (sum-primes a b) 
    (define (is-prime? x)
        (define (prime div) 
            (cond [(> div (sqrt x)) true]
                  [(= (remainder x div) 0) false]
                  [else (prime (+ div 1))]))
        (prime 2))
    (cond [(> a b) 0]
          [(is-prime? a) (+ a (sum-primes (+ a 1) b))]
          [else (sum-primes(+ a 1) b)]))

(define (perfect-number x) 
    (define (sum-of-div a) 
        (cond [(= a x) 0]
              [(= (remainder x a) 0) (+ a (sum-of-div (+ a 1)))]
              [else (sum-of-div (+ a 1))]))
    (= x (sum-of-div 1)))


(define (inc-digits? x) 
    (define (is-ascending-pair? a b number) 
        (cond [(or (= a 0) (= b 0)) false]
              [(and (not (= number 0)) (= (quotient number 10) 0) ) true]
              [(or (= a b) (< a b)) false]
              [(> a b) (is-ascending-pair? b (remainder (quotient number 10) 10) (quotient number 10))]))
    (is-ascending-pair? (remainder x 10) (remainder (quotient x 10) 10) (quotient x 10)))

(bc 6 3)
(reverse-digits 12345)
(sum-primes 2 10)
(perfect-number 496)
(inc-digits? 12346789)

