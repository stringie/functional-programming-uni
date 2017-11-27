#lang racket

(define (reverse l) 
    (define (iter lst answer) 
        (if (null? lst) 
            answer
            (iter (cdr lst) (cons (car lst) answer))))
    (iter l null))

(reverse (list 1 2 3 4))

(define (append x y) 
    (if (null? x) y (cons (car x) (append (cdr x) y))))

(append (list 1 2 3) (list 4 5 6))

(define (remove-duplicates x) 
    (define (remove x a) 
        (cond [(null? x) null]
            [(= (car x) a) (remove (cdr x) a)] 
            [else (cons (car x) (remove (cdr x) a))]))
    (if (null? x) null (cons (car x) (remove-duplicates (remove (cdr x) (car x))))))

(remove-duplicates (list 1 2 2 2 4))

(define (digits y)
    (define (get-digits x) 
        (if (< x 10) 
            (list x) 
            (cons (remainder x 10) (get-digits (quotient x 10)))))
    (reverse (get-digits y)))

(digits 1234)

(define (bulls-and-cows secret guess) 
    (define (bulls s g) 
        (cond [(null? s) 0]
              [(= (car s) (car g)) (+ 1 (bulls (cdr s) (cdr g)))]
              [else (bulls (cdr s) (cdr g))]))
    (define (cows s g)
        (cond [(null? g) 0]
              [(member (car g) s) (+ 1 (cows s (cdr g)))]
              [else (cows s (cdr g))]))
    (cons (bulls (digits secret) (digits guess)) (- (cows (digits secret) (digits guess)) (bulls (digits secret) (digits guess)))))

(bulls-and-cows 1324 1234)
