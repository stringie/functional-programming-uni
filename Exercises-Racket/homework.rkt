#lang racket

(define (int->list x) 
    (if (= x 0) null (append (int->list (quotient x 10)) (list (remainder x 10)))))
(define (sum x) (apply + x))
(define (whitespacearize c) 
    (if (not (char-numeric? c)) #\space  c))


(define (convert x k n) 
    (define (convert-to-decimal x k) 
        (sum (map (lambda(e y) (* e (expt k y))) (int->list x) (reverse (range 0 (length (int->list x)))))))    
    (define (convert-from-decimal x n) 
        (define (get-max-power num n) 
            (define (i a) (if (> (expt a n) num) (- a 1) (i (+ a 1)))) 
            (i 0))
        (define (iter result num power) 
            (let* ([power-applied (expt n power)]
                   [base (quotient num power-applied)])
                (if (= power 0) (+ (* result 10) base) (iter (+ (* result 10) base) (- num (* base power-applied)) (- power 1)))))
        (iter 0 x (get-max-power x n)))
    (convert-from-decimal (convert-to-decimal x k) n))

;;; (convert 123 10 2)

(define (sum-numbers str)
    (sum (map string->number (string-split (list->string (map whitespacearize (string->list str)))))))

;;; (sum-numbers "a123b2c56")

(define (encode str)
    (define (counter x l count) 
        (if (or (null? l) (not (equal? (car l) x))) count (counter x (cdr l) (+ count 1))))
    (define (repeat-cdr l count) 
        (if (= count 0) l (repeat-cdr (cdr l) (- count 1))))
    (if (null? str) null (cons (list (car str) (counter (car str) str 0)) (encode (repeat-cdr str (counter (car str) str 0))))))

;;; (encode '(m i s s i s s i p p i))

(define (maximize fs) 
    (lambda(x) (argmax abs (map (lambda(f) (f x)) fs))))


((maximize (list (lambda(x) (- x 10)) (lambda(x) (- x 5)))) 5)