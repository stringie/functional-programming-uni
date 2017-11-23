#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Помощни макроси за "pretty-print" на примерите ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (example stx)
  (if (string? stx)
      (printf "; ~a~n" stx)
      (printf "> ~s → ~v~n" 'stx stx)))

(define-syntax examples
  (syntax-rules ()
    [(examples x)
     (begin (example x) (displayln ""))]
    [(examples x y ...)
     (begin (example x) (examples y ...))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Полезни помощни функции върху числа
(define (last-digit n) (remainder n 10))
(define (front-digits n) (quotient n 10))
(define (divides? k n) (= 0 (remainder n k)))

; Основни функции върху списъци:
; - cons, car, cdr
; - null, null?
; - list, list?
; - map, filter, apply
; - length, reverse, member, remove-duplicates

; Полезни помощни функции върху списъци
(define (minimum xs) (apply min xs))
(define (maximum xs) (apply max xs))
(define (sum xs) (apply + xs))
(define (product xs) (apply * xs))
(define (concat xss) (apply append xss))


; Задача 1. Дефинирайте функцията (max-prime n), която връща
; най-голямото просто число по-малко или равно на n.
(define (max-prime n) 
    (define (sieve-of-erat num) 
        (define (sieve xs) 
            (if (null? xs) null (cons (car xs) (sieve (filter (lambda(x) (not (divides? (car xs) x))) (cdr xs))))))
        (sieve (range 2 (+ num 1))))
    (apply max (sieve-of-erat n)))

(examples
 (max-prime 10)
 (max-prime 100)
 )


; Задача 2. Дефинирайте функцията (inside-cirlce? c r), която
; приема n-мерна точка c (представена като списък от компоненти)
; и число r, и връща функция, чиято стойност в дадена точка p
; е дали p се намира вътре в n-мерната сфера с център c и радиус r.
(define (square x) (* x x))
(define (distance x y) 
    (sqrt (sum (map square (map - x y)))))
(define (inside-circle? c r)
    (lambda(x) (<= (distance c x) r)))

(examples
 ((inside-circle? '(0 0) 1) '(0 1))
 ((inside-circle? '(0 0) 1) '(1 0))
 ((inside-circle? '(0 0) 1) '(1 1))
 )


; Задача 3. Дефинирайте функцията (count-min-crosses fs), която
; приема списък от числа fs, съответсващи на стойностита на дадена
; непрекъсната функция f в интервала [0 .. n] и връща минималния
; брой пъти, които f пресича абсцисата в дадения интервал.
(define (count-min-crosses fs) 
    (define (pairs f) 
        (if (null? (cdr f)) null (cons (take f 2) (pairs (cdr f)))))
    (length (filter (lambda(x) (<= x 0)) (map (lambda(y) (apply * y)) (pairs fs)))))

(examples
 (count-min-crosses '(0 1 2 3 4))
 (count-min-crosses '(-1 1 -1 1))
 )

; Задача 4. Дефинирайте функцията (reverse-column i xss),
; която приема матрица xss (представена като списъс от списъци)
; и индекс на колона i (започващ от нула) и обръща елементите
; и връща матрица, в която елементите на i-тата колона са
; обърнати.
; Примери:
;
; (reverse-column 0 '((1 2 3)    → '((7 2 3)
;                     (4 5 6)        (4 5 6)
;                     (7 8 9)))      (1 8 9))
;
; (reverse-column 1 '((1 2 3)    → '((1 8 3)
;                     (4 5 6)        (4 5 6)
;                     (7 8 9)))      (7 2 9))
(define (reverse-column i xss) 
    (define (transpose matrix)  
        (apply map (cons list matrix)))
    (define (helper n ll) 
        (cond [(= n (length xss)) null]
              [(= n i) (cons (reverse (car ll)) (helper (+ n 1) (cdr ll)))]
              [else (cons (car ll) (helper (+ n 1) (cdr ll)))]))
    (transpose (helper 0 (transpose xss))))

(examples
 (reverse-column 0 '((1 2 3)
                     (4 5 6)
                     (7 8 9)))
 (reverse-column 1 '((1 2 3)
                     (4 5 6)
                     (7 8 9)))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Допълнителни задачи ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Задaча 1. Да се дефинира функцията (fib-sum-even n), която връща сбора
; на четните числа на фибоначи, които са по-малки или равни на n.
(define (fib-sum-even n) 
    (define (fib first second) 
        (if (> first n) null (cons first (fib second (+ first second)))))
    (sum (filter even? (fib 1 1))))

(examples
 (fib-sum-even 10)
 (fib-sum-even 1000)
 )

; Задача 2. Да се дефинира функцията (list-palindromes a b), която връща
; списък с всички числа палиндроми в интервала [a .. b].
(define (list-palindromes a b)
    (define (num-to-digits y) 
            (if (= y 0) null (cons (remainder y 10) (num-to-digits (quotient y 10)))))
    (filter (lambda(x) (equal? (reverse x) x)) (map num-to-digits (range a (+ b 1)))))

(examples
 (list-palindromes 10 99)
 (list-palindromes 300 500)
 )

; Задача 3. Дефинирайте функцията (closest-outside-circle c r ps),
; която приема n-мерна точка c, число r и списък от n-мерни точки ps
; и връща най-близката до c точка от ps, която се намира извън кръга
; с център c и радиус r.
(define (closest-outside-circle c r ps) 
    (define (outside-circle? c r)
        (lambda(x) (> (distance c x) r)))
    (argmin (curry distance c) (filter (outside-circle? c r) ps)))

(examples
 (closest-outside-circle '(0 0) 1 '((0 1) (2 2) (1 0) (1 1)))
 (closest-outside-circle '(1 1) 1 '((0 1) (2 2) (1 0) (1 1)))
 )

; Задача 4. Дефинирайте функцията (diagonal-product matrix),
; която приема числова квадратна матрица matrix и връща
; скаларното произведение на двата и диагонала.
;
; Примери:
; (diagonal-product '((1 0 1)
;                     (0 2 0)
;                     (3 0 3))) → 14
;
; (diagonal-product '((1 0 3)
;                     (0 2 0)
;                     (1 0 3))) → 10
(define (diagonal-product matrix) #f)

(examples
 (diagonal-product '((1 0 1)
                     (0 2 0)
                     (3 0 3)))
 (diagonal-product '((1 0 3)
                     (0 2 0)
                     (1 0 3)))
 )