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

; 1. apply fn xs (https://docs.racket-lang.org/reference/procedures.html)
;
; Примери:
; > (+ 1 2 3)                  -> ?
; > (+ (list 1 2 3))           -> ?
; > (apply + (list 1 2 3))     -> ?
;
;
; 2. filter p? xs (https://docs.racket-lang.org/reference/pairs.html#%28part._.List_.Filtering%29)
;
; Примери:
; > (filter even? (range 1 10))     -> ?
; > (filter odd? (range 1 10))      -> ?
; > (filter prime? (range 1 100))   -> ?
;
; Примерна рекурсивнa дефиниция на filter:
(define (my-filter p? xs)
  (if (null? xs)
      null
      (let* ([head (car xs)]
             [tail (cdr xs)]
             [filter-tail (my-filter p? tail)])
        (if (p? head)
            (cons head filter-tail)
            filter-tail))))

; Помощни функции:
(define (sum xs) (apply + xs))
(define (divides? k n) (= (remainder n k) 0))
(define (prime? n) (and (> n 1) (null? (my-filter (λ(k) (divides? k n)) (range 2 (+ 1 (floor (sqrt n))))))))

; Упр. 2, Зад. 2:
(define (sum-primes a b)
  (if (> a b)
      0
      (+ (sum-primes (+ a 1) b) (if (prime? a) a 0))))

; Oпростено решение с filter/sum
(define (sum-primes-new a b)
  (sum (filter prime? (range a (+ b 1)))))


; Зад 0. Полезни "едноредки" за операции върху списъци:
; Използвайте apply и функциите min, max, * и append, за да дефинирате
; следните функции:
; а). (minimum xs), която приема списък xs и връща най-малкия му елемент
; б). (maximum xs), която приема списък xs и връща най-големия му елемент
; в). (product xs), която връща произведението на елементите на списъка xs.
; г). (concat xss), която приема списък от списъци xss, и конкатенира всички
;      елементи (подсписъци) на xss.
(define (minimum xs) (apply min xs))
(define (maximum xs) (apply max xs))
(define (product xs) (apply * xs))
(define (concat xss) (apply append xss))

(examples
 (minimum (range 1 10))
 (maximum (range 1 10))
 (sum (range 1 11))
 (product (range 1 6))
 (concat '((1 2) (3 4) (5 6)))
 )


; Зад 1. Да се дефинира функцията (count-minimum xs), която връща
; броя на срещанията на най-малкия елемент на списъка xs в него.
;
; Пример:
; > (count-minimum (list 1 2 3 2 1 1 3 2)) → 3
(define (count-minimum xs) 
    (length (filter (lambda (x) (= (maximum xs) x)) xs)))

(examples
 (count-minimum (list 1 2 3 4))
 (count-minimum (list 1 2 3 2 1 1 3 2))
 )


; Задача 2. Да се дефинира функцията (count-occurrences xs subs),
; която по зададени списъци xs и subxs връща като резултат броя на
; срещанията на списъка subs в списъка xs.
;
; Пример:
; > (count-occurrences '(1 2 3 2 1 4 1 2 3) '(1 2)) -> 2
(define (count-occurrences xs subs) 
    (define (get-sublists xs n)
        (if (< (length xs) n)
            null 
            (cons (take xs n) (get-sublists (cdr xs) n))))
    (length (filter (lambda (x) (equal? x subs)) (get-sublists xs (length subs)))))


(examples
 (count-occurrences '() '(1 2))
 (count-occurrences '(1 2 3 2 1 4 1 2 3) '(1 2))
 )


; Задача 3. Дефинирайте следните функции:
; a). (my-identity x), функцията идентитет: връща каквото и дадете.
; б). (my-compose f g), която връща композицията на функциите f и g.
; в). (my-negate p?), която приема предикат p? и връща предиката (not p?).
; г). (my-curry f x), която приема многоаргумента функция f и първи аргумент x
;      и връща функцията получена от частичното прилагане на x върху f.
; д). (repeatf f n), която връща n-кратната композиция на функцията f.
(define (my-identity x) x)
(define (my-compose f g) (lambda (x) (f (g x))))
(define (my-negate p?) (lambda (x) (not (p? x))))
(define (my-curry f x) (lambda args (apply f (cons x args))))
(define (repeatf f n) 
    (if (= n 0)
        my-identity
        (my-compose f (repeatf f (- n 1)))))

; Примери
(define not-even? (my-negate even?))
(define add1 (my-curry + 1))
(define twice (my-curry * 2))
(define 2*n+1 (my-compose add1 twice))
(define add10 (repeatf add1 10))

(examples
 (my-identity 10)
 (my-identity (λ(x) (* x x)))
 (not-even? 3)
 (not-even? 4)
 (add1 10)
 (twice 10)
 (2*n+1 10)
 (add10 10)
 )


; Задача 4*. Решето на Ератостен:
; Да се дефинира функцията (sieve-of-eratosthenes n), която приема
; целочисления аргумент n и връща списък с всички прости числа
; по-малки или равни на n, като за целта използвайте решетото на
; Ератостен (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).
;
; Примери:
; > (sieve-of-eratosthenes 10) → '(2 3 5 7)
; > (sieve-of-eratosthenes 100) → '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
(define (sieve-of-eratosthenes n) 
    (define (sieve xs) 
        (if (null? xs) 
            null
            (cons (car xs) (sieve (filter (lambda (x) (not (divides? (car xs) x))) (cdr xs))))))
    (sieve (range 2 (+ n 1))))

(examples
 (sieve-of-eratosthenes 2)
 (sieve-of-eratosthenes 3)
 (sieve-of-eratosthenes 10)
 (sieve-of-eratosthenes 100)
 )

; Задача 5*. Разлагане на прости делители:
; Да се дефинира функцията (prime-factors n), която приема
; целочисления аргумент n и връща списък от двойки (pairs)
; от тип (pi . ki), където pi e i-тия прост делител на n, a
; ki степента на pi във факторизацията на n.
;
; Примери:
; 
; > (prime-factors 10) → '((2 . 1) (5 . 1))          ;  10 = 2^1 * 5^1
; > (prime-factors 360) → '((2 . 3) (3 . 2) (5 . 1)) ; 360 = 2^3 * 3^2 * 5^1
(define (prime-factors n)
    (map (lambda (x) (cons x (maximum (filter (lambda (z) (divides? (expt x z) n)) (range 1 n))))) (filter (lambda (x) (divides? x n)) (sieve-of-eratosthenes n))))

(examples
 (prime-factors 2)
 (prime-factors 3)
 (prime-factors 10)
 (prime-factors 12)
 (prime-factors 97)
 (prime-factors 360)
 (prime-factors 792)
 (prime-factors 1024)
 )