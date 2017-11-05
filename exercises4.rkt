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


; Функцията map fn xs ys...
;
; Reference:
; - https://docs.racket-lang.org/guide/Lists__Iteration__and_Recursion.html
; - https://docs.racket-lang.org/reference/pairs.html#%28part._.List_.Iteration%29
;
; Примери:
; > (map sqrt (list 1 4 9 16))             -> ?
; > (map (λ(x) (* x x)) (list 1 2 3 4))    -> ?
; > (map + '(1 2 3) '(10 20 30))           -> ?
;
; N.B. map e "for цикъла" на функционалното профрамиране :)

; Примерна рекурсивнa дефиниция на map за едноаргументна функция:
(define (map1 f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (map1 f (cdr xs)))))

; Примерна рекурсивнa дефиниция на map (общ случай):
(define (my-map f . xss)
  (if (or (null? xss) (null? (car xss)))
      null
      (cons (apply f (map1 car xss)) (apply my-map f (map1 cdr xss)))))


; Помощни ф-и (от Упр. 4).
(define (minimum xs) (apply min xs))
(define (maximum xs) (apply max xs))
(define (sum xs) (apply + xs))
(define (product xs) (apply * xs))
(define (concat xss) (apply append xss))


; Пример: Скаларно произведение на 2 вектора представени чрез списъци:
(define (dot-product xs ys) (sum (map * xs ys)))

(examples
 (map1 (λ(x) (* x x)) '(1 2 3 4))
 (my-map + '(1 2 3) '(10 20 30))
 (dot-product '(1 2 3) '(10 20 30))
 )


; Задача 1. Да се дефинира функцията (sum-of-odd-squares xs),
; която връща сбора на квадратите на нечетните числа
; в списъка xs.
;
; Примери:
; > (sum-of-odd-squares (list 5 7)) → 74
; > (sum-of-odd-squares (list 2 4 6 8)) → 0
; > (sum-of-odd-squares (range 1 6)) → 35
(define (sum-of-odd-squares xs) 
    (sum (map (lambda(x) (* x x)) (filter odd? xs))))

(examples
 (sum-of-odd-squares (list 5 7))
 (sum-of-odd-squares (list 2 4 6 8))
 (sum-of-odd-squares (range 1 6))
 )


; Задача 2. Да се дефинира функцията (num-bigger-elements xs),
; която за даден списък от числа xs връща като резултат
; списък с двойки (xi ni), където xi е i-тият елемент на xs,
; а ni е броят на елементите на xs, по-големи от xi.
;
; Примери:
; > (num-bigger-elements '(5 6 3 4)) → '((5 . 1) (6 . 0) (3 . 3) (4 . 2))
; > (num-bigger-elements '(1 1 1)) → '((1 . 0) (1 . 0) (1 . 0))
(define (num-bigger-elements xs) 
    (map (lambda(x) (cons x (length (filter (lambda(y) (> y x)) xs)))) xs))

(examples
 (num-bigger-elements '(5 6 3 4))
 (num-bigger-elements '(1 1 1))
 )

              
; Задача 3. Да се дефинира функцията (sum-unique xss),
; която по списък от списъци от цели числа xss намира
; сумата на тези от числата, които са уникални
; в рамките на списъка, в който се срещат.
;
; Примери:
; > (sum-unique '((1 2 3 2) (-4 -4) (5))) → 9
; > (sum-unique '((2 2 2) (3 3) (4))) → 4
; > (sum-unique '((1 2 3) (4 5 6) (7 8 9))) → 45
(define (sum-unique xss)    
    (sum (apply append (map (lambda(z) (filter (lambda(x) (= (length (filter (lambda(y) (= y x)) z)) 1)) z)) xss))))



(examples
 (sum-unique '((1 2 3 2) (-4 -4) (5)))
 (sum-unique '((2 2 2) (3 3) (4)))
 (sum-unique '((1 2 3) (4 5 6) (7 8 9)))
 )


; Задача 4. Да се дефинира функцията (repeat n x), която приема
; целочислен аргумент n и произволен обект x и връща списък,
; състоящ се от x повторено n пъти.
;
; Примери:
; > (repeat 2 10) → '(10 10)
; > (repeat 3 1) → '(1 1 1)
; > (repeat 5 (list 1 2)) → '((1 2) (1 2) (1 2) (1 2) (1 2))
(define (repeat n x) 
    (map (lambda(y) x) (range 0 n)))

(examples
 (repeat 2 10)
 (repeat 3 1)
 (repeat 5 (list 1 2))
 )


; Задача 5. Да се дефинира функцията (Id n), която
; приема целочислен аргумент n и връща единичната матрица
; с размер n (представена като списък от n линии, всеки -
; списък от n елемента).
;
; Примери:
;
; > (Id 2) → '((1 0) (0 1))
; > (Id 4) → '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1))
(define (Id n) 
    (map (lambda(x) (map (lambda(y) (if (= y x) 1 0)) (range 0 n))) (range 0 n)))

(examples
 (Id 0)
 (Id 1)
 (Id 2)
 (Id 4)
 )


; Задача 6*. Да се дефинира функцията (transpose xss), която
; приема матрица xss (представена като списък от списъци) и
; връща транспонираната на xss матрица.
;
; Примери:
; > (transpose '((1 2 3 4))) → '((1) (2) (3) (4))
; > (transpose '((1 2) (3 4))) → '((1 3) (2 4))
; > (transpose '((1 2 3) (4 5 6))) → '((1 4) (2 5) (3 6))
(define (transpose xss) 
    (apply map (cons list xss)))

(examples
 (transpose '((1 2 3 4)))
 (transpose '((1 2) (3 4)))
 (transpose '((1 2 3) (4 5 6)))
 )


; Задача 7**. Да се дефинира (на един ред :) функцията (matrix-multiply xss yss),
; която приема матриците xss и yss (представени като списъци от списъци)
; и връща тяхното матрично произведение.
(define (matrix-multiply xss yss) 
    (transpose (map (lambda (x) (map (lambda(y) (dot-product x y)) xss)) (transpose yss))))

(examples
 (matrix-multiply (Id 2) '((1 2 3) (4 5 6)))
 (matrix-multiply '((1 1) (0 1)) '((1 2 3) (4 5 6)))
 (matrix-multiply '((1 1) (0 0)) '((1 2 3) (4 5 6)))
 )