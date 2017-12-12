#lang racket
(#%require "exercises4.rkt")
(#%require "exercises3.rkt")
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


; Функции andmap/ormap:
; https://docs.racket-lang.org/reference/pairs.html#%28part._.List_.Iteration%29

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I. Операции върху множества ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Задача 1. Да се дефинира функцията (difference xs ys), която връща
; разликата на множествата xs и ys, представени чрез списъци
; (т.е. всички елементи на xs, които не са елементи на ys).
; Пример:
; > (difference '(1 2 3 4) '(2 4 6 8)) → '(1 3)
(define (difference xs ys) (filter (lambda(x) (not (member x ys))) xs))

; Задача 2. Да се дефинира функцията (union xs ys), която връща
; обединението на множествата xs и ys, представени чрез списъци.
;
; Пример:
; > (union '(1 2 3 4) '(2 4 6 8)) → '(1 2 3 4 6 8)
(define (union xs ys) (append xs (difference ys xs)))

; Задача 3. Да се дефинира функцията (intersection xs ys), която връща
; сечението на двете множествата xs и ys, представени чрез списъци.
;
; Пример:
; > (intersection '(1 2 3 4) '(2 4 6 8)) → '(2 4)
(define (intersection xs ys) (filter (lambda(x) (member x ys)) xs))

; Задача 4*. Да се дефинира функцията (cartesian-product xs ys), която връща
; като списък от двойки декартовото произведение на множествата xs и ys,
; представени чрез списъци.
;
; Пример:
; > (cartesian-product '(1 2) '(3 4)) → '((1 . 3) (1 . 4) (2 . 3) (2 . 4))
(define (cartesian-product xs ys) (apply append (map (lambda(x) (map (lambda(y) (cons x y)) ys)) xs)))                     
 
(examples
 (difference '(1 2 3 4) '(2 4 6 8))
 (union '(1 2 3 4) '(2 4 6 8))
 (intersection '(1 2 3 4) '(2 4 6 8))
 (cartesian-product '(1 2) '(3 4))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; II. Операции върху матрици  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Задача 5. Да се дефинира функцията (matrix? xss), която проверява дали
; списъкът от списъци xss е мaтрица (т.е. дали всичките му подсписъци/редове
; са с еднаква дължина).
;
; Пример:
; > (matrix? '((1 2 3) (4 5 6))) → #t
; > (matrix? '((1 2) (3 4) (5 6))) → #t
; > (matrix? '((1 2 3) (4 5) (6))) → #f
(define (matrix? xss) (apply = (map (lambda(x) (length x)) xss)))

(examples
 (matrix? '((1 2 3) (4 5 6)))
 (matrix? '((1 2) (3 4) (5 6)))
 (matrix? '((1 2 3) (4 5) (6)))
 )


; Задача 6*. Да се дефинира функцията (latin-square? xss), която проверява
; дали матрицата xss е латински квадрат. Можете да приемете, че за символи
; ще използваме числата от 1 до n, където n е големината на матрицата.
; 
; REF:
; - https://en.wikipedia.org/wiki/Latin_square
; - https://bg.wikipedia.org/wiki/Латински_квадрат
;
; Пример:
; > (latin-square? '((1 2) (2 1))) → #t
; > (latin-square? '((1 2 3) (3 1 2) (2 3 1))) → #t
; > (latin-square? '((1 2 3) (3 1 2) (3 2 1))) → #f
(define (latin-square? xss) 
    (define (elements-unique-in-each-row xss)
        (apply = (apply append (map (lambda(x) (map (lambda(xs) (length(filter (lambda(e) (= e x)) xs))) xss)) (car xss)))))
    (and 
         (elements-unique-in-each-row xss)
         (elements-unique-in-each-row (transpose xss))))

(examples
 (latin-square? '((1 2) (2 1)))
 (latin-square? '((1 2 3) (3 1 2) (2 3 1)))
 (latin-square? '((1 2 3) (3 1 2) (3 2 1)))
 )


 ;Задача 7**. Да се дефинира функция която проверява дали два списъка са
 ;пермутация един на друг за линейно време с даден списък от прости числа

 (define (check-permutation? xs ys)  
    (define (mapped-to-prime-product s l) 
        (apply * (map (lambda(x) (list-ref s x)) l)))
    (let [(sieve (sieve-of-eratosthenes 1000))] 
        (= 
            (mapped-to-prime-product sieve xs)
            (mapped-to-prime-product sieve ys))))


(check-permutation? '(6 5 3 9 8 7 1 2 4) '(9 5 6 7 4 3 2 8 1))