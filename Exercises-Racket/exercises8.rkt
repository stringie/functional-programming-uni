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


; Задача 1. Нека sxs е поток, а n цяло число.
; Дефинирайте следните функции:
; а). (stream-take n sxs), която връща списък с първите n
; елемента на sxs.
; б). (stream-drop n sxs), която връща поток който започва
; от n-тия елемент на sxs.    
(define (stream-take n sxs)
    (if (or (= n 0) (stream-empty? sxs)) 
            null 
            (cons (stream-first sxs) 
                  (stream-take (- n 1) (stream-rest sxs)))))

(define (stream-drop n sxs)
    (if (or (= n 0) (stream-empty? sxs)) sxs (stream-drop (- n 1) (stream-rest sxs))))


; Задача 2. Дефинирайте следните (безкрайни) потоци:
; a). (stream-const c) - повтаряме c до безкрай.
; б). naturals - естествените числа [0..].
; в). fibs - редицата на Фибоначи.
; г). primes - простите числа (чрез ситото на Ератостен).
(define (stream-const c)
  (stream-cons c (stream-const c)))

(define (stream-naturals n) 
    (stream-cons n (stream-naturals (+ n 1))))
(define naturals (stream-naturals 0))

(define (fib-stream a b) 
    (stream-cons a (fib-stream b (+ a b))))
(define fibs (fib-stream 0 1))

;;; Oretachi no solution
;;; (define (stream-prime n past-primes) 
;;;     (if (andmap (lambda(p) (not (= (remainder n p) 0))) past-primes) 
;;;         (stream-cons n (stream-prime (+ n 1) (cons n past-primes)))
;;;         (stream-prime (+ n 1) past-primes)))

;;; Better solution
(define (stream-prime numbers) 
    (stream-cons (stream-first numbers) 
                 (stream-prime (stream-filter (lambda(p) (not (= (remainder p (stream-first numbers)) 0))) 
                                              numbers))))

(define primes
  (stream-prime (stream-drop 2 naturals)))

(examples
 (stream-take 10 (stream-const 42))
 (stream-take 10 naturals)
 (stream-take 10 (stream-drop 20 naturals))
 (stream-take 10 fibs)
 (stream-take 10 primes)
 )

