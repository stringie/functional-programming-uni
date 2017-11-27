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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I. Функции от по-висок ред      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Примери:
(define (my-const x)
  (λ(_) x))
(define (my-compose f g)
  (λ(x) (f (g x))))
(define (derivative f eps)
  (λ(x) (/ (- (f (+ x eps)) (f (- x eps))) (* 2 eps))))

(let ([ten (my-const 10)]
      [add-ten-then-double (my-compose (λ(x) (* 2 x)) (λ(x) (+ x 10)))])
  (examples
   (ten 1)
   (ten 2)
   (ten 1000)
   (add-ten-then-double 1)
   (add-ten-then-double 10)
   (add-ten-then-double 100)
   ((derivative (λ(x) (* x x x)) 0.01) 1)
   ((derivative (λ(x) (* x x)) 0.0001) 10)
   ))


; Задача 1. Дефинирайте следните функции от по-висок ред:
; а). (fmin f g), която приема две едноместни числови функции f и gh
; и връща едноместни числова функция, чиято стойност в точка x е
; минимума на f и g.
; б). (fmax f g), като fmin, но връща максимума на f и g.
; в). (favg f g), като fmin, но връща средното аритметично на f и g.
(define (fmin f g) (lambda (x) (min (f x) (g x))))

(define (fmax f g) (lambda(x) (max (f x) (g x))))

(define (favg f g) (lambda(x) (/ (+ (f x) (g x)) 2)))

(let ([f (λ(x) (* x x))]
      [g (λ(x) (+ x 1))])
  (examples
   ((fmin f g) 1)
   ((fmin f g) 10)
   ((fmax f g) 1)
   ((fmax f g) 10)
   ((favg f g) 1)
   ((favg f g) 10)
   ))


; Задача 2. Дефинирайте следните функции от по-висок ред:
; а). (bound-up f upper), която приема едноместни числова функция f и
; и числова стойност up и връща едноместни числова функция, чиято
; стойност в точка x е минимума на f(x) и up.
; б). (bound-down f lower), същата като bound-up, но връща максимума
; на f(x) и down.
(define (bound-up f upper) (lambda (x) (min (f x) upper)))

(define (bound-down f lower) (lambda (x) (max (f x) lower)))

(let ([fup (bound-up (λ(x) (+ x 1)) 10)]
      [fdown (bound-down (λ(x) (* x x)) 5)])
  (examples
   (fup 0)
   (fup 10)
   (fdown 0)
   (fdown 10)
   ))
 

; Задача 3. Дефинирайте функцията (closest-point xys), която приема
; списък от точки в равнината (представени чрез двойки (x . y)) и връща
; едноаргументна функция, чиято стойност в дадена точка p e най-близката
; до p точка от xys.
(define (closest-point xys) 
    (define (distance x y) 
        (sqrt (+ (expt (- (car x) (car y)) 2) (expt (- (cdr x) (cdr y)) 2))))
    (define (pair-point-distance x) 
        (map (lambda(y) (cons y (distance x y))) xys))
    (lambda (x) (car (apply append (filter (lambda(z) (= (cdr z) (apply min (map (lambda(e) (cdr e)) (pair-point-distance x))))) (pair-point-distance x))))))

(examples
 ((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(3 . 3))
 ((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(6 . 6))
 ((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(10 . 1))
 ((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(1 . 10))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; II. Асоциативни списъци и графи ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Задача 4. Дефинирайте функцията (nodes edges), която приема
; списъс с ребрата edges на даден ориентиран граф (в който всяко
; ребро е представено като двойка (from . to)) и връща списък,
; съдържащ всички върхове на съответния граф.
(define (nodes edges) 
    (define (get-nodes edges) (apply append (map (lambda(x) (list (car x) (cdr x))) edges)))
    (define (remove-duplicates x) 
        (define (remove x a) 
            (cond [(null? x) null]
                [(= (car x) a) (remove (cdr x) a)] 
                [else (cons (car x) (remove (cdr x) a))]))
        (if (null? x) null (cons (car x) (remove-duplicates (remove (cdr x) (car x))))))
    (remove-duplicates (get-nodes edges)))


; Задача 5. Дефинирайте функцията (adjacency-list edges), която приема
; списъс с ребрата edges на даден ориентиран граф (в който всяко
; ребро е представено като двойка (from . to)) и връща списъка на
; наследниците на съответния граф.
(define (adjacency-list edges) 
    (map (lambda(e) (cons e (list (map (lambda(z) (cdr z)) (filter (lambda(x) (= (car x) e)) edges))))) (nodes edges)))


; Задача 6. Дефинирайте функцията (path? edges nodes), която приема
; списъс с ребрата edges на даден ориентиран граф и списък от върхове
; nodes и връща дали списъкът nodes е път в графа описан от edges.
(define (path? edges nodes) 
  null)


; Задача 7*. Дефинирайте функцията (simple-paths edges k from), която приема
; списъс с ребрата edges на даден ориентиран граф, цяло число k и идентификатор
; на връх node и връща всички прости пътища с дължина k, които започват от from.
(define (simple-paths edges k from) null)


; Задача 8*. Дефинирайте функцията (all-simple-paths edges from to), която приема
; списъс с ребрата edges на даден ориентиран граф и два идентификатора на върхове
; from и to и връща всички прости пътища, които започват от from и завършват в to.
(define (all-simple-paths edges from to) null)


; Примери върху графи
(let ([edges '((1 . 2) (1 . 3) (2 . 3) (3 . 2) (2 . 4) (3 . 5) (5 . 4))])
  (examples
   (nodes edges)
   (path? edges '(1 2 3 5))
   (path? edges '(1 2 5 3))
   (adjacency-list edges)
   (simple-paths edges 2 1)
   (simple-paths edges 3 1)
   (simple-paths edges 5 1)
   (all-simple-paths edges 1 4)
   (all-simple-paths edges 1 5)
   ))
  