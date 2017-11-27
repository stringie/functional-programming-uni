#lang racket
(define (int->list x) 
    (if (= x 0) null (append (int->list (quotient x 10)) (list(remainder x 10)))))
(define (list->int x sum) 
    (if (null? x) sum (list->int (cdr x) (+ (* sum 10) (car x)))))


(define (ksum k a b) 
     (apply + (map (curryr list->int 0) 
                   (map (lambda(x) (map cdr (filter (lambda(p) (= (remainder (car p) k) 0)) 
                                                    (map cons 
                                                         (range 0 (length (int->list x))) 
                                                         (reverse (int->list x)))))) 
                        (range a (+ b 1))))))

;;; (ksum 2 87654321 87654321)

(define (replace l change) 
    (map (lambda(x) (if (ormap (lambda(y) (= (car y) x)) change) (apply append (map cdr (filter (lambda(z) (= (car z) x)) change))) x)) l))

;;; (replace '(1 2 3 4 5) '((1 . 9)(5 . 7)(2 . 3)))

(define (concat a b) (string->number (apply string-append (map number->string (range a (+ b 1))))))

;;; (concat 19 23)

(define (mix a b) 
    (define (mixer ares bres x y i)
        (cond [(null? x) (cons ares bres)]
              [(even? i) (mixer (append ares (list (car x))) (append bres (list (car y))) (cdr x) (cdr y) (+ i 1))]
              [(odd? i) (mixer (append ares (list (car y))) (append bres (list (car x))) (cdr x) (cdr y) (+ i 1))]))
    (mixer null null a b 0))

;;; (car (mix '(1 2 3) '(4 5 6)))

(define (task f) 
    (define (tasker a b res i) 
        (if (> (f res) b) i (tasker a b (f res) (+ i 1))))
    (lambda(x y) (tasker x y x 0)))

;;; ((task (lambda(x) (* x 2))) 1 9)