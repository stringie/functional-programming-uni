#lang racket
;;;functions
(define (maximum x) (apply max x))
(define (sum x) (apply + x))

(define (divides? k n) (= (remainder n k) 0))

(define (flatmap proc xs) 
    (apply append (map proc xs)))

(define (irange a b) (range a (+ b 1)))

(define (sieve-of-eratosthenes n) 
    (define (sieve xs) 
        (if (null? xs) null (cons (car xs) (sieve (filter (lambda (x) (not (divides? (car xs) x))) (cdr xs))))))
    (sieve (range 2 (+ n 1))))

(define (prime-factors n)
    (map (lambda (x) (cons x (maximum (filter (lambda (z) (divides? (expt x z) n)) (range 1 n))))) (filter (lambda (x) (divides? x n)) (sieve-of-eratosthenes n))))

(define (int->list y) (if (= y 0) null (append (int->list (quotient y 10)) (list (remainder y 10)))))

(define (get-sequential-pairs lst) 
    (map cons (reverse (cdr (reverse lst))) (cdr lst)))

(define (descending? x) 
    (if (= (length x) 1) #t (apply >= x)))

(define (remove-duplicates x) 
    (define (remove x a) 
        (cond [(null? x) null]
            [(= (car x) a) (remove (cdr x) a)] 
            [else (cons (car x) (remove (cdr x) a))]))
    (if (null? x) null (cons (car x) (remove-duplicates (remove (cdr x) (car x))))))

(define (num-bigger-elements lst) 
    (map (lambda(x) (cons x (length (filter (lambda(y) (> y x)) lst)))) lst))

(define (switchsum f g n) 
    (define (summer a x) 
        (define (applier b x) 
            (cond [(= b 0) x]
                  [(even? b) (g (applier (- b 1) x))]
                  [(odd? b) (f (applier (- b 1) x))]))
        (if (= a 0) 0 (+ (applier a x) (summer (- a 1) x))))
    (lambda(x) (summer n x)))

(define (repeater str)  
    (define (appender n glue) 
        (if (= n 1) str (string-append str glue (appender (- n 1) glue))))
    (lambda(c g) (appender c g)))

(define (sum-sum-digit a b k) 
    (define (sum-digit? x) 
        (divides? k (sum (int->list x))))
    (sum (filter sum-digit? (irange a b))))

(define (pair-compose fs) 
    (define (composer f x) 
        (cond [(null? f) 0]
              [(and (null? (cdr f)) (odd? (length f))) ((car f) x)]
              (else (+ ((car f) ((cadr f) x)) (composer (cddr f) x)))))
    (lambda(x) (composer fs x)))

(define (add2 x) (add1 (add1 x)))

(define (where predicates elements) 
    (if (null? predicates) elements (where (cdr predicates) (filter (car predicates) elements))))

(define (sum-numbers a b)
    (define (descending-number? x) (descending? (int->list x)))
    (sum (filter descending-number? (irange a b))))

(define (sublists l)
        (define (taker elements count) 
            (if (= 0 count) null (cons (take elements count) (taker elements (- count 1))))) 
        (if (null? l) '() (append (taker l (length l)) (sublists (cdr l)))))

(define (ascending? l) 
    (if (= (length l) 1) #t (apply <= l)))

(define (max-ordered-sublist lst)
    (argmax length (filter ascending? (sublists lst))))

;;;function


(cons 'z (list 1 2))
