#lang racket

(define ns (make-base-namespace))

(define (convertFC lst)
  (map (lambda (x) (* (- x 32) (/ 5 9))) lst))

(define (check-temps1 temps)
  (cond
    [(empty? (filter (lambda (x) (if (or (> x 95) (< x 5)) #t #f)) temps)) #t]
    [else #f]))

(define (check-temps temps low high)
  (cond
    [(empty? (filter (lambda (x) (if (or (> x high) (< x low)) #t #f)) temps)) #t]
    [else #f]))

(define (pow-10 pow)
  (expt 10 pow))

(define (convert lst)
  (define powers (range (length lst)))
  (define tens (map (lambda (x) (pow-10 x)) powers))
  (define reslst (map (lambda (x y)
           (* x y))
         lst
         tens))
    (foldr + 0 (map (lambda (x) (+ 0 x)) reslst)))

(define (duple lst)
  (map (lambda (x) (list x x)) lst))

(define (average lst)
  (define len (length lst))
  (define sum (foldr + 0 lst))
  (/ sum len))

(define (remove-elem lst elem)
  (filter (lambda (x) (if (> x elem) #t #f)) lst))

(define (eliminate-larger lst)
  (define positions (range (length lst)))
  (flatten (map (lambda (x y) (cond
                         [(= (length (rest (list-tail lst y))) (length (remove-elem (rest (list-tail lst y)) (first (list-tail lst y))))) x]
                         [else '()])) lst positions)))

(define (curry2 func)
  (lambda (x)
    (lambda (y)
      (func x y))))
