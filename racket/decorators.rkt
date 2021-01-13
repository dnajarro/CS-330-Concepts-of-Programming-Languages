#lang racket

(define (degrees-to-radians angle)
  (define half-circle-deg 180)
  (* (/ angle half-circle-deg) pi))

(define (new-sin angle type)
  (if (symbol=? type 'radians) (sin angle)
      (if (symbol=? type 'degrees) (sin (degrees-to-radians angle))
          (error "type must be 'radians or 'degrees"))))

(define (default-parms f values)
  (lambda args
    (if (equal? (length args) (length values))
        (apply f args) (apply f (flatten (append args (list-tail values (length args))))))))

(define (type-parms f types)
  (lambda args
    (define paired (map (lambda (x y) (apply x (list y))) types args))
    (define res (andmap true? paired))
      (if (equal? res #t)
        (apply f args) (error "ERROR MSG"))))

(define (true? x)
  (if (equal? x #t)
      #t #f))

(define new-sin2 (default-parms 
            (type-parms
              new-sin
             (list number? symbol?)) 
            (list 0 'radians)))