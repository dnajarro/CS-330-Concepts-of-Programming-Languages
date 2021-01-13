#lang racket

(define (sum-coins pennies nickels dimes quarters)
  (define penny-value 1)
  (define nickel-value 5)
  (define dime-value 10)
  (define quarter-value 25)
  (+ (* penny-value pennies) (* nickel-value nickels) (* dime-value dimes) (* quarter-value quarters)))

(define (degrees-to-radians angle)
  (define half-circle-deg 180)
  (* (/ angle half-circle-deg) pi))

(define (sign x)
  (if (positive? x) 1
      (if (negative? x) -1 0)))

(define (new-sin angle type)
  (if (symbol=? type 'radians) (sin angle)
      (if (symbol=? type 'degrees) (sin (degrees-to-radians angle))
          (error "type must be 'radians or 'degrees"))))