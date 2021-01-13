#lang racket

;;(define (all-even? list)
  ;;(if (empty> lst) #t
   ;;   (and (even? (first lst))
    ;;       (all-even? (rest lst)))})

(define (check-temps1 temps)
  (cond
    [(empty? temps) #t]
    [else (if
           (or (< (first temps) 5) (> (first temps) 95))
              #f (check-temps1(rest temps)))]))

(define (check-temps temps low high)
  (cond
    [(empty? temps) #t]
    [else (if
           (or (< (first temps) low) (> (first temps) high))
           #f (check-temps(rest temps) low high))]))

;; helper function
(define (pow-10 pow)
  (expt 10 pow))

(define (convert digits)
  (define (link remaining sum)
    (cond
      [(empty? remaining) sum]
      [else (link (rest remaining) (+ sum (* (first remaining) (pow-10 (- (length remaining) 1)))))]))
  (link (reverse digits) 0))

(define (duple lst)
  (define (doub nums res)
    (cond
      [(empty? nums) res]
      [else (doub (rest nums) (append res (list (list (first nums) (first nums)))))]))
  (doub lst (list)))

(define (average lst)
  (define (calc remaining sum len)
    (cond
      [(empty? remaining) (/ sum len)]
      [else (calc (rest remaining) (+ sum (first remaining)) (+ len 1))]))
  (calc lst 0 0))

(define (convertFC temps)
  (define (conversion lst newlst)
    (cond
      [(empty? lst) newlst]
      [ else (conversion (rest lst) (append newlst (list (* (- (first lst) 32) (/ 5 9)))))]))
  (conversion temps (list)))

(define (remove-elem lst res)
  (if (empty? lst)
      res
      (if (> (first res) (first lst))
          (rest res)
          (remove-elem (rest lst) res))))

(define (eliminate-larger lst)
  (define (remove-larger lst res)
    (define startlst (if (empty? lst)
                         (list)
                         (rest lst)))
    (define reslst (flatten (append res (remove-elem startlst (if (empty? lst)
                                                                  (list)
                                                                  (list (first lst)))))))
	(cond
		[(empty? startlst) reslst]
		[else (remove-larger startlst reslst)]))
  (remove-larger lst (list)))

(define (get-nth lst n)
  (define (shorten remaining index)
    (cond
      [(= index 0) (first remaining)]
      [else (shorten (rest remaining) (- index 1))]))
  (shorten lst n))

(define (find-item lst target)
  (define (searching remaining val index)
    (cond
      [(empty? remaining) -1]
      [(= val (first remaining)) index]
      [else (searching (rest remaining) val (+ index 1))]))
  (searching lst target 0))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
