(define (fib n)
  (define (fib-iter a b n)
    (if (zero? n)
        b
        (fib-iter (+ a b ) a (- n 1))))
  (fib-iter 1 0 n))
;(fib 8)

;exercise 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* (f (- n 2)) 2) (* (f (- n 3)) 3))))
;(f 6)

(define (f1 n)
  (define (f-iter m)
    (if (<  n 3)
        n
        (+ (f (- m 1)) (* (f (- m 2)) 2) (* (f (- m 3)) 3))))
  (f-iter n))
;(f1 6)

;(define f (cons 1 (cons 2 (cons 3 '()))))
;(map (lambda (x) (/ x x)) f)

;exercise 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      (car lst)
      (last-pair (cdr lst))))
(last-pair (list 23 72 149 34))

;exercise 2.18
(define (reverse1 lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst)) (list (car lst)))))
(reverse1 '(1 2 3 4))