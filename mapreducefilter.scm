(define (square x) (* x x))

(define (square-all items)
    (if (null? items)
      '()
      (cons (square (car items))
            (square-all (cdr items)))))

(square-all '(1 2 -3 4))

((lambda (x) (+ x 1) ) 4)

;dette er slik map fungerer, tar inn en prosedyre og en liste
(define (map1 proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst))
                  (map proc (cdr lst)))))
(map1 (lambda (x) (+ x 1)) (list 1 2 3))

;slik filter fungerer, tar inn et predikat og en liste
(define (filter1 pred lst)
  (cond ((null? lst) '())
        ((pred (car lst) (cons (car lst) (filter1 pred (cdr lst)))))
        (else (filter1 pred (cdr lst)))))

;(filter (lambda (x) (= (/ x 2))))

;slik reduce fungerer
(define (reduce1 init op lst)
  (if (null?)
      init
      (op (car lst) (reduce1 init op (cdr lst)))))

;lage map med reduce
(define (map2 proc lst)
  (reduce '() (lambda (x y) (cons (proc x) y)) lst))

  
  