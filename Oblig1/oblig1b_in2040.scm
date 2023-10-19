;;Oppgave 1
;se oblig1b_jorundht_in2040.pdf
;(define bar 2)
;f
;(define f '(0 42 #t bar))
;(car (cdr f))

;g
;(define g '((0 42) (#t bar)))
;(car (cdr (car g)))

;h
;(define h '((0) (42 #t) (bar)))
;(car(car(cdr h)))

;i
;(list (list 0) (list 42 #t) (list bar))
;(cons (cons 0 '()) (cons (cons 42 (cons #t '())) (cons (cons bar '()) '())))

;Oppgave 2
;a
;vanlig rekursjon
(define (take n items)
  (if (or (zero? n) (null? items))
      '()
      (cons (car items) (take (- n 1) (cdr items)))))
;(take 3 '(a b c d e f g))
;(take 1 '(a b c d e f g))
;(take 4 '(a b))
;(take 4 '())

;b
;halerekursjon
(define (take1 n items)
  (define (take-iter count items prod) ;prod er den nye listen som inneholder de n første elementene
    (cond ((null? items) prod)
          ((= count n) prod)
          (else (take-iter (+ count 1) (cdr items) (cons (car items) prod)))))
  ;den innebygde reverse er rekursiv, lager en egen som er halerekursiv
  (define (reverse-list items res) 
    (if (null? items)
        res
        (reverse-list (cdr items) (cons (car items) res))))
  ;halerekursjonen gir de n første elementene i motsatt rekkefølge, bruker reverse til å endre dette
  (reverse-list (take-iter 0 items '()) '())) 

;(take1 3 '(a b c d e f g))
;(take1 1 '(a b c d e f g))
;(take1 4 '(a b))
;(take1 4 '())

;c
(define (take-while pred items)
  ;dersom listen er tom eller predikatet ikke stemmer returneres den tomme listen
  (if (or (null? items) (not (pred (car items))))
      '()
      ;ellers legges elementet inn i listen og prosedyren kjøres for neste element
      (cons (car items) (take-while pred (cdr items)))))

;(take-while even? '(2 34 42 75 88 103 250))
;(take-while odd? '(2 34 42 75 88 103 250))
;(take-while (lambda (x) (< x 100)) '(2 34 42 75 88 103 250))

;d
(define (map2 proc items1 items2)
  ;kunne like gjerne brukt if-setning
  (cond ((null? items1) '()) ;sjekker om den første listen er tom
        ((null? items2) '()) ;sjekker om den andre listen er tom
        (else (cons (proc (car items1) (car items2))
            (map2 proc (cdr items1) (cdr items2)))))) ;dersom ingen av dem er tomme
;(map2 + '(1 2 3 4) '(3 4 5))

;e
;bruker map2 til å regne gjennomsnittet av tallene på tilsvarende posisjon i listene
;siden denne gjennomsnitts prosedyren ikke er nødvendig utenfor uttrykket
;kan vi spare plass ved å definere den rett i proseydrekallet til map2
(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5 6 7))









