(load "prekode3a.scm")
;; Oblig 3a - IN2040 - JORUNDHT

;; Oppgave 1
(display "Oppgave 1a+b:") (newline)

(define org-procs (make-table)) ;; table of all the unmemoized procedures

(define (mem msg proc)
  
    (define memoize
       (let* ((table (make-table)) ;; let* so let is defined sequentially
             (new-proc (lambda args
         (let ((calculated (lookup args table))) ;; check if value already calculated
           (or calculated
               (let ((value (apply proc args)))
                 (insert! args value table) ;; store value in memoized table
                 value)))))) ;; return value
         (insert! new-proc proc org-procs) new-proc)) ;; store unmemoized proc in separate table
  
    (cond ((eq? msg 'memoize) memoize) ;; memoize procedure
          ((eq? msg 'unmemoize) (lookup proc org-procs))));; set the procedure back to one that does not memoize

(set! fib (mem 'memoize fib))

(fib 3)
(fib 3)
(fib 2)
(fib 4)
(set! fib (mem 'unmemoize fib))
(fib 3)
(fib 3)

(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)
(test-proc 42 43 44)


;; Oppgave 1c
;; det er fortsatt den originale fib som brukes, ikke den memoiserte
;; definerer en variabel som holder på verdien utregnet fra (mem-fib 3), men den holder ikke på verdiene
;; for (mem-fib 2) og (mem-fib 1), dersom men ønsker disse, må man eksplisitt be om denne verdien.
;; dette er nok fordi man bruker define istedefor set!.

;; Oppgave 2
(display "Oppgave 2a:") (newline)
(define (list-to-stream items)
  (if (null? items)
      '()
      (cons-stream (car items) (list-to-stream (cdr items)))))
(define lst (list-to-stream '(1 2 3 4)))
lst

(define (stream-length stream) ;; helper to find the length of a stream, used on non-endless streams
  (if (stream-null? stream)
      0
      (+ 1 (stream-length (stream-cdr stream)))))

(define (stream-to-list stream . n) ;; . n -> however many items you want
  (define (recurse res s n)
    (if (or (stream-null? s) (zero? n))
        res
        (recurse (cons (stream-car s) res) (stream-cdr s) (- n 1))))
           
  (if (null? n)
      (reverse (recurse '() stream (stream-length stream))) ;; only streams that are not endless
      (reverse (recurse '() stream (car n)))))
               
(stream-to-list (stream-interval 10 20))
(stream-to-list nats 10)

;; Oppgave 2b
(display "Oppgave 2b:") (newline)
(define (stream-take n stream)
  (define (recurse s res n)
    (if (or (stream-null? s) (zero? n))
        res
        (recurse (stream-cdr s) (cons (stream-car s) res) (- n 1))))
    (list-to-stream (reverse (recurse stream '() n))))
(define foo (stream-take 10 nats))
foo
(show-stream foo 5)
(show-stream foo 20)
(show-stream (stream-take 15 nats) 10)

;; Oppgave 2c
;; Det vil potensielt bli en veldig dyp rekursjon, som kan ende opp med å bruke mye minne.
;; en fordel med strømmer er at man sparer minne, fordi man ikke evaluerer de etterkommende parene før det er behov for dem.
;; Når vi ser etter duplikater må vi evaluere dem, og dermed vil vi bruke masse minne.
;; Dersom vi har en uendelig strøm, vil vi også få en uendelig rekursjon.


;; Oppgave 2d
(display "Oppgave 2d:") (newline)
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
        (cons-stream (stream-car stream)
                     (remove-duplicates (stream-filter
                       (lambda (x) (not (eq? x (stream-car stream)))) ;; make a predicate that checks all other items against the current
                       stream))))) ;;returns stream with duplicates of stream-car removed


(define strm (list-to-stream '(1 2 1 5 2 8 4 5 6 3 8)))
(show-stream strm)
(show-stream(remove-duplicates strm))

(show-stream (remove-duplicates nats))