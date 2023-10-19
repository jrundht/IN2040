;;Oppgave 1

;;a
(begin (display "Oppgave 1a:") (newline))
(define make-counter
  (lambda ()
    (let ((count 0))
      (lambda ()
        (set! count (+ count 1))
        count))))
(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))
(c1)
(c1)
(c1)
count
(c2)

;;b
;;se pdf

;;Oppgave 2

;;a
(begin (display "Oppgave 2a:") (newline))
(define (make-stack stack)
  (define (push! . items)
    (if (not (null? items))
        (set! stack (append (reverse items) stack))))
  (define (pop!)
    (if (not (null? stack)) ;;if stack is empty dont do anything
        (set! stack (cdr stack))))
  (define (dispatch message)
    (cond ((eq? message 'push!) push!)
          ((eq? message 'pop!) pop!)
          ((eq? message 'stack) stack)))
  dispatch)


(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
((s1 'pop!))
(s1 'stack)
((s2 'pop!)) 
((s2 'push!) 1 2 3 4)
(s2 'stack)
((s1 'push!) 'bah)
((s1 'push!) 'zap 'zip 'baz)
(s1 'stack)

;;b
(begin (display "Oppgave 2b:") (newline))
(define (push! stack . items)
  (for-each
   (lambda (item)
     ((stack 'push!) item))
   items))
(define (pop! stack)
  ((stack 'pop!)))
(define (stack a-stack)
  (a-stack 'stack))

(pop! s1)
(stack s1)
(push! s1 'foo 'faa)
(stack s1)

;;Oppgave 3

;;a
(begin (display "Oppgave 3a:") (newline))
(define bar (list 'a 'b 'c 'd 'e))

(set-cdr! (cdddr bar) (cdr bar))
(list-ref bar 0)
(list-ref bar 3)
(list-ref bar 4)
(list-ref bar 5)
;; se pdf for tegning
;; Dette blir en sirkulær liste, der cdr av bar gjentar seg selv i det uendelige
;; derfor blir element 4 lik b igjen, og 5 refererer til c

;;b
(begin (display "Oppgave 3b:") (newline))
(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
bah
(set-car! (car bah) 42)
bah
;; se pdf for tegning
;; bah evalueres til det den gjør fordi (car bah) peker på det elementet som står i (cdr bah).
;; det første elementet i listen bah peker på paret som opprinnelig var (cdr bah), så når (car bah) endres
;; er det egentlig det elementet som (car bah) peker på som endres (som er/var cdr bah)


;;c
(begin (display "Oppgave 3c:") (newline))
(define (cycle? lst)
  (define (check-circular element previous)
    (cond ((null? element) #f) ;;a circular list does not contain the empty list
          ((member element previous) #t) ;;if the element is in the list of previously seen elements it is circular
          (else (check-circular (cdr element) (cons element previous)))))
  (check-circular lst '()))
(cycle? '(hey ho))
(cycle? '(la la la))
(cycle? bah)
(cycle? bar)


;;d
(begin (display "Oppgave 3d:") (newline))
(list? bar)
;;sirkulære lister er ikke lister, fordi de ikke avsluttes med den tomme listen, dermed er ikke bar en liste

(list? bah)
;;bah er en liste som inneholder den tomme listen








           