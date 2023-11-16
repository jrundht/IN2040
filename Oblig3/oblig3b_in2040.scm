;; OBLIG3A - IN2040 - JORUNDHT
(load "evaluator.scm")

;; Oppgave 1a
;; (foo 2 square) -> 0
;; (foo 4 square) -> 16
;; (cond ((= cond 2) 0) (else (else 4))) -> 2

;; Når prosedyren foo anvendes på uttrykkene blir cond gjort om til et if-uttrykk
;; Siden foo har en lokalt definert variabel ved navn cond er det argumentet
;; som sendes med til foo som er cond, altså 2 og 4.
;; Så i første uttrykk evalueres (= cond 2) til true, og 0 returneres.
;; I det andre uttrykket er den lokale variabelen cond = 4,
;; mens den lokale variabelen else her er prosedyren square som kalles
;; med 4 som argument.

;; I det følgende uttrykket (cond ((= cond 2) 0) (else (else 4))) evalueres
;; cond til 3 siden det er en konstant definert i evaluatoren
;; og else evalueres til prosedyren (/ x 2), så resultatet blir 2.

;; Oppgave 2a
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <) ;; lagt til
        (list '> >) ;; lagt til
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
        (list '1+
              (lambda (x) (+ x 1))) ;; Oppgave 2a
        (list '1-
              (lambda (x) (- x 1))) ;; Oppgave 2a
;;      her kan vi legge til flere primitiver.
        ))

;; Oppgave 2b
(define (install-primitive! name proc)
  ;; Kaller på metode som finnes i evaluator.scm
  (define-variable! name
    (list 'primitive proc) the-global-environment))

;; test:
;(install-primitive! 'square (lambda (x) (* x x))) 


;; Oppgave 3a
;; kaller på en metode fra evaluator.scm som sjekker at
;; første element i listen er 'and
(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (cond ((null? exp) #t) ;; hvis det ikke er flere uttrykk var alle andre #t
        ((eq? 'and (car exp))
         (eval-and (cdr exp) env))
        ((not (mc-eval (car exp) env)) #f) ;; Alle må være #t
        (else (eval-and (cdr exp) env))))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (cond ((null? exp) #f)
        ((eq? 'or (car exp))
         (eval-or (cdr exp) env))
        ((mc-eval (car exp) env) #t) ;; trenger bare at en er #t
        (else (eval-or (cdr exp) env))))

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and exp env)) ;; Oppgave 3a
        ((or? exp) (eval-or exp env))   ;; Oppgave 3a
        ((let? exp) (eval-let exp))))   ;; Oppgave 3c/d


(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t)     ;; oppgave 3a
        ((or? exp) #t)      ;; oppgave 3a
        ((let? exp) #t)     ;; oppgave 3c/d
        (else #f)))


;; Oppgave 3b
;; redefinerer eval-if, kan ikke lenger skrive ordinær if-setning
(define (eval-if exp env)
  (cond ((null? exp) '())
        ((or (eq? 'if (car exp)) (eq? 'elsif (car exp)))
         (if (mc-eval (cadr exp) env)
             (mc-eval (cadddr exp) env)
             (eval-if (cdr exp) env)))
        ((eq? 'else (car exp))
         (mc-eval (cadr exp) env))
        (else (eval-if (cdr exp) env))))  


;; Oppgave 3c - vanlig let -> (let ((x 4)) (* x x))
(define (let? exp)
  (tagged-list? exp 'let))


;; Hent ut variablene i let-uttrykk rekursivt (x, y, z,..)
(define (let-variables items)
  (if (null? items)
      '()
      (cons (caar items) (let-variables (cdr items)))))

;; Hent ut uttrykkene bundet til hver let rekursivt 
(define (let-expressions items)
  (if (null? items)
      '()
      (cons (cadar items) (let-expressions (cdr items)))))

(define (let-body exp) (cddr exp))

;; Gjør om til et lambda-uttrykk,
;; egentlig en liste satt opp som et lambda-uttrykk
(define (transform-let exp)
  ;; Til original let - Oppgave 3c (kommenteres ut for å bruke alternativ
  (append (list (append (list 'lambda
                              (let-variables (cadr exp))) (let-body exp)))
          (let-expressions (cadr exp))))

  ;; Til alternativ let - Oppgave 3d
  ;;(append (list (append (list 'lambda
    ;;                          (get-let-variables exp)) (get-let-body exp)))
      ;;    (get-let-expressions exp)))

;; Sender lambda-uttrykket til mc-eval
(define (eval-let exp)
  (mc-eval (transform-let exp) the-global-environment))

;; Oppgave 3d - alternativ let
;; Henter variabelnavn - (x, y, z,..)
(define (get-let-variables items)
  (cond ((or (null? items) (eq? 'in (car items))) '())
        ((eq? '= (cadr items))
         (cons (car items) (get-let-variables (cdr items))))
        (else (get-let-variables (cdr items)))))

;; Henter alle verdier
(define (get-let-expressions items)
  (cond ((or (null? items) (eq? 'in (car items))) '())
        ((or (eq? 'and (cadr items)) (eq? 'in (cadr items)))
         (cons (car items) (get-let-expressions (cdr items))))
        (else (get-let-expressions (cdr items)))))

;; Henter alle uttrykk
(define (get-let-body lst)
  (cdr (memq 'in lst)))

;; test:
;; (let x = 2 and y = 3 in (display (cons x y))(+ x y))

;;; For å starte read-eval-print loopen og initialisere 
;;; den globale omgivelsen, kjør:
;;; (set! the-global-environment (setup-environment))
;;; (define global the-global-environment)
;;; (read-eval-print-loop)
(define repl read-eval-print-loop) ;for å slippe å skrive hele
