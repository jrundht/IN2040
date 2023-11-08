;; OBLIG3A - IN2040 - JORUNDHT

;; Oppgave 1
;; a
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

(load "evaluator.scm")

;; Oppgave 2
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

;; b
(define (install-primitive! name proc)
  ;; Kaller på metode som finnes i evaluator.scm
  (define-variable! name proc the-global-environment))
 

;;(install-primitive! 'square (lambda (x) (* x x)))

;; Oppgave 3a
(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (and (mc-eval (cadr exp) env) (mc-eval (caddr exp) env)))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (or (mc-eval (cadr exp) env) (mc-eval (caddr exp) env)))

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
        ((let? exp) (eval-let exp))))   ;; Oppgave 3b


(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; oppgave 3a
        ((or? exp) #t)  ;; oppgave 3a
        ((let? exp) #t) ;; oppgave 3b
        (else #f)))

;; Oppgave 3b
;; har ikke klart å få til vilkårlig mange elsif
(define-syntax eval-if
  (syntax-rules (else elsif)
    ((eval-if condition body1
         elsif condition1 body2
         else body3)
     (cond (condition body1)
           (condition1 body2)
           (else body3)))))

;; Oppgave 3c
(define (let? exp)
  (tagged-list? exp 'let))

;; hent ut variablene i let-uttrykk rekursivt
(define (let-variables items)
  (if (null? items)
      '()
      (cons (caar items) (let-variables (cdr items)))))

;; hent ut uttrykkene bundet til hver let rekursivt 
(define (let-expressions items)
  (if (null? items)
      '()
      (cons (cadar items) (let-expressions (cdr items)))))

(define (let-body exp) (caddr exp))

;; Gjør om til et lambda-uttrykk,
;; egentlig en liste satt opp som et lambda-uttrykk
(define (transform-let exp)
  (append (list (list 'lambda
               (let-variables (cadr exp)) (let-body exp)))
                    (let-expressions (cadr exp))))

;; Sender lambda-uttrykket til mc-eval
(define (eval-let exp)
  (mc-eval (transform-let exp) the-global-environment))


;;; For å starte read-eval-print loopen og initialisere 
;;; den globale omgivelsen, kjør:
(set! the-global-environment (setup-environment))
(define global the-global-environment)
;;; (read-eval-print-loop)
(define repl read-eval-print-loop)
