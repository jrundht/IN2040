;Oppgave 1; skal lagres som .scm fil
;a - evaluerer til 30, evaluerer først "subuttrykk" deretter hoveduttrykk
;(* (+ 4 2) 5)

;b - dette uttrykket blir feil fordi det forventes (i (5)) en prosedyre med operator og argumenter, men det mangler en operator
;(* (+ 4 2) (5))

;c - dette uttrykket blir feil, fordi det forventes en operator før operandene
;(* (4 + 2) 5)

;d - dette blir riktig, det definerer bar til et tall (22)
;(define bar (/ 44 2))

;e - dette uttrykket benytter variabelen bar som ble definert i uttrykket over - evalueres til 11
;(- bar 11)

;f - dette utrykket blir riktig, evalueres til 12. subuttrykk evalueres først deretter hoveduttrykk
;(/ (* bar 3 4 1) bar)

;Oppgave2
;a
;or - evaluerer uttrykk helt til noe blir true og returnerer dette, i dette tilfellet "paff!" og resten av uttrykket evalueres ikke
;and - evaluerer uttrykk helt til noe evt blir false, og returnerer evt true/false, i dette tilfellet returneres false fordi det første uttrykket er false
;if - evalueres til "poff!" fordi uttrykket (positive? 42) evalueres til true
;Disse uttrykkene viser at or, and og if er special forms ved at ikke hele uttrykket nødvendig vis blir evaluert
;i alle disse uttrykkene er det enten en syntakstisk feil eller en udefinert prosedyre, men uttrykkene kjører like fint.
;Dette er fordi programmet ikke oppdager feilen, da det stopper å evaluere uttrykket før det har kommet så langt

;b
(define (sign1 x)
  (cond ((< x 0) (- 1))
        ((> x 0) 1)
        ((= x 0) 0)))

(define (sign2 x)
  (if(> x 0) 1
     (if(< x 0) (- 1)
        0)))
       
;c

(define (sign3 x)
  (or (and (> x 0) 1) ;dersom x > 0 print 1
      (and (= x 0) 0) ;dersom x = 0 print 0
      (- 1)))         ;eller print -1

;Oppgave3
;a
(define (add1 x) (+ x 1)) ;legger til 1
(define (sub1 x) (- x 1)) ;trekker fra 1

;b
(define (plus x y)
  (if (zero? y)
      x                          ;dersom y er null avslutt rekursjonen
      (plus (add1 x) (sub1 y)))) ;kall på prosedyren igjen med x+1 og y-1

;c
;Prosedyren over gir opphav til en rekursiv prosess, dette kan man se ved at prosedyren gjør rekursive kall som venter på returverdi til
;basistilfellet slår inn, prosessens ressursbehov vokser dermed proporsjonalt med størrelsen på y, siden det er denne
;variabelen som slår inn basistilfellet. I motsetning vil en iterativ prosess holde et konstant mengde informasjon.

;Denne prosedyren er orgås rekursiv, men den gir opphav til en iterativ prosess, fordi det er en konstant
;mengde informasjon den holder styr på
(define (plus1 x y)
  (define (iter sum count)
    (if (zero? count)
        sum
        (iter (add1 sum)
              (sub1 count))))
  (iter x y))

;d

;egen omskriving - siden hjelpeprosedyren nå er definert inn i power-close-to, har den tilgang på variablene b n.
;dermed trenger ikke power-iter å ha noen egne variabler som definerer disse på nytt. Det eneste den trenger er en teller,
;som holder styr på eksponenten.
(define (power-close-to b n e)
  (define (power-iter f)
    (if (> (expt b f) n)
        f
        (power-iter (+ 1 f))))
  (power-iter e))


;e

;Den interne fib-iter prosedyren kan ikke forenkles noe mer. Siden den er avhengig av å bruke n til å telle ned, må den ta inn denne
;som en parameter. I tillegg er den avhengig av de to foregående tallene for å kunne finne det neste.
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n)