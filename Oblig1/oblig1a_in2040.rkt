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

;(sign1 0)

(define (sign2 x)
  (if(> x 0) 1
     (if(< x 0) (- 1)
        0)))

;(sign2 (- 1))
       
;c

(define (sign3 x)
  (or (and (> x 0) 1) ;dersom x > 0 print 1
      (and (= x 0) 0) ;dersom x = 0 print 0
      (- 1)))         ;eller print -1
;(sign3 (- 5))


;Oppgave3
;a
(define (add1 x) (+ x 1)) ;legger til 1
(define (sub1 x) (- x 1)) ;trekker fra 1

;b
(define (plus x y)
  (if (zero? y)
      x                          ;dersom y er null avslutt rekursjonen
      (plus (add1 x) (sub1 y)))) ;kall på prosedyren igjen med x+1 og y-1
;(plus 5 4)

;c
;ikke løst enda

;d
;original
(define (power-close-to b n)
  (power-iter b n 1))
(define (power-iter b n e)
  (if (> (expt b e) n)
      e
      (power-iter b n (+ 1 e))))
(power-close-to 2 8)

;egen omskriving - ikke ferdig
(define (power-close-to2 b n)
  (define (power-iter2 b n e)
    (if (> (expt b e) n)
        e
        (power-iter2 b n (+ 1 e)))))

(power-close-to2 2 8)