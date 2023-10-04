(load "huffman.scm")

;; Oppgave1
;; a
(begin (display "Oppgave 1a:") (newline))
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car pair) ;; dette blir proc i p-cons
  (pair (lambda (x y) x)))

(define (p-cdr pair)
  (pair (lambda (x y) y)))
  
(define pair (p-cons "foo" "bar"))
(p-car pair)
(p-cdr pair)
(p-car (p-cdr (p-cons "zoo" pair))) ;; '(zoo foo bar)

;;b
(begin (display "Oppgave 1b:") (newline))
(define foo 42)
(let ((foo 5)
      (x foo))
  (if (= x foo)
      'same
      'different))

;; omskriving med lambda 
((lambda (x foo)
  (if (= x foo)
      'same
      'different)) foo 5) ;;evalueres til different 

(let ((bar foo)
      (baz 'towel))
  (let ((bar (list bar baz))
        (foo baz))
    (list foo bar)))

;; omskriving med lambda
(define baz 'towel) ;;ikke egentlig nødvendig å definere?

((lambda (bar baz foo)
  (list foo (list bar baz)))
 foo 'towel baz) ;;evalueres til (towel (42 towel))

;; c - infix-eval
;; prosedyre som anvender en operator på to operander
(begin (display "infix-eval:") (newline))
(define (infix-eval exp)
  (if (= 3 (length exp))
      ((cadr exp) (car exp) (caddr exp))
      (begin
        (display "Not an expression that can be evaluated")
        (newline))))

(define (infix-eval1 exp) (cadr exp) (car exp) (caddr exp))
(define foo (list 21 + 21))
(define baz (list 21 list 21))
(define bar (list 84 / 2))

(infix-eval '(42)) ;;feilmelding
(infix-eval foo);; -> 42
(infix-eval baz);; -> (21 21)
(infix-eval bar);; -> 42

;; d
;; Når man skriver med quote blir ikke det som følger evaluert
;; det blir en liste av symbolene.
;; Dermed blir ikke '/' evaluert som en operator
;; 84 og 2 blir heller ikke evaluert som tall.
;; (define bah '(84 / 2))
;; (infix-eval bah) 


;; Oppgave2
;; a - halerekusriv decode
(begin (display "decode:") (newline))
(define (decode1 bits tree)
  (define (decode-iter bits current-branch res)
    (if (null? bits)
        res
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-iter (cdr bits) tree (cons (symbol-leaf next-branch) res))
              (decode-iter (cdr bits) next-branch res)))))
  (define (reverse-list items res) ;;reverserer listen 
    (if (null? items)
        res
        (reverse-list (cdr items) (cons (car items) res))))
  (reverse-list (decode-iter bits tree '()) '())) ;;usikker på om jeg kan bruke den innebygde reverse, siden den er rekursiv
        
;; b
(decode1 sample-code sample-tree);; resultat: (samurais fight ninjas by night)
(decode1 sample-code sample-tree) ;; samme resultat

;; c - encode
;; hjelpeprosedyre for å sjekke om symbol er i branch
(begin (display "encode:") (newline))
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (encode msg tree)
  (define (encode-1 symbol branch) ;;kode ett symbol av gangen
    (cond ((or (null? branch) (leaf? branch)) '())
          ((element-of-set? symbol (symbols (left-branch branch)))
           (cons 0 (encode-1 symbol (left-branch branch))))
          (else (cons 1 (encode-1 symbol (right-branch branch))))))
                 
  (define (encode-msg msg tree)
    (if (null? msg)
        '()
        (append (encode-1 (car msg) tree) (encode-msg (cdr msg) tree))))
  (encode-msg msg tree))

(decode1 (encode '(ninjas fight samurais) sample-tree) sample-tree)
(decode1 (encode '(ninjas fight by night) sample-tree) sample-tree)
(decode1 (encode '(ninjas fight ninjas) sample-tree) sample-tree)

;; d - grow-huffman-tree
(begin (display "grow-hufman-tree:") (newline))
(define (grow-huffman-tree lst)
  (define (grower lst)
    (cond ((null? lst) '())
          ((= (length lst) 1) (car lst)) ;;ferdig når det bare er ett element i listen
          (else (grower (cons (make-code-tree (car lst) (cadr lst)) (cddr lst)))))) ;;slå sammen to noder og legg til
  (grower (make-leaf-set lst))) ;;starter med et ordnet set løvnoder
 
(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode1 (encode '(a b c) codebook) codebook)

;; e
(define alfabet '((samurais 57)
                  (ninjas 20) (fight 45)
                  (night 12) (hide 3)
                  (in 2) (ambush 2)
                  (defeat 1) (the 5)
                  (sword 4) (by 12)
                  (assassin 1) (river 2)
                  (forest 1) (wait 1) (poison 1)))

(define code-tree (grow-huffman-tree alfabet))
(encode '(ninjas fight
                 ninjas fight ninjas
                 ninjas fight samurais
                 samurais fight
                 samurais fight ninjas
                 ninjas fight by night) code-tree)
;; bruker 42 bit til å kode meldingen

;; 42/17 = 2.5bits i gjennosnitt

;; for å kode med fast lengde for 16 symboler, trengs log2(16) = 4 bits
;; da trengs det 4*17 = 68 bits for å kode meldingen med fast lengde.

;; f - extract huffman-leaves
(begin (display "extract huffman-leaves:") (newline))
(define (huffman-leaves tree)
  (define (leaf-extractor tree)
    (cond ((null? tree)'())
          ((leaf? tree) (list (list (cadr tree) (caddr tree)))) ;;dersom dette er en løvnode, returner symbol og frekvens
          (else (append (leaf-extractor (left-branch tree)) ;;finn løvnoder til venstre 
                    (leaf-extractor (right-branch tree)))))) ;;finn løvnoder til høyre
  (leaf-extractor tree))
(huffman-leaves sample-tree)