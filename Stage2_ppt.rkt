#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (foldr + 0 (map * X Y)))


; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
(define (multiply M V)
   ; map itereaza in M
   ;=> fiecare element din matrice e o lista
   ; si aplica pe fiecare element functia λ care primeste ca parametru oferit de map => x si param V
   (map (λ (x) (dot-product x V))  M))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.
; Pow function
(define (power n k i)
  (if (equal? i k)
      1
      (* n (power n k (+ i 1)))))

(define (pow n k)
  (power n k 0))


; Nivelul tripletului curent
(define (find_level_helper n sum level)
  (if (>= sum n)
      level
      (find_level_helper n (+ sum (pow 3 level)) (add1 level))))

(define (find_level n)
  (find_level_helper n 0 0))


; Suma dupa k de la 0
(define (find_leftmost_helper k sum)
  (if (< k 0)
      sum
      (find_leftmost_helper (sub1 k) (+ (pow 3 k) sum))))

; Formula pentru cel mai din stanga element de pe nivel = sum(3 ^ (k-2) + 1)
(define (find_leftmost level)
  (find_leftmost_helper (- level 2) 1))


; Al catelea element este din grupare de la stanga la dreapta
(define (find_position n)
  (define level (find_level n))
  (define leftmost (find_leftmost level))
  (+ (- n leftmost) 1))

; Transformarea curenta
(define (find_current_transformation pos)
  (define q (quotient pos 3))  ; catul => gruparea din care face parte
  (define r (modulo pos 3))    ; restul => pozitia elementului curent din grupare => numarul transformarii
  (cond [(= r 1) 1]
        [(= r 2) 2]
        [(= r 0) 3]))

; Gruparea din care provine grupul curent (intoarcere inapoi pe recursivitate in arbore)
(define (find_next_group pos)
   (define q (quotient pos 3))  ; catul => gruparea din care face parte (se poate schimba in functie de rest)
   (define r (modulo pos 3))    ; restul => afecteaza gruparea din care vine
   (if (> r 0)
       (add1 q)
       q))

(define (get-transformations-helper pos level)
  (if (equal? level 1)
      null
      (append (get-transformations-helper (find_next_group pos) (sub1 level)) (list (find_current_transformation pos)))))

(define (get-transformations n)
  (define pos (find_position n))
  (define level (find_level n))
  (get-transformations-helper pos level))


; TODO
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).


(define (apply-functional-transformations Fs tuple)
  ; fold itereaza prin Fs, si aplica functia data ca al doilea param asupra tuple
  (foldl (λ(x tp) (x tp)) tuple Fs))
  

; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.


; Start-tuple poate fi atat un tuplu cat si un cvartet
(define (get-nth-tuple n tuple functions)

  ; tran-list este de tipul '(1 1 2 3 4) si desemneaza ce functie trebuie aplicata
  (define trans-list (get-transformations n))

  (define f1 (car functions))
  (define f2 (cadr functions))
  (define f3 (caddr functions))
  
  ; Iterez prin lista de transformari, si aplic functia aferenta
  (foldl (λ(trans tuple) (cond  [(equal? trans 1)  (f1 tuple)]
                                [(equal? trans 2) (f2 tuple)]
                                [(equal? trans 3) (f3 tuple)])) tuple trans-list))
  

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
;(define (get-nth-ppt-from-matrix-transformations n)


 (define (get-nth-ppt-from-matrix-transformations n)
   (define f1 ((curry multiply) T1))
   (define f2 ((curry multiply) T2))
   (define f3 ((curry multiply) T3))
   (get-nth-tuple n '(3 4 5) (list f1 f2 f3)))
  


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define (get-nth-quadruple n)
   (define f1 ((curry apply) Q1))
   (define f2 ((curry apply) Q2))
   (define f3 ((curry apply) Q3))

   (get-nth-tuple n '(1 1 2 3) (list f1 f2 f3)))
  

; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define (get-nth-ppt-from-GH-quadruples n)
  (define quadruple (get-nth-quadruple n))
  ; a = g * h
  (define a (* (car quadruple) (cadddr quadruple)))
  ; b = 2 * e * f
  (define b (* 2 (cadr quadruple) (caddr quadruple)))
  ; c = e^2 + f^2
  (define c (+ (pow (cadr quadruple) 2) (pow (caddr quadruple) 2)))
  (list a b c))
