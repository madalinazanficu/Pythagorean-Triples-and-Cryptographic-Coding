#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (if (null? X)
      0
      (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))


; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply-helper M V myList)
   (if (null? M)
       myList
       (multiply-helper (cdr M) V (append myList (list(dot-product (car M) V))))))

(define (multiply M V)
  (multiply-helper M V null))


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)

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


; Suma pana la k
(define (find_leftmost_helper k sum)
  (if (< k 0)
      sum
      (find_leftmost_helper (sub1 k) (+ (pow 3 k) sum))))

; Formula pentru cel mai din stanga element = sum(3 ^ (k-2) + 1)
(define (find_leftmost level)
  (find_leftmost_helper (- level 2) 1))


; Pozitia elementului, indexata de la stanga la dreapta
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

; Gruparea din care provine grupul curent (intoarcere inapoi pe recursivitate)
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
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)                               
  (cond [(null? Ts) ppt]
        [(equal? (car Ts) 1) (apply-matrix-transformations (cdr Ts) (multiply T1 ppt))]
        [(equal? (car Ts) 2) (apply-matrix-transformations (cdr Ts) (multiply T2 ppt))]
        [(equal? (car Ts) 3) (apply-matrix-transformations (cdr Ts) (multiply T3 ppt))]))
        


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (define Ts (get-transformations n))
  (define ppt '(3 4 5))
  (apply-matrix-transformations Ts ppt))
