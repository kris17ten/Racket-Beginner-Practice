#lang racket
(define A '(1 2 3 4 5))
(define B '(2 4 3 1 2))
(define C '(9 6 8 2 3))

; ---------------------------------------------------------
; Write a function that takes a list of numbers
;  and returns the results of addinf the first odd number
;  in the list to the first even number. These may
;  appear in any order.
; ---------------------------------------------------------

(define find-if-odd (λ (a) (cond
                             ((empty? a) #f)
                             ((equal? 1 (length a)) (first a))
                             ((odd? (first a)) (first a))
                             (#t (find-if-odd (rest a))))))
(define find-if-even (λ (b) (cond
                             ((empty? b) #f)
                             ((equal? 1 (length b)) (first b))
                             ((even? (first b)) (first b))
                             (#t (find-if-even (rest b))))))
(define foe (λ (c) (+ (find-if-odd c) (find-if-even c))))



; ---------------------------------------------------------
; Write a function to add the largest number in a list
;  to the smallest number.
; ---------------------------------------------------------

(define find-largest
  (λ (b)
    (cond
      ((empty? b) 0)
      ((equal? 1 (length b)) (first b))
      ((> (first b) (second b))
         (find-largest1 (list (first b) (find-largest (rest b)))))
      (#t (find-largest (rest b)))
         )))
(define find-largest1
  (λ (b)
    (cond
      ((empty? b) 0)
      ((equal? 1 (length b)) (first b))
      ((> (first b) (second b)) (first b))
      (#t (find-largest1 (rest b)))
         )))
(define find-smallest
  (λ (b)
    (cond
      ((empty? b) 0)
      ((equal? 1 (length b)) (first b))
      ((< (first b) (second b))
         (find-smallest1 (list (first b) (find-smallest (rest b)))))
      (#t (find-smallest (rest b)))
         )))
(define find-smallest1
  (λ (b)
    (cond
      ((empty? b) 0)
      ((equal? 1 (length b)) (first b))
      ((< (first b) (second b)) (first b))
      (#t (find-smallest1 (rest b)))
         )))
(define asl (λ (f) (+ (find-largest f) (find-smallest f))))



; ---------------------------------------------------------
; A domino playing programme is being developed. The state
;  of play is represented by a list of pairs. The list
;  ((6 5) (5 4) (4 0)) for example, represents three
;  dominoes in a chain [6--5][5--4][4--0]. Write a function
;  that accepts a domino and a state of play and returns
;  true if the domino can be played; false otherwise.
; ---------------------------------------------------------

(define can-play (λ (a b) (cond
                       ((empty? b) #t)
                       ((member? (or (first a) (last a))
                                 (list (caar b) (last (last b)))) #t)
                       (#t #f))))
(define member? (λ (c d) (cond
                         ((empty? d) #f)
                         ((equal? c (first d)) #t)
                         (#t (member? c (rest d))))))

