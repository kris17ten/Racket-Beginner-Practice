#lang racket

; --------------------------------------------------------
; Write a function, using recursion that adds up the 
;  numbers from a given number. For example, sumfrom(10)
;  returns 10 + 9 + 8 + 7 + 6 + 5 + 4 + 3 + 2 + 1 = 55
; --------------------------------------------------------

(define sumfrom (λ (n)
                  (cond
                    ((= n 1) 1)
                    (#t (+ n (sumfrom (- n 1)))))))



; --------------------------------------------------------
; Using the same pattern as before, produce a function
;  for factorial. This is just the numbers from n to 1
;  multiplied together.
; --------------------------------------------------------

(define factorial (λ (n)
                    (cond
                      ((= n 1) 1)
                      (#t (* n (factorial (- n 1)))))))



; --------------------------------------------------------
; Using recursion, write a function to add up all the 
;  numbers in a list.
; --------------------------------------------------------

(define sumlist (λ (x)
                  (cond
                    ((empty? x) 0)
                    (#t (+ (first x) (sumlist (rest x)))))))



; --------------------------------------------------------
; Using recursion, write a function to remove all the odd
; numbers in a list.
; --------------------------------------------------------

(define remove-if-odd (λ (y)
                        (cond
                          ((empty? y) '())
                          ((odd? (first y)) (remove-if-odd (rest y)))
                          (#t (cons (first y) (remove-if-odd (rest y)))))))



; --------------------------------------------------------
; Making the test a variable, write a function that
;  removes anything if it passes the test.
; --------------------------------------------------------

(define remove-if (λ (test a)
                    (cond
                      ((empty? a) '())
                      ((test (first a)) (remove-if test (rest a)))
                      (#t (cons (first a) (remove-if test (rest a)))))))

(define fn (λ (b) (cond
                    ((number? b) (cond
                                   ((positive? b) "positive")
                                   ((negative? b) "negative")
                                   (#t "zero")))
                                 (#t "invalid input"))))



; --------------------------------------------------------
; Using recursion, write a counter function to count
;  the odd numbers in the list.
; --------------------------------------------------------

(define count-if-odd1 (λ (c counter)
                        (cond
                          ((empty? c) counter)
                          ((odd? (first c)) (count-if-odd1 (rest c) (+ counter 1)))
                          (#t (count-if-odd1 (rest c) counter)))))


; --------------------------------------------------------
; Further simplify the count-if-odd function, starting
;  the counter at 0.
; --------------------------------------------------------

(define count-if-odd (λ (x) (count-if-odd1 x 0)))
