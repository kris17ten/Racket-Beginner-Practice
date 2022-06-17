#lang racket

;---------------------------------------------------
; Define a mutable structure for a library.
; It must have readers, books and borrowings.
; Populate the structure.
; -------
; Write a function that takes the library structure 
;  defined above and returns the set of readers 
;  who are currently borrowing a book.
; -------
; Write a function that takes the library structure
;  defined above and returns the set of books
;  that are currently available (not borrowed).
; --------------------------------------------------


(struct reader (name) #:mutable #:transparent)
(struct book (title author) #:transparent)
(struct borrowing (bk readr) #:mutable #:transparent)

(define readers
  (list (reader "Giuseppe")
        (reader "Nikos")
        (reader "Franco")
        (reader "Martin")
        (reader "Bob")
        (reader "Ed")
        (reader "Leo")
        (reader "Michael")
        (reader "Jaap")
        (reader "Barny")))

(define books
  (list (book "Anna Karenina" "abc")
        (book "Kritik der reinen Vernunft" "def")
        (book "The Little Schemer" "ghi")
        (book "The Art of Electronics" "jkl")
        (book "The C++ Programming language" "mno")
        (book "Computational Fluid Dynamics" "pqr")))

(define borrowings
  (list (borrowing "Kritik der reinen Vernunft" "Giuseppe")
        (borrowing "The Art of Electronics" "Michael")))

(define library (list readers books borrowings))

; lst->lst
(define get-rdr1 (λ (a)
                  (cond
                    ((empty? a) a)
                    ((= (length a) 1) (list (borrowing-readr (first a))))
                    (#t (append (get-rdr1 (rest a)) (list (borrowing-readr (first a))))))))
(define get-rdr (λ (lib)
                  (get-rdr1 (third lib))))

(define get-bks1 (λ (c)
                  (cond
                    ((empty? c) c)
                    ((= (length c) 1) (list (book-title (first c))))
                    (#t (append (get-bks1 (rest c)) (list (book-title (first c))))))))
(define get-bks (λ (d)
                  (get-bks1 (second d))))






