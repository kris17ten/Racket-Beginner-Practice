#lang racket
#|
Suppose a man is carrying out a census in some tourist attraction
and needs to answer the question:
Were there more men or more women attending on a given day?
    He only needs to keep track of the difference.

        woman = red
        man = blue

   If man enters:                        If woman enters:
   mt?: push blue                        mt?: push red
   top(blue): push blue                  top(red): push red
   top(red): pop red                     top(blue): pop blue

So, if stack     mt? = equal number
                 top(red): women more
                 top(blue): men more
|#


(define stack-ex '())
(define push (λ (x) (set! stack-ex (cons x stack-ex))))
(define pop (λ () (set! stack-ex (rest stack-ex))))
(define top (λ () (first stack-ex)))

;----------
;SITUATION

(define checkIF (λ (el)
                  (cond
                    ;((not (symbol? el)) "Sorry! Enter valid value.")
                    ((equal? el 'b)
                     (cond
                       ((empty? stack-ex) (push 'b))
                       ((equal? (top) 'b) (push 'b))
                       ((equal? (top) 'r) (pop))))
                    ((equal? el 'r)
                     (cond
                       ((empty? stack-ex) (push 'r))
                       ((equal? (top) 'r) (push 'r))
                       ((equal? (top) 'b) (pop))))
                    (#t "I don't know what to do."))))

(define run-stack (λ (lst)
                    (for ([i lst])
                      (checkIF i))
                    (display stack-ex)
                    ))

(define resultFor (λ (day)
                    (run-stack day)
                    (cond
                      ((empty? stack-ex) (printf "  Both men and women attendees were equal."))
                      ((equal? (length stack-ex) 1)
                       (cond
                         ((equal? (first stack-ex) 'r)
                          (printf
                           "  There was 1 woman more in the attendees than men."))
                         ((equal? (first stack-ex) 'b)
                          (printf
                           "  There was 1 man more in the attendees than women."))
                         (#t "You've made a mistake")))
                       (#t (cond
                             ((equal? (first stack-ex) 'r)
                              (printf (string-append "  There were "
                                                     (number->string (length stack-ex))
                                                     " more women in the attendees than men.")))
                              ((equal? (first stack-ex) 'b)
                               (printf (string-append "  There were "
                                                      (number->string (length stack-ex))
                                                      " more men in the attendees than women.")))
                               (#t "You've made a mistake"))))
                    (set! stack-ex '())
                    ))

(define day1 (list 'r 'r 'b 'r 'r 'b 'b))
(define day2 (list 'r 'b 'r 'r 'r 'b 'r 'b 'b 'r 'b 'r))
(define day3 (list 'r 'b 'r 'b 'b 'b 'r 'r 'r 'b))
(define day4 (list 'b 'b 'b 'r 'b 'b))
(define day5 (list 'b 'r 'b 'b 'r 'b 'b 'r 'b 'r 'b 'r 'b 'r 'b))