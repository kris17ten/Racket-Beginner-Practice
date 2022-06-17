#lang racket
#|
can do with (require racklog) -> Racket Logic thing
FAMILY TREE
               Jane                            Joan
                |                               |
           ------------          ------------------------------
              Stephen            Ruth                   Richard
                           V                               |
         ---------------------------------        ------------------
         Matthew        Ben         Hannah        Becky         John
            |                         |             |
     ---------------               -------      -----------
     Joe      Darren                Mike        Anne   Cece
|#
;motherOf, fatherOf, sisterOf, brotherOf, grandparentOf, cousinOf, ancestorOf
(define member? (λ (c d) (cond
                           ((empty? d) #f)
                           ((equal? c (first d)) #t)
                           (#t (member? c (rest d))))))

(define parentOflst (list (list "Stephen" "Matthew") (list "Stephen" "Ben")
                          (list "Stephen" "Hannah") (list "Ruth" "Matthew")
                          (list "Ruth" "Ben") (list "Ruth" "Hannah")
                          (list "Joan" "Ruth") (list "Joan" "Richard")
                          (list "Jane" "Stephen") (list "Richard" "Becky")
                          (list "Richard" "John") (list "Matthew" "Joe")
                          (list "Matthew" "Darren") (list "Hannah" "Mike")
                          (list "Becky" "Anne") (list "Becky" "Cece")))
(define male (list "Stephen" "Matthew" "Ben" "Richard" "John" "Joe" "Darren" "Mike"))
(define female (list "Joan" "Jane" "Ruth" "Hannah" "Becky" "Anne" "Cece"))

(define parentOf (λ (x) (member? (second x)
                                 (map (λ (y)(first (rest y)))
                                      (filter (λ (y) (equal? (first y) (first x)))
                                              parentOflst)))))

(define motherOf (λ (x)
                   (and (member? (second x)
                                 (map (λ (y)(first (rest y)))
                                      (filter (λ (y) (equal? (first y) (first x)))
                                              parentOflst)))
                        (member? (first x) female))))

(define fatherOf (λ (x)
                   (and (member? (second x)
                                 (map (λ (y)(first (rest y)))
                                      (filter (λ (y) (equal? (first y) (first x)))
                                              parentOflst)))
                        (member? (first x) male))))

(define sisterOf (λ (x)
                   (and (equal? (map (λ (y) (first y))
                                     (filter (λ (y) (equal? (second y) (first x)))
                                             parentOflst))
                                (map (λ (y) (first y))
                                     (filter (λ (y) (equal? (second y) (second x)))
                                             parentOflst)))
                        (member? (first x) female))))

(define brotherOf (λ (x)
                    (and (equal? (map (λ (y) (first y))
                                      (filter (λ (y) (equal? (second y) (first x)))
                                              parentOflst))
                                 (map (λ (y) (first y))
                                      (filter (λ (y) (equal? (second y) (second x)))
                                              parentOflst)))
                         (member? (first x) male))))

(define grandparentOf (λ (x)
                        (let ([chklst (map (λ (y) (second y))
                                           (filter (λ (y) (equal? (first y) (first x)))
                                                   parentOflst))]
                              [othrlst (map (λ (y) (first y))
                                            (filter (λ (y) (equal? (second y) (second x)))
                                                    parentOflst))])
                          (cond
                            ((equal? (length othrlst) 1)
                             (member? (first othrlst) chklst))
                            (#t (or (member? (first othrlst) chklst)
                                    (member? (second othrlst) chklst)))))))

(define cousinOf (λ (x) (let ([chklst (map (λ (y) (first y))
                                           (filter (λ (y) (equal? (second y) (first x)))
                                                   parentOflst))]
                              [othrlst (map (λ (y) (first y))
                                            (filter (λ (y) (equal? (second y) (second x)))
                                                    parentOflst))])
                          (and (not (equal? chklst othrlst))
                               (or (sisterOf (or (list (first chklst) (first othrlst))
                                                 (cond
                                                   ((equal? (length othrlst) 2)
                                                    (list (second chklst) (first othrlst))))))
                                   (brotherOf (or (list (first chklst) (first othrlst))
                                                  (cond
                                                    ((equal? (length othrlst) 2)
                                                     (list (second chklst) (first othrlst)))))))))))


(define displayParent (λ (x)
                 (displayln (map (λ (y) (first y))
                                 (filter (λ (y) (equal? (second y) (first x)))
                                         parentOflst)))
                 (displayln (map (λ (y) (first y))
                                 (filter (λ (y) (equal? (second y) (second x)))
                                         parentOflst)))))

(define ancestorTo2 (λ (x)
                      (map (λ (y)(first (rest y)))
                           (filter (λ (y) (equal? (first y) x)) parentOflst))))
(define ancestorTo1 (λ (x y st)
                      (cond [(equal? x y) #t]
                            [(not (set-member? st x)) #f]
                            [(not (set-member? st y)) #f]
                            [#t (ormap (λ (z)(ancestorTo1 z y
                                                          (set-remove st x)))
                                       (ancestorTo2 x))])))
(define ancestorTo (λ (x y)
                     (ancestorTo1 x y 
                                  (list->set (flatten parentOflst)))))