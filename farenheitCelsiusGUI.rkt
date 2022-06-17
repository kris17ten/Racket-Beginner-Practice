#lang racket
(require racket/gui)

; ----------------------------------
; Farenheit to Celsius conversion
;  with GUI
; ----------------------------------

(define f-to-c (λ (f) (/ (* 5 (- f 32)) 9)))

(define myframe (new frame% [label "My Window"]
                     [width 200] [height 200]))
(define fahren (new text-field%
                    [label "Farenheit"]
                    [parent myframe]
                    [init-value "32"]))
#|(new button% [parent myframe]
     [label "Convert F to C"]
     [callback (λ (o e) (send centi set-value
                              (real->decimal-string
                               (f-to-c (string->number
                                        (send fahren get-value))))))])
|#
(define hpan1 (new horizontal-panel% [parent myframe]))
(define centi (new message%
                   [label "Celsius:"]
                   [parent hpan1]))
(define val (new message%
                 [label "0"]
                 [parent hpan1]))

(send val set-label
      (real->decimal-string
       (f-to-c (string->number
                (send fahren get-value)))))
(send myframe show #t)