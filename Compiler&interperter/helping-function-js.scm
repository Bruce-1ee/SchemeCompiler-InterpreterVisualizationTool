(define true #t)
(define false #f)
(define (error str . exp)
  (display str)
  (display " ")
  (if (null? exp)
    (display " ")
    (display exp))
  (newline))



;(js-eval str) evaluate str as JavaScript code

;(js-ref jsobj str) = a.b

;(js-set! jsobj str value) = a.b = c

;(js-call jsfunc args...) = a()

;(js-invoke jsobj methodname args...) = a.b()



(define (send-stack stack s)
  (js-call (js-eval "updateStack") stack s))

(define (eval-update name expr)
    (js-call (js-eval "updateEvalInfo") name expr))