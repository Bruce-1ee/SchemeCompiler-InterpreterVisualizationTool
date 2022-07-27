(define true #t)
(define false #f)
(define (error str . exp)
  (display str)
  (display " ")
  (if (null? exp)
    (display " ")
    (display exp))
  (newline))

(define (send-stack stack s)
  (js-call (js-eval "updateStack") stack s))

(define (eval-update name expr)
    (js-call (js-eval "updateEvalInfo") name expr))

(define (add-frame-counter)
  (js-call (js-eval "addFrameCounter")))

(define (sub-frame-counter)
  (js-call (js-eval "subFrameCounter")))

(define (make-envrionment-frame)
  (js-call (js-eval "makeEnvrionmentFrame")))

(define (send-arguments-to-js args vals body type)
  (js-call (js-eval "getArgumentsFromScheme") args vals body type))


; view.stack.push(val)
(define (js-push-element-into-stack val)
  (js-invoke (js-ref (js-eval "view") "stack") "push" val))