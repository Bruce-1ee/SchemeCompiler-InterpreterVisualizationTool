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


;(define (test-val val)
;  (js-call (js-eval "testVal") val))

;(define (test-arg vals args)
;  (js-call (js-eval "newFrame") vals args))

(define (interpreter-new-frame vals args)
  (js-call (js-eval "interNewFrame") vals args))

(define (stack-createFrame)
 (js-invoke (js-ref (js-eval "view") "stack") "createFrame" ))


; view.stack.push(val)
(define (js-push-element-into-stack val)
  (js-invoke (js-ref (js-eval "view") "stack") "push" val))


(define (js-pop-element)
  (js-invoke (js-ref (js-eval "view") "stack") "pop"))

(define (stack-deleteFrame)
 (js-invoke (js-ref (js-eval "view") "stack") "deleteFrame" ))