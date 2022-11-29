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


(define (test-fun val)
  (js-call (js-eval "testFun") val))

;(define (test-arg vals args)
;  (js-call (js-eval "newFrame") vals args))

(define (interpreter-new-frame vals args frameNum targetNum)
  (js-call (js-eval "interNewFrame") vals args frameNum targetNum))

(define (stack-createFrame)
 (js-invoke (js-ref (js-eval "view") "stack") "createFrame" ))


; view.stack.push(val)
(define (js-push-element-into-stack val)
  (js-invoke (js-ref (js-eval "view") "stack") "push" val))

  ; view.stack.push(val type)
(define (js-stack-push val type)
  (js-invoke (js-ref (js-eval "view") "stack") "push" val type))

; view.stack.pushArgument(val)
(define (js-push-argument val)
  (js-invoke (js-ref (js-eval "view") "stack") "pushArgument" val))

(define (js-pop-element)
  (js-invoke (js-ref (js-eval "view") "stack") "pop"))

; view.stack.pushStaticLink(val)
(define (js-push-pushStaticLink val)
  (js-invoke (js-ref (js-eval "view") "stack") "pushStaticLink" val))

(define (stack-deleteFrame)
 (js-invoke (js-ref (js-eval "view") "stack") "deleteFrame" ))


;view.closure.createClosure(l)
(define (js-closure-createClosure val)
  (js-invoke (js-ref (js-eval "view") "closure") "createClosure" val))

;view.environment.addClosure(l)
(define (js-env-addClosure val targetNum)
  (js-invoke (js-ref (js-eval "view") "environment") "addClosure" val targetNum))

;drawexpression(exp)
(define (js-draw-expression exp)
  (js-call (js-eval "drawexpression") exp))


;callFrame.evalCallFunc()
(define (js-call-frame-eval-add num)
  (js-invoke (js-eval "callFrame") "evalCallFunc" num))

;callFrame.evalReturn()
(define (js-call-frame-eval-sub num)
  (js-invoke (js-eval "callFrame") "evalReturn" num))

;callFrame.vmCallFunc()
(define (js-call-frame-vm-add num)
  (js-invoke (js-eval "callFrame") "vmCallFunc" num))

;callFrame.vmReturn()
(define (js-call-frame-vm-sub)
  (js-invoke (js-eval "callFrame") "vmReturn"))

;callFrame.showFrame()
(define (js-call-frame-show)
  (js-invoke (js-eval "callFrame") "showFrame"))



(define (draw-interpreter-info info)
  (js-call (js-eval "drawInterpreterInfo") info))

(define (draw-VM-Info info)
  (js-call (js-eval "drawVMInfo") info))

