(define true #t)
(define false #f)
(define (error str . exp)
  (display str)
  (display " ")
  (if (null? exp)
    (display " ")
    (display exp))
  (newline))

;测试用函数，可以将参数val传给js中的testArg
(define (test-fun val)
  (js-call (js-eval "testFun") val))

;绘制新的环境时使用，待修改
(define (interpreter-new-frame vals args frameNum targetNum)
  (js-call (js-eval "interNewFrame") vals args frameNum targetNum))

;view.stack.createFrame()
;插入新的stackFrame
(define (view-stack-createFrame)
 (js-invoke (js-ref (js-eval "view") "stack") "createFrame" ))


;view.stack.push(val type)
;新版push，type为该element的类型
(define (view-stack-push val type)
  (js-invoke (js-ref (js-eval "view") "stack") "push" val type))

;view.stack.pop()
(define (view-stack-pop)
  (js-invoke (js-ref (js-eval "view") "stack") "pop"))

;view.stack.deleteFrame()
(define (view-stack-deleteframe)
 (js-invoke (js-ref (js-eval "view") "stack") "deleteFrame" ))


;view.closure.createClosure(l)
(define (view-closure-createclosure val)
  (js-invoke (js-ref (js-eval "view") "closure") "createClosure" val))

;view.environment.addClosure(l)
(define (view-environment-addclosure val targetNum)
  (js-invoke (js-ref (js-eval "view") "environment") "addClosure" val targetNum))

;drawexpression(exp)
(define (js-draw-expression exp)
  (js-call (js-eval "drawexpression") exp))


;;;
;;; 这些函数用来保持环境和栈的编号的统一
;;;

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


;view.environment.highlightFrame(frameNumber)
(define (view-environment-highlightframe frameNumber)
  (js-invoke (js-ref (js-eval "view") "environment") "highlightFrame" frameNumber))

;drawInterpreterExp(exp)
(define (draw-interpreter-exp exp)
  (js-call (js-eval "drawInterpreterExp") exp))

;drawVMExp(exp)
(define (draw-draw-VM-exp exp)
  (js-call (js-eval "drawVMExp") exp))

;addIndent()
(define (add-indent)
  (js-call (js-eval "addIndent")))

;subIndent()
(define (sub-indent)
  (js-call (js-eval "subIndent")))


(lambda (make)
  (lambda (c1 c2) 
    (((lambda (a1 a2)
      a2) (c1) (c1) ))))
