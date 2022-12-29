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
(define (interpreter-new-frame vals args frameNum targetNum syn)
  (js-call (js-eval "interNewFrame") vals args frameNum targetNum syn))

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

;view.stack.addBox(eleNum, boxName)
(define (view-stack-addbox n b)
 (js-invoke (js-ref (js-eval "view") "stack") "addBox" n b))


;view.closure.createClosure(l)
(define (view-closure-createclosure val)
  (js-invoke (js-ref (js-eval "view") "closure") "createClosure" val))

;view.closure.createBox(val)
(define (view-closure-createbox val)
  (js-invoke (js-ref (js-eval "view") "closure") "createBox" val))

;view.closure.changeBoxVal(number, val)
(define (view-closure-changeboxval number val)
  (js-invoke (js-ref (js-eval "view") "closure") "changeBoxVal" number val))

;view.environment.addClosure(l)
(define (view-environment-addclosure val targetNum)
  (js-invoke (js-ref (js-eval "view") "environment") "addClosure" val targetNum))

;view.environment.addGlobalVariable(varname varval)
(define (view-environment-addglobalvariable var val)
  (js-invoke (js-ref (js-eval "view") "environment") "addGlobalVariable" var val))

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

(define (draw-interpreter-info info)
  (js-call (js-eval "drawInterpreterInfo") info))

(define (draw-VM-Info info from)
  (js-call (js-eval "drawVMInfo") info from))


;view.environment.highlightFrame(frameNumber)
(define (view-environment-highlightframe frameNumber)
  (js-invoke (js-ref (js-eval "view") "environment") "highlightFrame" frameNumber))

;view.environment.changeGlobalVariable(frameNumber, varName, val)
(define (view-environment-changeglobalvariable frameNumber varName val)
  (js-invoke (js-ref (js-eval "view") "environment") "changeGlobalVariable" frameNumber varName val))

;drawInterpreterExp(exp)
(define (draw-interpreter-exp exp)
  (js-call (js-eval "drawInterpreterExp") exp))

;drawVMExp(exp)
(define (draw-draw-VM-exp exp)
  (js-call (js-eval "drawVMExp") exp))

;addIndentInte()
(define (add-indent-inte)
  (js-call (js-eval "addIndentInte")))

;subIndentInte()
(define (sub-indent-inte)
  (js-call (js-eval "subIndentInte")))

;addIndentInte()
(define (add-indent-vm)
  (js-call (js-eval "addIndentVm")))

;subIndentInte()
(define (sub-indent-vm)
  (js-call (js-eval "subIndentVm")))


;getInteLabel
(define (send-label label)
  (js-call (js-eval "getInteLabel") label))
;getVMLabel
(define (send-label-vm act num)
  (js-call (js-eval "getVMLabel") act num))

;向js发送加入label的代码
;getProgram
(define (send-program inte-p vm-p)
  (js-call (js-eval "getProgram") inte-p vm-p))


;setAccumulatorInfo(info)
(define (send-acc-info info)
  (js-call (js-eval "setAccumulatorInfo") info))

;drawSubInterpreterInfo(info)
(define (draw-sub-inte-info info)
  (js-call (js-eval "drawSubInterpreterInfo") info))

;animeEvalLookup(envNum, varNum)
(define (anime-eval-lookup envNum varNum)
  (js-call (js-eval "animeEvalLookup") envNum varNum))

;animeVmFindlink(target)
(define (anime-vm-findlink target)
  (js-call (js-eval "animeVmFindlink") target))

;animeVmIndex(target)
(define (anime-vm-index target)
  (js-call (js-eval "animeVmIndex") target))

;view.narration.newNarration(key)
(define (view-narration-newnarration key)
  (js-invoke (js-ref (js-eval "view") "narration") "newNarration" key))
  

;view.narration.addSubActNarration(str)
(define (view-narration-addsubactnarration str)
  (js-invoke (js-ref (js-eval "view") "narration") "addSubActNarration" str))

