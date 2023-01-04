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

(define (draw-VM-Info info)
  (js-call (js-eval "drawVMInfo") info))


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


(define (compile exp env set next)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp next))
        ((quoted? exp) (compile-quoted exp next))
        ((assignment? exp) (compile-assignment exp env set next))
        ((variable? exp) (compile-variable exp env set next))
        ((definition? exp) (compile-definition exp))
        ((let? exp) (compile-let exp env next))
        ((if? exp) (compile-if exp env set next)) 
        ((lambda? exp) (compile-lambda exp env set next))
        ((application? exp)
         (compile-application exp env set next))
        (else (error "Unknown expression type -- COMPILE" exp))))

;=============tagged-list========================

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (variable? exp)
  (symbol? exp))
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (let? exp)
  (tagged-list? exp 'let))
(define (if? exp)
  (tagged-list? exp 'if))
(define (lambda? exp)
  (or (tagged-list? exp 'lambda)
      (tagged-list? exp 'slambda)
      (tagged-list? exp 'clambda)))
(define (application? exp)
  (pair? exp))


;===========compiler-process=====================
(define (compile-self-evaluating val next)
  (list 'constant val next))
(define (compile-quoted exp next)
  (list 'constant (cadr exp) next))
(define compile-variable
  (lambda (exp env set next)
    (compile-refer exp env (if (set-member? exp set)
                               (list 'indirect next)
                               next))))

(define compile-refer
  (lambda (exp env next)
    (compile-lookup exp env
                    (lambda (n m) (list 'refer n m next))
                    (lambda (n) (list 'refer-free n next))
                    (lambda () (list 'refer-global exp next)))))


(define compile-lookup
  (lambda (x e return return-free return-global)
    (let ((free (cdr e)))
      (let nxtrib ((e (car e)) (rib 0) (ccb #f))
        (if (null? e)
            (return-global)
            (let nxtelt ((vars (car e)) (n 0) (ccb ccb))
              (cond ((null? vars) (nxtrib (cdr e) (+ rib 1) ccb))
                    ((eq? vars 'CB) (nxtrib (cdr e) rib #t))
                    ((eq? (car vars) x)
                     (if ccb
                         (return-free (- (length free) (length (memq x free))))
                         (return rib n)))
                    (else (nxtelt (cdr vars) (+ n 1) ccb)))))))))
(define (compile-if exp env set next)
  (let ((test (cadr exp)) (then (caddr exp)) (else (cadddr exp)))
    (let ((thenc (compile-then then env set next))
          (elsec (compile-else else env set next)))
      (compile-test test env set (list 'test thenc elsec)))))

(define (compile-test test env set next)
  (compile test env set next))

(define (compile-then then env set next)
  (compile then env set next))

(define (compile-else else env set next)
  (compile else env set next))

(define (compile-definition exp)
  (let ((c (run (caddr exp))))
    (cond ((assq (cadr exp) the-global-environment)
           => (lambda (p) (set-cdr! p c)) )
          (else
           (set! the-global-environment (cons (cons (cadr exp) c)
                                              the-global-environment))
           ))))
(define (compile-assignment exp env set next)
  (let ((var (cadr exp)) (exp (caddr exp)))
    (compile-lookup var env
                    (lambda (n m)
                      (compile  exp env set (list 'assign n m next)))
                    (lambda (n)
                      (compile  exp env set (list 'assign-free n next)))
                    ;;;
                    (lambda ()
                      (compile  exp env set (list 'assign-global var next))))))

(define (compile-lambda exp env set next)
  (cond [(eq? (car exp) 'slambda) (compile-slambda exp env set next)]
        [(eq? (car exp) 'clambda) (compile-clambda exp env set next)]
        [else (error 'unknown_lambda)]))
(define (compile-slambda exp env set next)
  (let ((vars (cadr exp)) (body (caddr exp)))
    (let ((free (remove-global (car env) (find-free body vars)))
          (sets (find-sets body vars))) 
      (list 'functional
            (make-boxes sets `(*link* ,@vars) 
                        (compile body
                                 (cons (compile-extend (car env) vars)
                                       (cdr env))
                                 (set-union sets (set-intersect set free))
                                 (list 'return (+ (length vars) 1))))
            next))))
(define (compile-clambda exp env set next)
  (let ((vars (cadr exp)) (body (caddr exp)))
    (let ((free (remove-global (car env) (find-free body vars)))
          (sets (find-sets body vars)))
      (collect-free free env (list 'close
                                   (length free)
                                   (make-boxes sets vars
                                               (compile body
                                                        (cons (compile-extend (compile-extend (car env) 'CB) vars) ;;;
                                                              free)
                                                        (set-union sets (set-intersect set free))
                                                        (list 'return
                                                              (length vars))))
                                   next)))))

(define (compile-let exp env set next)
  (let ((e (let->lambda exp)))
    (compile e env set next)))

(define (compile-application exp env set next)
  (let loop ((args (cdr exp))
             (c (compile-fun-body exp env set)))
    (if (null? args)
        (list 'frame
              next
              (compile-arg c))
        (loop (cdr args)
              (compile (car args)
                       env
                       set
                       (list 'argument c))))))

(define (compile-fun-body exp env set)
  (compile (car exp) env set '(apply)))
(define (compile-arg c) c)

;==========helping-function=========================
(define compile-extend
  (lambda (e r)
    (cons r e)))
(define (remove-global e free) ;;;
  (define (flatten l)
    (cond ((null? l) l)
          ((not (pair? l)) (list l))
          (else (append (flatten (car l)) (flatten (cdr l))))))
  (set-intersect (flatten e) free))
(define find-free
  (lambda (x b)
    (cond
      ((symbol? x) (if (set-member? x b) '() (list x)))
      ((pair? x)
       (case (car x)
         ((quote) '())
         ((lambda slambda clambda)
          (let ((vars (cadr x))
                (body (caddr x)))
            (find-free body (set-union vars b))))
         ((if)
          (let ((test (cadr x))
                (then (caddr x))
                (els (cadddr x)))
            (set-union (find-free test b)
                       (set-union (find-free then b)
                                  (find-free els b)))))
         ((call/cc)
          (let ((exp (cadr x))) (find-free exp b)))
         (else
          (let next ((x x))
            (if (null? x)
                '()
                (set-union (find-free (car x) b)
                           (next (cdr x))))))))
      (else '()))))
(define collect-free
  (lambda (vars e next)
    (if (null? vars)
        next
        (collect-free (cdr vars) e
                      (compile-refer (car vars) e
                                     (list 'argument next))))))
(define find-sets
  (lambda (x v)
    (cond
      ((symbol? x) '())
      ((pair? x)
       (case (car x)
         ((quote) '())
         ((lambda slambda clambda)
          (let ((vars (cadr x))
                (body (caddr x)))
            (find-sets body (set-minus v vars))))
         ((if)
          (let ((test (cadr x))
                (then (caddr x))
                (els (cadddr x)))
            (set-union (find-sets test v)
                       (set-union (find-sets then v)
                                  (find-sets els v)))))
         ((set!)
          (let ((var (cadr x))
                (x (caddr x)))
            (set-union (if (set-member? var v) (list var) '())
                       (find-sets x v))))
         ;((call/cc)
         ; (let ((exp (cadr x))) (find-sets exp b)))
         (else
          (let next ((x x))
            (if (null? x)
                '()
                (set-union (find-sets (car x) v)
                           (next (cdr x))))))))
      (else '()))))
(define (make-boxes sets vars next)
  (make-boxes-loop vars 0 sets next ))
(define (make-boxes-loop vars n sets next )
  (if (null? vars)
      next
      (if (set-member? (car vars) sets)
          (make-box-helping vars n sets next)
          (make-boxes-loop (cdr vars) (+ n 1) sets next ))))
(define (make-box-helping vars n sets next)
  (list 'box n (make-boxes-loop (cdr vars) (+ n 1) sets next )))
(define getargs
  (lambda (args ret)
    
    (if (null? args)
        ret
        (getargs (cdr args) (append ret (list (car (car args))))))))
(define getvals
  (lambda (args ret)
    (if (null? args)
        ret
        (getvals (cdr args) (append ret (list (cadr (car args))))))))
(define let->lambda
  (lambda (exp)
    (let* ((args (cadr exp))
           (body (caddr exp))
           
           (lambda-args (getargs args '()))
           (lambda-vals (getvals args '())))

      (cons (list 'lambda
                  lambda-args
                  (if (eq? (car body) 'let)
                      (let->lambda body)
                      body))
            lambda-vals))))     
;(cons (list 'lambda lambda-args body) lambda-vals))))                                   

;=========set-function===========
(define set-member?
  (lambda (x s)
    (cond
      ((null? s) #f) ;;;
      ((eq? x (car s)) #t) ;;;
      (else (set-member? x (cdr s))))))
(define set-cons
  (lambda (x s)
    (if (set-member? x s)
        s
        (cons x s))))
(define set-union
  (lambda (s1 s2)
    (if (null? s1)
        s2
        (set-union (cdr s1) (set-cons (car s1) s2)))))
(define set-minus
  (lambda (s1 s2)
    (if (null? s1)
        '()
        (if (set-member? (car s1) s2)
            (set-minus (cdr s1) s2)
            (cons (car s1) (set-minus (cdr s1) s2))))))
(define set-intersect
  (lambda (s1 s2)
    (if (null? s1)
        '()
        (if (set-member? (car s1) s2)
            (cons (car s1) (set-intersect (cdr s1) s2))
            (set-intersect (cdr s1) s2)))))

;==========preprocess==========

(define (preprocess x funpos?)
  (cond ((symbol? x) x)
        ((pair? x)
         (case (car x)
           ((quote) x)
           ((lambda)
            `(,(if funpos? 'slambda 'clambda)
              ,(cadr x)
              ,@(map (lambda (e) (preprocess e #f)) (cddr x)))) 
           ((if)
            `(if ,(preprocess (cadr x) #f)
                 ,(preprocess (caddr x) funpos?)
                 ,(preprocess (cadddr x) funpos?)))
           ((set!)
            `(set! ,(cadr x) ,(preprocess (caddr x) #f)))
           (else
            `(,(preprocess (car x) #t)
              ,@(map (lambda (x) (preprocess x #f))
                     (cdr x))))))
        (else x)))

(define (sc exp)
  (compile (preprocess exp #f) '(() . ()) '() '(halt)))


;f:frame pointer
;c:closure pointer
(define VM
  (lambda (a x f c s) ;; (a x e s)
    (case (car x)
      ((halt) a)
      ((refer) (VM-refer a x f c s))
      ((refer-free) (VM-refer-free a x f c s))
      ((refer-global) (VM-refer-global a x f c s))      
      ((indirect) (VM-refer-indirect a x f c s))
      ((constant) (VM-constant a x f c s))
      ((functional) (VM-functional a x f c s))
      ((close) (VM-close a x f c s)) 
      ((box) (VM-box a x f c s))
      ((test) (VM-test a x f c s))
      ((assign) (VM-assign a x f c s))
      ((assign-free) (VM-assign-free a x f c s))
      ((assign-global) (VM-assign-global a x f c s))
      ((frame) (VM-frame a x f c s))
      ((argument) (VM-argument a x f c s))
      ((apply) (VM-apply a x f c s))
      ((return) (VM-return a x f c s))
      (else (VM-otherwise a x f c s)))))

(define (VM-refer a x f c s)
  (let* ((n (cadr x))
         (m (caddr x))
         (x (cadddr x))
         (l (find-link n f))
         (v (anime-index l m)))
    (VM v x f c s)))

(define (VM-refer-free a x f c s)
  (let ((n (cadr x))
        (x (caddr x)))
    (VM (index-closure c n) x f c s)))

(define (VM-refer-global a x f c s)
  (let ((var (cadr x))
        (x (caddr x)))
    (VM (refer-global-var var) x f c s)))

(define (VM-refer-indirect a x f c s)
  (let ((x (cadr x)))
    (VM (unbox a) x f c s)))

(define (VM-constant a x f c s)
  (let ((obj (cadr x))
        (x (caddr x)))
    (VM obj x f c s)))   

(define (VM-functional a x f c s)
  (let ((body (cadr x))
        (x (caddr x)))
    (VM (functional body f) x f c s)))

(define (VM-close a x f c s)
  (let ((n (cadr x))
        (body (caddr x))
        (x (cadddr x)))
    (VM (closure body n s) x f c (- s n))))

(define (VM-box a x f c s)
  (let* ((n (cadr x))
         (x (caddr x))
         (b (box (index s n))))
    (VM-box-helping s n b)
    (VM a x f c s)))

(define (VM-box-helping s n b)
  (index-set! s n b))

(define (VM-test a x f c s)
  (let ((then (cadr x))
        (els (caddr x)))
    (VM a (if a then els) f c s)))

(define (VM-assign a x f c s)
  (let* ((n (cadr x))
         (m (caddr x))
         (x (cadddr x))
         (b (index (find-link n f) m))
         (obj a))
    ;(console-log (index (find-link n f) m))(newline)
    (VM-assign-helping b obj)
    (VM a x f c s)))

(define (VM-assign-helping box obj) (set-box! box obj))

(define (VM-assign-free a x f c s)
  (let* ((n (cadr x))
         (x (caddr x))
         (b (index-closure c n))
         )
    ;(set-box! (index-closure c n) a)
    (VM-assign-helping b a)
    (VM a x f c s)))

(define (VM-assign-global a x f c s)
  (let ((var (cadr x))
        (x (caddr x)))
    (VM (assign-global-var var a) x f c s)))

(define (VM-frame a x f c s)
  (let ((ret (cadr x))
        (x (caddr x)))
    (VM a x f c (push ret (push f (push c s))))))

(define (VM-argument a x f c s)
  (let ((x (cadr x)))
    (VM a x f c (push a s))))

(define (VM-apply a x f c s)
  (case (car a)
    ((functional) ;;;
     (VM-apply-functional a x f c s))
    ((closure) ;;;
     (VM a (closure-body (cdr a)) s (cdr a) s)) ;;;
    ((primitive) ;;;
     (VM-apply-primitive a x f c s))
    (else
     (error "Not a function -- VM-apply"))))

(define (VM-apply-functional a x f c s)
  (let ((body (cadr a))
        (link (caddr a)))
    (VM a body s c (push link s))))

(define (VM-apply-primitive a x f c s)
  (let ((s (+ s 1)) ;; (push link s)
        (primfun (cadr a)))
    (primfun s)))

(define (VM-return a x f c s)
  (let ((n (cadr x)))
    (let ((s (- s n)))
      (VM a (index s 0) (index s 1) (index s 2) (- s 3)))))

(define (VM-otherwise a x f c s)
  (error "Unknow VM instruction -- VM-otherwise"))
 
(define functional
  (lambda (body e)
    (list 'functional body e))) ;;;

(define (primitive-fun natfun) ;;;
  (list 'primitive natfun))

(define (box obj) (cons obj '()))

(define (unbox box) (car box))

(define (set-box! box obj) (set-car! box obj))

(define (assign-global-var var a)
  (cond ((assq var the-global-environment)
         => (lambda (p) (set-cdr! p a)))
        (else
         (error "Unbound variable"))))

(define stack (make-vector 1000))

(define push
  (lambda (x s)
    (vector-set! stack s x)
    (+ s 1)))

(define index
  (lambda (s i)
    (vector-ref stack (- (- s i) 1))))

(define (anime-index-bak s i)
  ;(console-log "i: ") (console-log i) (newline)
  (anime-vm-index (- (- s i) 1))
  (view-narration-addsubactnarration 'act-index)
  (make-subjumppoint-vm)
  (vector-ref stack (- (- s i) 1)))

(define (anime-index s i)
  (anime-vm-index (- (- s i) 1))
  (vector-ref stack (- (- s i) 1)))

(define index-set!
  (lambda (s i v)
    (vector-set! stack (- (- s i) 1) v)))

(define closure
  (lambda (body n s)
    (let ((v (make-clo body n s)))
      (cons 'closure v)))) ;;;
  
(define (make-clo body n s)
  (let ((v (make-vector (+ n 1))))
    (vector-set! v 0 body)
    (let f ((i 0))
      (unless (= i n)
        (vector-set! v (+ i 1) (index s i))
        (f (+ i 1))))
    v))

(define closure-body
  (lambda (c)
    (vector-ref c 0)))

(define index-closure
  (lambda (c n)
    (vector-ref c (+ n 1))))

(define find-link-bak
  (lambda (n e)
    
    (anime-vm-findlink e)
    
    ;(console-log "finding link e: ") (console-log e) (newline)
    (if (= n 0) 
        (begin 
          ;(view-narration-addsubactnarration 'act-findlink-done)
          (make-subjumppoint-vm)
          e
          )
        (begin
          ;(view-narration-addsubactnarration 'act-findlink-next)
          (make-subjumppoint-vm)
          (find-link (- n 1) (index e -1)))))
  )

    
(define (find-link n e)
  (anime-vm-findlink e)
  (if (= n 0)
      (find-link-done e)
      (find-link-next n e)))

(define (find-link-done e) e)
(define (find-link-next n e)
  (find-link (- n 1) (index e -1)))    




(define (prim-return retval s)
  (VM retval (index s 0) (index s 1) (index s 2) (- s 3)))

(define the-global-environment ;;;
  `((+ . ,(primitive-fun (lambda (s)
                           (let ((ans (+ (index s 1)
                                         (index s 2))))
                             (prim-return ans (- s 3))))))
    (- . ,(primitive-fun (lambda (s)
                           (let ((ans (- (index s 1)
                                         (index s 2))))
                             (prim-return ans (- s 3))))))
    (* . ,(primitive-fun (lambda (s)
                           (let ((ans (* (index s 1)
                                         (index s 2))))
                             (prim-return ans (- s 3))))))
    (/ . ,(primitive-fun (lambda (s)
                           (let ((ans (/ (index s 1)
                                         (index s 2))))
                             (prim-return ans (- s 3))))))
    (= . ,(primitive-fun (lambda (s)
                           (let ((ans (= (index s 1)
                                         (index s 2))))
                             (prim-return ans (- s 3))))))
    (eq? . ,(primitive-fun (lambda (s)
                             (let ((ans (eq? (index s 1)
                                             (index s 2))))
                               (prim-return ans (- s 3))))))
    (< . ,(primitive-fun (lambda (s)
                           (let ((ans (< (index s 1)
                                         (index s 2))))
                             (prim-return ans (- s 3))))))
    (<= . ,(primitive-fun (lambda (s)
                            (let ((ans (<= (index s 1)
                                           (index s 2))))
                              (prim-return ans (- s 3))))))
    (> . ,(primitive-fun (lambda (s)
                           (let ((ans (> (index s 1)
                                         (index s 2))))
                             (prim-return ans (- s 3))))))
    (>= . ,(primitive-fun (lambda (s)
                            (let ((ans (>= (index s 1)
                                           (index s 2))))
                              (prim-return ans (- s 3))))))))

(define (refer-global-var var) ;;;
  (cond ((assq var the-global-environment)
         => (lambda (p) (cdr p)))
        (else
         (error "unbound symbol"))))

(define run
  (lambda (exp)
    (VM '() (compile (preprocess exp #f) '(() . ()) '() '(halt)) 0 '() 0)))
(define run-vm
  (lambda (exp)
    (VM '() exp 0 '() 0)))
;; ===============================================
;; ===============================================
;; ===============================================
;; ===============================================
;; ===============================================
(define exec
  (lambda (exp env)
    (call/cc
     (lambda (quit)
       (cond ((label? exp) (eval-label exp env))
             ((self-evaluating? exp) (eval-self-evaluating exp))
             ((variable? exp) (eval-variable exp env))
             ((quoted? exp) (eval-quotation exp))
             ((assignment? exp) (eval-assignment exp env))
             ((definition? exp) (eval-definition exp env))
             ((let? exp) (eval-let exp env))
             ((if? exp) (eval-if exp env))
             ((lambda? exp) (eval-lambda exp env))
             ((application? exp) (eval-application exp env))
             (else (error "Unknown expression type -- EXEC" exp)))))))

(define (label? exp)
  (and (list? exp)
       (string? (car exp))))
(define (eval-label exp env)
  (send-label (car exp))
  (exec (cadr exp) env))

(define eval-extend 
  (lambda (env vars vals)
    (cons (cons vars vals) env)))
(define (eval-lookup-bak var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (cdr env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env '())
        (error "Unbound variable" var)
        (let ((frame (car env)))
          (scan (car frame)
                (cdr frame)))))
  (env-loop env))


(define (eval-lookup var env)
  (eval-lookup-env var env 0))

(define (eval-lookup-env var env n)
  (view-narration-newnarration "-1")
  (cond ((null? env) (error  "Unbound variable" ))
        (else (eval-lookup-frame var env (car (car env)) (cdr (car env)) n 0))))

(define (eval-lookup-frame-bak var env vars vals n m)
  (anime-eval-lookup (get-env-id env) m)
  (view-narration-addsubactnarration 'eval-lookup)
  (cond ((null? vars)
         (begin
           (view-narration-addsubactnarration 'eval-lookup-diff-fra)
           (make-subjumppoint-eval)
           (eval-lookup-env var (cdr env) (+ n 1))))
        ((eq? var (car vars))
         (begin
           (view-narration-addsubactnarration 'eval-lookup-same-var)
           (make-subjumppoint-eval)
           (car vals)))
        (else (begin
                (view-narration-addsubactnarration 'eval-lookup-diff-var)
                (make-subjumppoint-eval)
                (eval-lookup-frame var env (cdr vars) (cdr vals) n (+ 1 m))))))


(define (eval-lookup-frame var env vars vals n m)
  (anime-eval-lookup (get-env-id env) m)
  (cond ((null? vars) (eval-lookup-frame-diff-fra var env n))
        ((eq? var (car vars)) (eval-lookup-frame-same-var vals))
        (else (eval-lookup-frame-diff-var var env vars vals n m))))


(define (eval-lookup-frame-diff-fra var env n)
  (eval-lookup-env var (cdr env) (+ n 1)))
(define (eval-lookup-frame-same-var vals)
  (car vals))
(define (eval-lookup-frame-diff-var var env vars vals n m)
  (eval-lookup-frame var env (cdr vars) (cdr vals) n (+ 1 m)))


(define GE
  (eval-extend
   '()
   '(+ - * / = eq? < <= > >= console-log) ; <- ( + - * / )
   (map (lambda (f) (cons 'primitive f))
        (list + - * / = eq? < <= > >= console-log))))  ; <- ( + - * / )

;==========解析用函数==========

(define (eval-self-evaluating exp) exp)
(define (eval-quotation) (cadr exp))
(define (eval-variable exp env) (eval-lookup exp env))
(define (eval-assignmen1t exp env)
  (let ((var (cadr exp))
        (val (caddr exp)))
    (set-car! (eval-lookup var env) (exec val env))))

(define (eval-assignment exp env) ;处理标签版
  (let ((var (cadr (cadr exp))) ;外侧的cadr是用来去掉标签的
        (val (exec (caddr exp) env)))
    (eval-assignment-set-env! var val env)))

(define (eval-assignment-set-env! var val env)
  (define (loop-env env)
    (cond ((null? env) (error 'var_not_found))
          (else (loop-frame (car (car env)) (cdr (car env)) env))))
  (define (loop-frame vars vals env)
    (cond ((null? vars) (loop-env (cdr env) ))
          ((eq? (car vars) var) (eval-assignment-set-env!-helping vals val var env))
          (else (loop-frame (cdr vars) (cdr vals) env))))
  (loop-env env))

(define (eval-assignment-set-env!-helping vals val var env)
  (console-log (get-env-id env))
  (view-environment-changeglobalvariable (get-env-id env) (symbol->string var) val)
  (set-car! vals val))

(define (eval-definition exp env) ;处理标签版
  (if (not (symbol? (cadr (cadr exp)))) ;使用cadr去掉外侧的标签
      (error "Not a variable -- DEFINE"))
  (define-variable! (cadr  (cadr exp)) (exec (caddr exp) env)))
;默认这个define是全局变量的定义

(define (define-variable! var val)

  (let ((glo-var (car (car GE)))
        (glo-val (cdr (car GE))))
    (let ((new-glo-var (append glo-var (list var)))
          (new-glo-val (append glo-val (list val))))
      (set-car! GE (cons new-glo-var new-glo-val)))))

(define (eval-if exp env)
  (let ((test (cadr exp))
        (then (caddr exp))
        (else (cadddr exp)))
    (if (eval-if-test test env)
        (eval-if-then then env)
        (if (null? else)
            #f
            (eval-if-else else env)))))
(define (eval-if-test test env) (exec test env))
(define (eval-if-then then env) (exec then env))
(define (eval-if-else else env) (exec else env))
(define (eval-lambda-bak exp env)
  (let ((vars (cadr exp))
        (body (caddr exp)))
    (list 'function vars body env)))

(define (eval-lambda exp env)
  (cond [(eq? (car exp) 'slambda) (eval-slambda exp env)]
        [(eq? (car exp) 'clambda) (eval-clambda exp env)]
        [else (error 'unknown_lambda)]))

(define (eval-slambda exp env)
  (let ((vars (cadr exp))
        (body (caddr exp)))
    (list 'function vars body env)))

(define (eval-clambda exp env)
  (let ((vars (cadr exp))
        (body (caddr exp))) ;vars-body list
    ;; draw make-closure 
    (list 'function vars body env)))

(define (eval-clambda-helping-fun exp)
  (let ((vars (cadr exp))
        (body (caddr exp)))
    (list vars body)))

; (define (eval-application exp env)
;   (let ((args (eval-application-args (cdr exp) env))
;         (body (eval-application-body (car exp) env)))
;     (eval-application-apply body args env)))

(define syn-counter-inte 1)
(define (eval-application exp env) ;为了同步,在原函数上进行了修改
  (let ((syn syn-counter-inte))
    (set! syn-counter-inte (+ 1 syn-counter-inte))
    (let ((args (eval-application-args (cdr exp) env))
          (body (eval-application-body (car exp) env)))
      (eval-application-apply body args env syn))))

(define (eval-application-args args env)
  (let ((ret (reverse (map (lambda(x) (exec x env)) (reverse args)))))
    
    ret
    ))

(define (eval-application-body name env) (exec name env))

; (define (eval-application-apply func arguments env) ;
;   (case (car func)
;     ((primitive)
;      (eval-application-apply-primitive func arguments))
;     ((function)
;     (let ((f (caddr func))
;           (e (eval-extend (cadddr func) (cadr func) arguments)))
;       (eval-application-apply-functional f e func arguments)))
;     (else (error "Not a function -- eval-application-apply"))))

(define (eval-application-apply func arguments env syn) ;;为了同步,在原函数上进行了修改
  (case (car func)
    ((primitive)
     (eval-application-apply-primitive func arguments))
    ((function)
     (let ((f (caddr func))
           (e (eval-extend (cadddr func) (cadr func) arguments)))
       (eval-application-apply-functional f e func arguments syn)))
    (else (error "Not a function -- eval-application-apply"))))

; (define (eval-application-apply-functional exp env func arguments)
;   (exec exp env))
(define (eval-application-apply-functional exp env func arguments syn)
  (exec exp env))

(define (eval-application-apply-primitive func args)
  (apply (cdr func) args))

(define (eval-let exp env)
  (let ((e (let->lambda exp)))
    (exec e env)))

(define (glo-define exp)
  (let ((cur-v VM-break-switch)
        (cur-i interpreter-break-switch))
    (set-vm-break #f)
    (sc exp)
    (set-vm-break cur-v)
    (set-inte-break #f)
    (eval1 exp)
    (set-inte-break cur-i)))


;(define (eval1 exp) (exec (preprocess exp #f) GE))
(define (eval code) (exec code GE))
(define (eval1 exp) (exec (make-label (preprocess exp #f)) GE))

;==========================;==========================;==========================
;==========================;==========tables==========;==========================
;==========================;==========================;==========================

(define closure-table-vm (list))

;向a-list中追加新元素,编号自动维护
(define (add-vm-closure c)
  (let ((ret (cons (cdr c) (length closure-table-vm))))
    (set! closure-table-vm (append closure-table-vm (list ret)))))

;获取编号
(define (get-vm-closure-number c table)
  (cond ((null? table) -1)
        ((eq? (car (car table)) c) (cdr (car table)))
        (else (get-vm-closure-number c (cdr table)))))

;生成闭包字符串 无前缀closure
(define (make-vm-clo-name c)
  (let ((num (get-vm-closure-number c closure-table-vm)))
    (if (eq? num -1)
        ;(error "bad_closure -- make-inte-clo-name")
        "null"
        (string-append "<clo" (number->string (+ num 1)) ">"))))


;非计算动作标签
(define non-act-table '(eval-application eval-arguments eval-body
                                         eval-if eval-test eval-then eval-else))
;判断act是否为非计算动作标签
(define (is-non-act? act table)
  (cond ((null? table) #f)
        ((eq? act (car table)) #t)
        (else (is-non-act? act (cdr table)))))

(define (make-arg-str num str)
  (string-append "<" str (number->string (+ 1 num)) ">"))

(define (make-short-argument arg)
  (cond ((not (eq? -1 (get-box-num arg box-table)))
         (make-arg-str (get-box-num arg box-table) "box"))
        ((not (eq? -1 (get-inte-closure-number arg closure-table-inte)))
         (make-arg-str (get-inte-closure-number arg closure-table-inte) "clo"))
        (else arg)))


;(define indirect-flag #f)

(define box-table (list))

(define (add-box b)
  (set! box-table (append box-table (list (list b (length box-table))))))

(define (get-box-num b table) 
  (cond ((null? table) -1)
        ((eq? b (car (car table))) (cadr (car table)))
        (else (get-box-num b (cdr table) ))))


;这个变量是用来存放标签序号的
;( (标签名(act-...) 序号) ... )
;eg. ( (act-if 33) (act-then 22) )
;表示下一次碰到if表达式,其标签序列为33
(define vm-act-counter-tabel (list ))

;传入act名就可以获得该动作的编号
;(get-vm-act-counter 'act-if) -> 33
(define (get-vm-act-counter act)
  (letrec ((loop (lambda(tab)
                   (cond ((null? tab)
                          (set! vm-act-counter-tabel
                                (append vm-act-counter-tabel (list (list act 1))))
                          1)
                         ((eq? (car (car tab)) act)
                          (set-cdr! (car tab)
                                    (list (+ 1 (cadr (car tab)))))
                          (cadr (car tab)))
                         (else (loop (cdr tab)))))))
    (loop vm-act-counter-tabel)))

;结构
;[
;  [env , id]
;  [env , id]
;]

; env是该函数的环境,(cadr env)是指向的环境

(define env-table (list (cons GE 0)))

(define (get-env-id env)
  (define (loop table)
    (cond ((null? table) -1)
          ((eq? env (car (car table))) (cdr (car table)))
          (else (loop (cdr table)))))
  (loop env-table))

(define (append-new-env env)
  (set! env-table
        (append env-table
                (list (cons env (length env-table))))))


;解释器用闭包      
;创建一个a-list存放 (闭包.编号)
(define closure-table-inte (list))

;向a-list中追加新元素,编号自动维护
(define (add-inte-closure c)
  (let ((ret (cons c (length closure-table-inte))))
    (set! closure-table-inte (append closure-table-inte (list ret)))))

;获取编号
(define (get-inte-closure-number c table)
  (cond ((null? table) -1)
        ((eq? (car (car table)) c) (cdr (car table)))
        (else (get-inte-closure-number c (cdr table)))))

;生成闭包字符串
(define (make-inte-clo-name c)
  (let ((num (get-inte-closure-number c closure-table-inte)))
    (if (eq? num -1)
        ;(error "bad_closure -- make-inte-clo-name")
        "null"
        (string-append "<clo" (number->string (+ num 1)) ">"))))


;用闭包编号来替代本身的闭包
(define (inplace-arg-by-number args ret)
  (cond ((null? args) ret)
        ((not (eq?  (get-inte-closure-number (car args) closure-table-inte) -1)) ;闭包编号不等于-1,即存在这个闭包
         (inplace-arg-by-number (cdr args)
                                (append ret (list (make-str (get-inte-closure-number (car args) closure-table-inte))))))
        (else (inplace-arg-by-number (cdr args) (append ret (list (car args)))))))
;生成 <clo num> 这样的字符串 其中num是加一表示的,因为是从0开始计数而画面上是从1开始计数的。
(define (make-str num)
  (string-append "<clo" (number->string (+ 1 num)) ">"))

(define (make-inte-str exp ret)
  (define (any->string any)
    (cond ((list? any) (make-inte-str any '()))
          ((symbol? any) (symbol->string any))
          ((number? any) (number->string any))
          (else (error "unknow type -- any->string"))))
  (cond ((not (list? exp)) (any->string exp))
        ((null? exp) ret)
        (else (make-inte-str (cdr exp) (append ret (list (any->string (car exp))))))))

(define label-counter 0)
(define (get-label)
  (set! label-counter (+ 1 label-counter))
  (string-append "L" (number->string label-counter)))

(define (make-label exp)
  (cond
    ((self-evaluating? exp) (list (get-label) exp))
    ;((self-evaluating? exp) (list exp))
    ((symbol? exp)  (list (get-label) exp))
    ;((symbol? exp)  (list exp))
    ((eq? 'define (car exp))
     (list (get-label)
           (list 'define
                 (make-label (cadr exp))
                 (make-label (caddr exp)))))
    ((eq? 'set! (car exp))
     (list (get-label)
           (list 'set!
                 (make-label (cadr exp))
                 (make-label (caddr exp)))))
    ((eq? 'if (car exp))
     (let ((test (cadr exp))
           (then (caddr exp))
           (else (cadddr exp)))
       (list (get-label)
             (list 'if
                   (make-label test)
                   (make-label then)
                   (make-label else)))))
    ((or (eq? 'lambda (car exp))
         (eq? 'slambda (car exp))
         (eq? 'clambda (car exp)))
     
     (let ((body (caddr exp))
           (lam (car exp)))
       (list (get-label)
             (list lam
                   (cadr exp)
                   (make-label body)))))
    (else
     (let ((fun (car exp))
           (args (cdr exp)))
       (list (get-label)
             (append (list  (make-label fun))
                     (map (lambda(x) (make-label x)) args)))))))

(define (make-acc-str a)
  (cond ((pair? a)
         (cond ((eq? (car a) 'closure)
                (make-vm-clo-name (cdr a)))))
        (else a)))

;====macro======

(define interpreter-break-switch #t)
(define VM-break-switch #t)
(define (set-vm-break val)
  (set! VM-break-switch val))
(define (set-inte-break val)
  (set! interpreter-break-switch val))

(define (cc)
  (call/cc
   (lambda (quit)
     (set! resume-meta quit)))) 
;(cc)
(define temp-break #f)
(define temp-break-flag #f)
(define (make-temp-break)
  (call/cc (lambda (breakpoint)
             (set! temp-break breakpoint)
             (set! temp-break-flag #t)
             (set! break #t)
             (resume-meta 'ok))))

(define (make-jumppoint-eval)
  (call/cc (lambda (breakpoint)
             (set! exec-k breakpoint)
             (cond (interpreter-break-switch (resume-meta 'ok))))))

(define (make-jumppoint-vm)
  (call/cc (lambda (breakpoint)
             (set! vm-k breakpoint)
             (cond (VM-break-switch (resume-meta 'ok))))))

(define (make-subjumppoint-eval)
  (call/cc (lambda (breakpoint)
             (set! sub-exec-k breakpoint)
             (set! sub-exec-flag #t)
             (if sub-breakpoints
                 (begin (set!  break #t) (resume-meta 'ok))
                 (resume-meta 'ok))
             )))

(define (make-subjumppoint-vm)
  (call/cc (lambda (breakpoint)
             (set! sub-vm-k breakpoint)
             (set! sub-vm-flag #t)
             (if sub-breakpoints
                 (begin (set!  break #t) (resume-meta 'ok))
                 (resume-meta 'ok))
             ;(cond (VM-break-switch (resume-meta 'ok)))
             ;(set! break #t)
             )))

(define-macro define-act-eval
  (lambda (fun info act)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)
              
               (add-indent-inte)      ;增加缩进距离
               (set! inte-info ,act)  ;传递解释器信息
               (cond ((break? inte-info) (set! break #t)))  ;如果在断点名单中,就将break置为真
               (make-jumppoint-eval) 
               (view-narration-newnarration ,info)
               (draw-interpreter-info ,info)
              
               (make-subjumppoint-eval)
               (let ((ret (apply org-fun (cons arg restarg))))

                 (cond ((is-non-act? ,info non-act-table)
                        (draw-interpreter-info (string-append "END-" (symbol->string ,info)  )) ))
                 (view-narration-addsubactnarration 'end)
                 (make-subjumppoint-eval)
                
                 (sub-indent-inte)
                 ret
                 )
               )))))

(define-macro define-sub-act-eval
  (lambda (fun act)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)
               (set! sub-inte-info ,act)
               (make-subjumppoint-eval)
               (draw-sub-inte-info ,act)
               ;(console-log 'sub-eval:)(console-log ,act)(newline)
               (apply org-fun (cons arg restarg)))))))



(define-macro define-sub-eval-info
  (lambda(fun info)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)
               (add-indent-inte)
               (draw-sub-inte-info ,info)
               (sub-indent-inte)
               (apply org-fun (cons arg restarg)))))))

(define-macro define-act-compiler
  (lambda (fun pseins)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)
               (list ,pseins (get-vm-act-counter ,pseins)
                     (apply org-fun (cons arg restarg))))))))


(define-macro define-vm-otherwise
  (lambda ()
    `(let* ((org-fun VM-otherwise))
       (set! VM-otherwise
             (lambda (a x f c s)
               (if (get-act (car x)) ;如果存在于acts的VECTOR里那么就执行,不然就报错
                   (begin
                     
                     (set! vm-info (car x)) ;设置vm-info为当前act的值
                     (cond ((break? vm-info) (set! break #t))) ;判断当前的act是否需要中断
                     ;(make-jumppoint-vm)
                     (view-narration-newnarration (car x))
                     (send-label-vm (car x) (cadr x))
                     (make-jumppoint-vm)
                     ;(make-subjumppoint-vm)
                     (let ((ret (VM a (caddr x) f c s)))
                       ret
                       ))
                    
                   (org-fun a x f c s)))))))

(define-macro define-sub-act-vm
  (lambda (fun act)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)
               (set! sub-vm-info ,act)
               (make-subjumppoint-vm)
               (console-log 'sub-vm:)(console-log ,act)(newline)
               (apply org-fun (cons arg restarg)))))))

(define-macro define-vm-make-clo
  (lambda ()
    `(let* ((org-fun make-clo))
       (set! make-clo
             (lambda (body n s)
               (let ((v (org-fun body n s)))
                 (test-fun v)
                 v
                 )
               )))))

;eval-application (length (cdr (reverse env)))
(define-macro define-eval-application
  (lambda ()
    `(let* ((org-fun eval-application))
       (set! eval-application
             (lambda (exp env)
               (js-call-frame-eval-add (length  env))
               ;(js-call-frame-eval-add)

               (let ((res (org-fun exp env)))
                 (js-call-frame-eval-sub (length  env))

                 res
                 )
              
 
               )))))


(define-macro define-eval-exec
  (lambda ()
    `(let* ((org-fun exec))
       (set! exec
             (lambda (exp env)
               ;(js-draw-expression exp)


               (view-environment-highlightframe (get-env-id env))
               ;(draw-interpreter-exp (make-inte-str exp '()))
               (org-fun exp env)

               )))))


(define-macro define-vm-VM
  (lambda ()
    `(let* ((org-fun VM))
       (set! VM
             (lambda (a x f c s)
               ;(make-subjumppoint-vm)
               (draw-VM-Info (car x))
               (send-acc-info (make-acc-str a))
               ;(draw-draw-VM-exp (make-inte-str x '()))
               (org-fun a x f c s)
               )))))

(define-macro define-vm-box-helping
  (lambda ()
    `(let* ((org-fun VM-box-helping))
       (set! VM-box-helping
             (lambda (s n b)
               (add-box b)
               (view-stack-addbox (- s n 1) (+ 1 (get-box-num b box-table)))
               (view-closure-createbox (car b))
               (org-fun s n b)
               )))))

(define-macro define-vm-refer-indirect
  (lambda ()
    `(let* ((org-fun VM-refer-indirect))
       (set! VM-refer-indirect
             (lambda (a x f c s)
               ;(cond ((eq? (car (cadr x)) 'argument) 
               ;      (set! indirect-flag (+ 1 (get-box-num a box-table)))))   
               (org-fun a x f c s)
               )))))

(define-macro define-vm-assign-helping
  (lambda ()
    `(let* ((org-fun VM-assign-helping))
       (set! VM-assign-helping
             (lambda (box obj)
               (view-closure-changeboxval (+ 1 (get-box-num box box-table))
                                          obj)
               (org-fun box obj)
               )))))




;===============画面相关=================

; (define-macro embed-eval-draw-frame-functional
;   (lambda ()
;     `(let* ((org-fun eval-application-apply-functional))
;        (set! eval-application-apply-functional
;              (lambda (exp env func arguments)

;                 (append-new-env env)
;                 ;因为interpreter-new-frame需要用到arguments 和 func两个参数,所以也将这两个参数传递
;                 ;待修改
;                 (interpreter-new-frame (inplace-arg-by-number arguments '()) func (get-env-id env) (get-env-id (cdr env)) 99)

;                 (org-fun exp env func arguments)

;               )))))
(define-macro embed-eval-draw-frame-functional
  (lambda ()
    `(let* ((org-fun eval-application-apply-functional))
       (set! eval-application-apply-functional
             (lambda (exp env func arguments syn)
               (draw-interpreter-info "environment extended")
               (append-new-env env)
               ;因为interpreter-new-frame需要用到arguments 和 func两个参数,所以也将这两个参数传递
               ;待修改
               (interpreter-new-frame (inplace-arg-by-number arguments '()) func (get-env-id env) (get-env-id (cdr env)) syn)

               (org-fun exp env func arguments syn)

               )))))

(define-macro embed-vm-draw-frame 
  (lambda ()
    `(let* ((org-fun VM-frame))
       (set! VM-frame
             (lambda (a x f c s)
               (js-call-frame-vm-add (trav-link f 1))
               (view-stack-createFrame)
               (view-stack-push (make-vm-clo-name c) "closure")
               (view-stack-push f "frame")
               (view-stack-push (cadr x) "return")
               (org-fun a x f c s))))))


(define-macro embed-vm-draw-arg-push 
  (lambda ()
    `(let* ((org-fun VM-argument))
       (set! VM-argument
             (lambda (a x f c s)
               (view-stack-push (make-short-argument a) "argument")
               (org-fun a x f c s))))))

(define-macro embed-vm-draw-apply-functional 
  (lambda ()
    `(let* ((org-fun VM-apply-functional))
       (set! VM-apply-functional
             (lambda (a x f c s)
               (view-stack-push (caddr a) "link")
               (org-fun a x f c s))))))

(define-macro embed-vm-draw-apply-primitive
  (lambda ()
    `(let* ((org-fun VM-apply-primitive))
       (set! VM-apply-primitive
             (lambda (a x f c s)
               (view-stack-push 0 "empty")
               (org-fun a x f c s))))))

(define-macro embed-vm-draw-return 
  (lambda ()
    `(let* ((org-fun VM-return))
       (set! VM-return
             (lambda (a x f c s)
               (js-call-frame-vm-sub)
               (loop (+ 3 (cadr x)) view-stack-pop)
               (view-stack-deleteframe)
               (org-fun a x f c s))))))

(define-macro embed-vm-draw-prim-return 
  (lambda ()
    `(let* ((org-fun prim-return))
       (set! prim-return
             (lambda (retval s)
               (js-call-frame-vm-sub)
               (loop 6 view-stack-pop) ;3 + 3 c,f,x, arg1,arg2,link
               (view-stack-deleteframe)
               (org-fun retval s))))))

(define-macro embed-vm-draw-make-clo 
  (lambda ()
    `(let* ((org-fun make-clo))
       (set! make-clo
             (lambda (body n s)
               (let ((v (org-fun body n s)))
                 (view-closure-createclosure (list->vector (map (lambda (x) (make-short-argument x)) (vector->list v))))
                 v
                 )
               )))))

(define-macro embed-eval-draw-clambda
  (lambda ()
    `(let* ((org-fun eval-clambda))
       (set! eval-clambda
             (lambda (exp env)
               (let ((ret (org-fun exp env)))
                 (add-inte-closure ret)
                 (view-environment-addclosure (list->vector (list (list->vector (cadr exp)) (caddr exp)))
                                              (get-env-id env))
                 ret
                 )
               )))))

(define-macro embed-vm-draw-close
  (lambda ()
    `(let* ((org-fun VM-close))
       (set! VM-close
             (lambda (a x f c s)
               (loop (cadr x) view-stack-pop)
               (org-fun a x f c s)))))) 

(define-macro embed-vm-closure
  (lambda ()
    `(let* ((org-fun closure))
       (set! closure
             (lambda (body n s)
               (let ((ret (org-fun body n s)))
                 (add-vm-closure ret)
                 ret
                 ))))))



(define-macro embed-eval-draw-define-variable
  (lambda ()
    `(let* ((org-fun define-variable!))
       (set! define-variable!
             (lambda (var val)
             
               (let ((ret (org-fun var val)))
                 (cond ((not (eq?  (get-inte-closure-number val closure-table-inte) -1))
                        (view-environment-addglobalvariable (symbol->string var) (make-str (get-inte-closure-number val closure-table-inte))))
                       (else (view-environment-addglobalvariable (symbol->string var) val)))
                 ret
                 ))))))


;;定义旁白,fun是加入旁白的处理函数
;;nar-code对应的是旁白对象中map里映射的值
;;type是指旁白的类型,1为新旁白,此时旁白页面会被刷新
;;2为追加旁白,此时旁白会被追加近原来的旁白中,页面不会刷新
(define-macro define-narration
  (lambda (fun nar-code break-flag)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)
               (view-narration-addsubactnarration ,nar-code)
               (cond (,break-flag (make-subjumppoint-eval)))
               (apply org-fun (cons arg restarg)))))))


(define (loop n fun)
  (cond ((> n 0) (fun) (loop (- n 1) fun))))

(define (trav-link f n)
  (let ((ele (vector-ref stack f)))
    (cond ((not (number? ele)) n )
          ((eq? f 0) n)
          (else (trav-link ele (+ n 1))))))

          
        

(define-narration eval-self-evaluating 'eval-self-evaluating #t)
(define-narration eval-quotation 'eval-quotation #t)


(define-narration VM-constant 'vm-constant #t)
(define-narration VM-functional 'vm-functional #t) 
(define-narration VM-box 'vm-box #t)
(define-narration VM-close 'vm-close #t)
(define-narration VM-frame 'vm-frame #t)
(define-narration VM-apply 'vm-apply #t)
(define-narration VM-return 'vm-return #f)



(define-sub-eval-info eval-self-evaluating 'self-evaluating)
(define-sub-eval-info eval-quotation 'quotation)


;;def
(define-narration eval-variable 'eval-variable #t)
(define-sub-eval-info eval-variable 'variable)




;定义旁白

;;1 self-evaluating eval






;;1 constant vm



;;2 lambda eval
(define-narration eval-lambda 'eval-lambda #t)
(define-sub-eval-info eval-lambda 'eval-lambda)




;; functional vm


;;find-link
(define-narration find-link-done 'find-link-done #t)
(define-narration find-link-next 'find-link-next #t)

;;anime-index
(define-narration anime-index 'anime-index #t)

;;eval-lookup
(define-narration eval-lookup-frame-diff-fra 'eval-lookup-diff-fra #t)
(define-narration eval-lookup-frame-same-var 'eval-lookup-same-var #t)
(define-narration eval-lookup-frame-diff-var 'eval-lookup-diff-var #t)
(define-narration eval-lookup-frame 'eval-lookup #f)

;;vm-argument
(define-narration VM-argument 'vm-argument #t)

;;VM-frame






;定义伪指令


;; 0 self-evaluating
;(define-act-compiler compile-self-evaluating 'act-constant)


;(define-act-eval eval-self-evaluating 'eval-self-evaluating 'act-constant)


;; 1 quotation
;(define-act-compiler compile-quoted 'act-constant)


;(define-act-eval eval-quotation 'eval-quotation 'act-constant)


;; 2 variable
;(define-act-compiler compile-variable 'act-variable)


;(define-act-eval eval-variable 'eval-variable 'act-variable)


;; 3 if
(define-act-compiler compile-if 'act-if)
(define-act-compiler compile-test 'act-test)
(define-act-compiler compile-then 'act-then)
(define-act-compiler compile-else 'act-else)


(define-act-eval eval-if 'eval-if 'act-if)
(define-act-eval eval-if-test 'eval-test 'act-test)
(define-act-eval eval-if-then 'eval-then 'act-then)
(define-act-eval eval-if-else 'eval-else 'act-else)


;; 4 lambda
;(define-act-compiler compile-lambda 'act-lambda)

;(define-act-eval eval-lambda 'eval-lambda 'act-lambda)


;; 5 application
(define-act-compiler compile-application 'act-application)
(define-act-compiler compile-fun-body 'act-fun-body)
(define-act-compiler compile-arg 'act-args)


(define-act-eval eval-application 'eval-application 'act-application)
(define-act-eval eval-application-args 'eval-arguments 'act-args)
(define-act-eval eval-application-body 'eval-body 'act-fun-body)

;; 6 assignment

(define-act-eval eval-assignment 'eval-assignment 'act-assignment)
(define-act-compiler compile-assignment 'act-assignment)


;; 7 others

(define-vm-otherwise)

(embed-eval-draw-frame-functional)
(embed-vm-draw-frame)
(embed-vm-draw-arg-push)
(embed-vm-draw-apply-functional)
(embed-vm-draw-return)

(embed-vm-draw-apply-primitive)
(embed-vm-draw-prim-return)

(embed-vm-draw-make-clo) 

(embed-eval-draw-clambda)

(define-eval-application)
(embed-vm-draw-close)
(define-eval-exec)
(define-vm-VM)

(embed-eval-draw-define-variable)
(define-vm-box-helping)
(define-vm-refer-indirect)
(define-vm-assign-helping)

(embed-vm-closure)

;(define-sub-act-eval eval-variable 'variable )
(define-sub-act-vm VM-refer 'local-variable)
;====break===



(define sub-breakpoints #f)

(define breakpoints (vector (vector 'act-constant #f)
                            (vector 'act-variable #f)
                            (vector 'act-if #f)
                            (vector 'act-test #f)
                            (vector 'act-then #f)
                            (vector 'act-else #f)
                            (vector 'act-lambda #f)
                            (vector 'act-application #f)
                            (vector 'act-args #f)
                            (vector 'act-fun-body #f)
                            (vector 'act-assignment #f)
                            (vector 'act-box #f)))

(define (get-breakpoint-pos name)
  (let loop ((n 0))
    (cond ((>= n (vector-length breakpoints)) (error 'bad_name))
          ((eq? (vector-ref (vector-ref breakpoints n) 0) name) n)
          (else (loop (+ n 1))))))

(define (breakpoint-switch name)
  (let* ((n (get-breakpoint-pos name))
         (v (vector-ref breakpoints n)))
    (vector-set! v 1 (not (vector-ref v 1)))))

(define (breakpoint-on)
  (set! sub-breakpoints #t)
  (let ((len (vector-length breakpoints)))
    (let loop ((n 0))
      (cond ((>= n len) 'done)
            (else
             (begin
               (vector-set! (vector-ref breakpoints n) 1 #t)
               (loop (+ n 1))))))))

(define (breakpoint-off)
  (set! sub-breakpoints #f)
  (let ((len (vector-length breakpoints)))
    (let loop ((n 0))
      (cond ((>= n len) 'done)
            (else
             (begin
               (vector-set! (vector-ref breakpoints n) 1 #f)
               (loop (+ n 1))))))))

(define (break? name)
  (let* ((n (get-breakpoint-pos name))
         (v (vector-ref breakpoints n)))
    (vector-ref v 1)))

;========meta======


(define exec-k #f)
(define vm-k #f)
(define resume-meta #f)

(define sub-exec-k #f)
(define sub-vm-k #f)

(define sub-exec-flag #f)
(define sub-vm-flag #f)

(define vm-info #f)
(define inte-info #f)

(define sub-inte-info #f)
(define sub-vm-info #f)

(define next #f)
(define break #f)

(define acts (vector (vector 'act-constant 0 0)
                     (vector 'act-variable 0 0)
                     (vector 'act-if 0 0)
                     (vector 'act-test 0 0)
                     (vector 'act-then 0 0)
                     (vector 'act-else 0 0)
                     (vector 'act-lambda 0 0)
                     (vector 'act-application 0 0)
                     (vector 'act-args 0 0)
                     (vector 'act-fun-body 0 0)
                     (vector 'act-assignment 0 0)
                     (vector 'act-box 0 0)))


(define (get-act act-name)
  (define (loop n)
    (cond ((>= n (vector-length acts)) #f)
          ((eq? (vector-ref (vector-ref acts n) 0) act-name) n)
          (else (loop (+ n 1)))))
  (loop 0))

(define (get-target target)
  (cond ((eq? target 'exec) 1)
        ((eq? target 'vm) 2)
        (else (error 'unknow_target))))

(define (get-act-num act-name target)
  (let ((t (get-target target))
        (n (get-act act-name)))
    (vector-ref (vector-ref acts n) t)))

(define (act-add1 act-name target)
  (let* ((t (get-target target))
         (n (get-act act-name))
         (v (vector-ref acts n))
         (org (get-act-num act-name target)))
    (vector-set! v t (+ org 1))))

(define (is-same-position?)
  (if (or (eq? vm-info #f)
          (eq? inte-info #f))
      #f
      (and (eq? vm-info inte-info)
           (eqv? (get-act-num vm-info 'vm)
                 (get-act-num inte-info 'exec)))))



(define (p t)
  (console-log t) (newline))

(define (meta program)
  (set! vm-k #f)
  (set! exec-k #f)
  (let ((inte-code (make-label (preprocess program #f)))
        (VM-code (sc program) ))
  
    (send-program  inte-code VM-code )

    ;总继续，整个meta由该继续控制
    (call/cc
     (lambda (break-meta)

       ;创建顶部继续，用来设置返回点
       (call/cc
        (lambda (top)
          (set! resume-meta top)))

       ;断点继续，当执行返回top之后，判断
       ;break的真假，为真则停止meta，为假
       ;则继续执行
       (call/cc
        (lambda (c)
          (cond ((eq? break #t)
                 (set! break #f)
                 (set! next c)
                 (break-meta 'ok)))))

       ;这里的分支决定了解释器和VM的同步方式
       (cond 

         (sub-exec-flag
          (begin (set! sub-exec-flag #f)
                 (sub-exec-k 'ok)))
         (sub-vm-flag
          (begin (set! sub-vm-flag #f)
                 (sub-vm-k 'ok)))
         ((eq? vm-k #f) (run-vm VM-code))
         ((eq? exec-k #f) (resume-meta (eval inte-code)))
         ((is-same-position?)
          (act-add1 inte-info 'exec)
          (exec-k 'ok))
         (else
          (act-add1 vm-info 'vm)
          (vm-k 'ok)))))))
         
  

;(breakpoint-switch 'act-if)
;(meta4 '((lambda (a) (if a 99 0)) 1))

;(breakpoint-on)
;(breakpoint-off)


;(run '(if 1 2 3))
;(eval1 '(if 1 2 3))

;(run '(+ 1 1))
;(eval1 '(+ 1 1))

;(run '1)
;(eval1 '1)

