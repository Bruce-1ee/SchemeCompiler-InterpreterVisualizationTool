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
;-------------------------------------

(define exec-k #f)
(define vm-k #f)
(define resume-meta #f)
(define beginning #f)


(define lst (make-vector 20))
(define lst-vm (make-vector 20))
(define lst-p 0)
(define lst-eval-vm 0)

;=========INTERPRETER=========

(define exec
  (lambda (exp env)
    (call/cc
     (lambda (quit)
       (cond ((self-evaluating? exp)(eval-self-evaluating exp))
             ((variable? exp)(eval-variable exp env))
             ((quoted? exp) (eval-quotation exp))
             ((assignment? exp) (eval-assignment exp env))
             ((definition? exp) (eval-definition exp env))
             ((let? exp) (eval-let exp env))
             ((if? exp) (eval-if exp env))
             ((lambda? exp) (eval-lambda exp env))
             ((begin? exp) (eval-sequence (cdr exp) env)) ;new
             ((application? exp) (eval-application exp env))
             (else (error "Unknown expression type -- EXEC" exp)))))))


(define eval-extend 
  (lambda (env vars vals)
    (cons (cons vars vals) env)))
(define (eval-lookup var env)
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


(define GE
  (eval-extend
   '()
   '(+ - * = display) ; <- ( + - * / )
   (map (lambda (f) (cons 'primitive f))
        (list + - * = display))))  ; <- ( + - * / )

;==========判断用函数==========

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (if? exp) (tagged-list? exp 'if))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (application? exp) (pair? exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (let? exp)
  (tagged-list? exp 'let))

;==========new==========
(define (begin? exp) (tagged-list? exp 'begin))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (exec (first-exp exps) env))
        (else (exec (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))




;==========解析用函数==========

(define (eval-self-evaluating exp) exp)
(define (eval-quotation) (cadr exp))
(define (eval-variable exp env) (eval-lookup exp env))
(define (eval-assignment exp env) (let ((var (cadr exp))
        (val (caddr exp)))
    (set-car! (eval-lookup var env) (exec val env))))

(define (eval-definition exp env)
  (if (not (symbol? (cadr exp)))
      (error "Not a variable -- DEFINE"))
      (define-variable! (cadr exp) (exec (caddr exp) env) env)
  'ok)
(define (define-variable! var val env)
  (let ((frame (car env)))
    (define (scan vars vals)
      (cond ((null? vals) (set-car! frame (cons var (car frame))) (set-cdr! frame (cons val (cdr frame))))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (car frame) (cdr frame))))

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
(define (eval-lambda exp env)
  (let ((vars (cadr exp))
        (body (caddr exp)))
    (list 'function vars body env)))

(define (eval-application exp env)
  
  (let ((arg (map (lambda(x) (eval-application-arg x env)) (cdr exp)))
        (body (eval-application-body (car exp) env)))
    ;(display (caddr body))(newline)
    (eval-application-apply body arg env)))
(define (eval-application-arg arg env) (exec arg env))
(define (eval-application-body name env) (exec name env))

(define (eval-application-apply func arguments env)
  (case (car func)
    ((primitive)
     ;biwaschemeFunction
     (send-arguments-to-js arguments arguments (cdr func) 0)
     (eval-application-apply-primitive func arguments))
    ((function)
     ;biwaschemeFunction
     ;(send-arguments-to-js (cadr func) arguments (caddr func) 1)
     (exec (caddr func)
           (eval-extend (cadddr func) (cadr func) arguments)))
    (else (error "Not a function -- eval-application-apply"))))
(define (eval-application-apply-primitive func args)
  (apply (cdr func) args))


(define (eval-let exp env)
  (let ((e (let->lambda exp)))
        (exec e env)))

;==========new==========
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
    (cons (list 'lambda lambda-args body) lambda-vals))))




;=========COMPILER=========

(define SS 30) ;stackSize 10

(define (compile exp env next)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp next))
        ((quoted? exp) (compile-quoted exp next))
        ((variable? exp) (compile-variable exp env next))
        ((definition? exp) (compile-definition exp env next))
        ((let? exp) (compile-let exp env next))
        ((if? exp) (compile-if exp env next))
        ((lambda? exp) (compile-lambda exp env next))
        ((application? exp)
         (compile-application exp env next))
        (else (error "Unknown expression type -- COMPILE" exp))))

(define (compile-self-evaluating exp next) (list 'constant exp next))
(define (compile-quoted exp next) (list 'constant (cadr exp) next))
(define (compile-variable exp env next)
  (compile-lookup
   exp env 
   (lambda (n m)(compile-variable-local n m next))
   (lambda (var)(compile-varable-global var next))))
(define (compile-variable-local n m next)(list 'refer n m next))
(define (compile-varable-global var next)(list 'refer-global var next))

(define (compile-definition2 exp env next) ;definition
  (if (null? env)
            ;全局变量
            (cond    
              ((number? (caddr exp)) (set! GE (cons (cons (cadr exp)(cons 'constant (caddr exp))) GE)) (list 'ok))
              ((symbol? (caddr exp)) (set! GE (cons (cons (cadr exp) (find-global (caddr exp))) GE)) (list 'ok))
              (else  (set! GE (cons (cons (cadr exp) (cons 'function (list (compile (caddr exp) env (list 'return 0))))) GE)) (list 'ok)))
            ;局部变量
            (compile (caddr exp) 
                     (begin (set-car! env (reverse (cons (cadr exp) (reverse (car env))))) env)
                     (list 'argument next))))

(define (compile-definition exp env next)
  (let ((var (cadr exp))
        (val (caddr exp)))
   (compile val env (list 'definition var next))))

(define (compile-if exp env next)
  (let* ((test (cadr exp))
         (then (caddr exp))
         (else (cadddr exp))
         (thenc (compile-if-then then env next))
         (elsec (compile-if-else else env next)))
    (compile-if-test test env next thenc elsec)))
(define (compile-if-then then env next) (compile then env next))
(define (compile-if-else else env next) (compile else env next))
(define (compile-if-test test env next then else)
  (compile test env (list 'test then else)))
    
(define (compile-lambda exp env next)
  (let* ((vars (cadr exp))
         (body (caddr exp))
         (ret
          (list 'functional
                (compile body
                         (extend-compiler env vars)
                         (list 'return (+ (length vars) 1)))
                next)))
    ret))
(define (compile-application exp env next)
  (letrec ((loop (lambda (args c)
                         (if (null? args)
                             (compile-application-frame next c)
                             (loop (cdr args)
                                   (compile-application-argument (car args) env c))))))
                                   ;;(compile (car args) env (list 'argument c))
          (loop (cdr exp) (compile-application-name exp env next))))
(define (compile-application-frame next c) (list 'frame next c))
(define (compile-application-name exp env next) (compile (car exp) env (list 'apply (length (cdr exp)))))
(define (compile-application-argument arg env c) (compile arg env (list 'argument c)))
  
(define (compile-lookup var e return-local return-global)
  (letrec
      ((nxtrib
        (lambda(e rib)
          (if (null? e)
              (return-global var)
              (begin 
                (letrec
                    ((nxtelt
                      (lambda(vars elt)
                        (cond
                          [(null? vars) (nxtrib (cdr e) (+ rib 1))]
                          [(eq? (car vars) var) (return-local rib elt)]
                          [else (nxtelt (cdr vars) (+ elt 1))]))))
                  (nxtelt (car e) 0)))))))
    (nxtrib e 0)))


;==========new==========

(define (compile-begin exp env next)
  (letrec ((loop (lambda(exps)
                   (if (null? (cdr exps))
                       (compile (car exps) env next)
                       (compile (car exps) env (loop (cdr exps)))))))
    (loop (cdr exp))))

(define (compile-let exp env next)
  (let ((e (let->lambda exp)))
    (compile e env next)))


;==========VM==========


(define VM

  (lambda (a x e s)
    ;(display "a:")(display a)(newline)
    ;(display "x:")(display x)(newline)
    ;(display "e:")(display e)(newline)
    ;(display "s:")(display s)(newline)
    ;(display "s:")(display stack)(newline)(newline)
    (case (car x)
      [(halt)  (vm-halt a x e s)]
      [(empty) (vm-empty a x e s)]
      [(refer) (vm-refer a x e s)]
      [(refer-global) (vm-refer-global a x e s)]
      [(constant) (vm-constant a x e s)]
      [(close) (vm-close a x e s)]
      [(functional) (vm-functional a x e s)]
      [(test) (vm-test a x e s)]
      [(frame) (vm-frame a x e s)]
      [(argument) (vm-argument a x e s)]
      [(apply) (vm-apply a x e s)]
      [(return) (vm-return a x e s)]
      [else (vm-else a x e s)])))

(define (vm-halt a x e s) a)
(define (vm-empty a x e s) 'ok)

(define (vm-refer a x e s)
  (let ((n (cadr x))
        (m (caddr x))
        (x (cadddr x)))
    (VM (index (find-link n e) m) x e s)))

(define (vm-refer-global a x e s)
  (let ((var (cadr x))
        (x (caddr x)))
    (VM (find-global var GE) x e s)))

(define (vm-constant a x e s)
  (let ((obj (cadr x))
        (x (caddr x)))
    (VM obj x e s)))

;待修改
(define (vm-close a x e s)
  (let ((body (cadr x))
        (x (caddr x)))
    (VM (closure body e) x e s)))

(define (vm-functional a x e s)
  (let ((body (cadr x))
        (x (caddr x)))
    (VM (closure body e) x e s)))

(define (vm-test a x e s)
  (let ((then (cadr x))
        (else (caddr x)))
    (VM a (if a then else) e s)))

(define (vm-frame a x e s)
  (let ((ret (cadr x))
        (x (caddr x)))
    ;(display "frame") (newline)
    (add-frame-counter)
    (VM a x e (push ret (push e s)))))

(define (vm-argument a x e s)
  (if (pair? (car x))
      (VM a (cadr x) e (push (cadr a) s))
      (VM a (cadr x) e (push a s))))

;未修改
(define (vm-apply a x e s)
    (cond
      [(eq? 'primitive (car a))
       ;(display "apply") (newline)
       (sub-frame-counter) ;自动删除
       (letrec ((loop (lambda(argl n)
                        (cond
                          [(= n 0) (VM (apply (cdr a) argl)
                                       (index (- s  (cadr x)) 0)
                                       (index (- s  (cadr x)) 1)
                                       (- (- s (cadr x)) 2))]
                          [else  (loop (cons (index s (- n 1)) argl) (- n 1))]))))
         (loop '() (cadr x)))]

      [(eq? 'function (car a))
       (if (eq? 'frame (car (cadr a)))
           (VM a (cadr a) e s)
           (VM a (cadr (cadr a)) e (push e s)))]
      [else
       (VM a (car a) s (push (cadr a) s))]))

(define (vm-return a x e s)
  ;(display "return") (newline)
  (sub-frame-counter) ;自动删除
  (let ([s (- s (cadr x))])
         (VM a (index s 0) (index s 1) (- s 2))))
  


;==========helping-function==========

(define index ;找到s向下偏移i位置的元素
  (lambda (s i)
    (vector-ref stack (- (- s i) 1))))
(define index-set!
  (lambda (s i v)
    (vector-set! stack (- (- s i) 1) v)))
(define (find-link n e)
  ;(display "n:   ")(display n)(newline)
  ;(display "e:   ")(display e)(newline)
  (if (= n 0) e
      (find-link (- n 1) (index e -1))))
(define closure
  (lambda (body e)
    (list body e)))
(define extend-compiler
  (lambda (e r)
    (cons r e)))

(define (primitive-apply fun n s)
  (letrec ( (loop (lambda (num argl)
                    (if (= num 0)
                        (apply fun argl)
                        (loop (- 1 num) (index s num))))))
    (loop n '())))

(define (find-global var env)
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


(define stack (make-vector SS))
(define (push x s)
  (vector-set! stack s x)
  (+ s 1))

(define (vm-else a x e s)
  (error "Unknow instruction -- VM"))

(define (vm-else-with-breakpoint a x e s)
      (cond
        ((eq? (car x) 'act-constant) (VM-info-output a x e s))
        ((eq? (car x) 'act-refer) (VM-info-output a x e s))
        ((eq? (car x) 'act-lambda) (VM-info-output a x e s))
        ((eq? (car x) 'act-if) (VM-info-output a x e s))
        ((eq? (car x) 'act-test) (VM-info-output a x e s))
        ((eq? (car x) 'act-then) (VM-info-output a x e s))
        ((eq? (car x) 'act-else) (VM-info-output a x e s))
        ((eq? (car x) 'act-argument) (VM-info-output a x e s))
        ((eq? (car x) 'act-function-call) (VM-info-output a x e s))
        ((eq? (car x) 'act-body) (VM-info-output a x e s))
        (else (error "Unknow instruction -- VM"))))
(define (VM-info-output a x e s)
  ;(display (car x)) (display " -- VM") (newline)
  ;(display x) (newline)
  ;(display stack) (newline)
  ;(display a) (newline)(newline)(newline)
  (send-stack stack s)
  (make-breakpoint-vm)
  (VM a (cadr x) e s))

;==========entery==========

(define run
  (lambda (x)
    (VM '() (compile x '() '(halt)) '()  0)))

(define (sc x)
  (compile x '() '(halt)))

(define (eval exp) (exec exp GE))

;==========控制程序==========

(define (eval-output info)
  (display info) (display "-- eval") (newline))

(define (make-breakpoint-eval)
  (call/cc (lambda (breakpoint)
             (set! exec-k breakpoint)
             (resume-meta 'ok))))

(define (make-breakpoint-vm)
  (call/cc (lambda (breakpoint)
             (set! vm-k breakpoint)
             (resume-meta 'ok))))

(define (pause c)
    (call/cc (lambda (k)
             (set! resume-meta k)
             (c))))

(define (meta exp)
  (define (small-step)
    (call/cc
     (lambda (halt)
       (call/cc (lambda (k) (set! beginning k)))
       
       (call/cc (lambda (k) (set! resume-meta k)))
       (display "new step -- meta") (newline)
       (pause halt)
       
       (call/cc
        (lambda (k)
          (set! resume-meta k)
          (if (eq? exec-k #f)
              (exec exp '())
              (exec-k 'ok))))
       ;(display "exec -- meta")(newline)
       ;(pause halt)
       
       (call/cc
        (lambda (k)
          (set! resume-meta k)
          (if (eq? vm-k #f)
              (run exp)
              (vm-k 'ok))))
       ;(display "VM -- meta")(newline)
       ;(pause halt)
       (display "generate graphic -- meta")(newline)
       (set! resume-meta beginning))))
  
  (define (dispatch m)
    (cond ((eq? m 'ini) (small-step))
          ((eq? m 'nxt) (resume-meta 'ok))
          (else (error "message error -- meta -- dispatch"))))
  dispatch)

(define p (meta '(if 1 2 3)) )
;(p 'ini)

;(f 'nxt)

(define (debuger)
  (call/cc
   (lambda (quit)
     (call/cc (lambda (k) (set! resume-meta k))))))    
(debuger)



;==========define-act-macro==========

(define-macro define-act-eval
  (lambda (fun inst)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)
               (eval-output ,inst)
               ;(eval-update ,inst arg)
               (make-breakpoint-eval)
               (apply org-fun (cons arg restarg)))))))

(define-macro define-act-compile
  (lambda (fun inst)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)
               (list ,inst
                     (apply org-fun (cons arg restarg))))))))

(define-macro define-act-vm-1
  (lambda (fun inst)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)

               (make-breakpoint-eval)
               (apply org-fun (cons arg restarg)))))))

(define-macro define-act-vm
  (lambda ()
    `(set! vm-else
           vm-else-with-breakpoint)))


;==========lst=================


(define (gen-vec ins proc)
  (let ((v (make-vector 3)))
    (vector-set! v 0 ins)
    (vector-set! v 1 proc)
    (vector-set! v 2 true)
    (vector-set! lst lst-p v)
    (set! lst-p (+ lst-p 1))))

(define (exist? ins p)
  (if (< p lst-p)
      (if (eq? (vector-ref (vector-ref lst p) 0) ins)
          true
          (exist? ins (+ 1 p)))
      false))

(define (set-proc ins p)
  (if (< p lst-p)
      (if (eq? (vector-ref (vector-ref lst p) 0) ins)
            (vector-set! (vector-ref lst p) 2 (not (vector-ref (vector-ref lst p) 2)))
            ;(vector-ref (vector-ref lst p) 1)
          (set-proc ins (+ 1 p)))
      false))

(define (get-vec ins p)
  (if (< p lst-p)
      (if (eq? (vector-ref (vector-ref lst p) 0) ins)
          (vector-ref lst p)
          (get-vec ins (+ 1 p)))
      (error "no such ins")))

(define-macro act-1 ;可变定义列表
  (lambda (fun ins type)
    `(if (exist? ,ins 0)
         (if (vector-ref (get-vec ,ins 0) 2)
             (begin
               (set! ,fun (vector-ref (get-vec ,ins 0) 1))
               (set-proc ,ins 0) )
             (begin
               (if (eq? ,type 'eval)
                   (define-act-eval ,fun ,ins)
                   (define-act-compile ,fun ,ins))
               (set-proc ,ins 0)))
         (begin 
           (gen-vec ,ins ,fun)
           (if (eq? ,type 'eval)
                 (define-act-eval ,fun ,ins)
                 (define-act-compile ,fun ,ins))))))


        
;==========run define-act==========




;(act-1 eval-self-evaluating "eval-self-evaluating" 'eval)
;(act-1 eval-quotation "eval-quotation" 'eval)
;(act-1 eval-variable "eval-variable" 'eval)
;(act-1 eval-if "eval-if" 'eval)
;(act-1 eval-if-test "eval-if" 'eval)
;(act-1 eval-if-then "eval-if-then" 'eval)
;(act-1 eval-if-else "eval-if-else"   'eval)
;(act-1 eval-lambda "eval-lambda" 'eval)
;(act-1 eval-application "eval-application" 'eval)
;(act-1 eval-application-arg "eval-application-arg" 'eval)
;(act-1 eval-application-body "eval-application-body" 'eval)




(define act
  (lambda ()
    (define-act-compile compile-self-evaluating 'act-constant)
    (define-act-compile compile-quoted 'act-constant)
    (define-act-compile compile-variable-local 'act-refer)
    (define-act-compile compile-varable-global 'act-refer)
    (define-act-compile compile-if-then 'act-then)
    (define-act-compile compile-if-else 'act-else)
    (define-act-compile compile-if-test 'act-if)
    (define-act-compile compile-lambda 'act-lambda)
    (define-act-compile compile-application-frame 'act-function-call)
    (define-act-compile compile-application-argument 'act-argument)
    (define-act-compile compile-application-name 'act-body)

    (define-act-eval eval-self-evaluating "eval-self-evaluating")
    (define-act-eval eval-quotation "eval-quotation")
    (define-act-eval eval-variable "eval-variable")
    (define-act-eval eval-if "eval-if")
    (define-act-eval eval-if-test "eval-if")
    (define-act-eval eval-if-then "eval-if-then")
    (define-act-eval eval-if-else "eval-if-else")
    (define-act-eval eval-lambda "eval-lambda")
    (define-act-eval eval-application "eval-application")
    (define-act-eval eval-application-arg "eval-application-arg")
    (define-act-eval eval-application-body "eval-application-body")

    (let ((org vm-halt))
      (set! vm-halt
            (lambda (a x e s)
              (make-breakpoint-vm)
              (org a x e s))))

    (let ((org vm-apply))
      (set! vm-apply
            (lambda (a x e s)
              ;;biwaschemeFunction
              ;(make-envrionment-frame)  
              (org a x e s))))

    (let ((org eval-application-apply))
      (set! eval-application-apply
            (lambda (func arguments env)
              (org func arguments env)
              (make-envrionment-frame)
              )))

    (let ((org vm-return))
      (set! vm-return
            (lambda (a x e s)
              (make-breakpoint-vm)
              (org a x e s))))
          
    (define-act-vm)))


(define define-act-if
  (lambda ()
    (define-act-compile compile-if-then 'act-then)
    (define-act-compile compile-if-else 'act-else)
    (define-act-compile compile-if-test 'act-if)

    (define-act-eval eval-if-test "eval-if")
    (define-act-eval eval-if-then "eval-if-then")
    (define-act-eval eval-if-else "eval-if-else")

    (define-act-vm)))

(act)





















