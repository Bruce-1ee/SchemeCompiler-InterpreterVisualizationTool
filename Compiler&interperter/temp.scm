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



(define (VM-update stack s)
  (js-call (js-eval "updateStack") stack s))

(define (eval-update name expr)
    (js-call (js-eval "updateEvalInfo") name expr))

(define exec-k #f)
(define vm-k #f)
(define resume-meta #f)
(define beginning #f)

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
             ((if? exp) (eval-if exp env))
             ((lambda? exp) (eval-lambda exp env))
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
    (eval-application-apply body arg env)))
(define (eval-application-arg arg env) (exec arg env))
(define (eval-application-body name env) (exec name env))

(define (eval-application-apply func arguments env)
  (case (car func)
    ((primitive)
     (eval-application-apply-primitive func arguments))
    ((function)
     (exec (caddr func)
           (eval-extend (cadddr func) (cadr func) arguments)))
    (else (error "Not a function -- eval-application-apply"))))
(define (eval-application-apply-primitive func args)
  (apply (cdr func) args))




;=========COMPILER=========

(define SS 30) ;stackSize 10

(define (compile exp env next)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp next))
        ((quoted? exp) (compile-quoted exp next))
        ((variable? exp) (compile-variable exp env next))
        ((definition? exp) (compile-definition exp env next))
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

;==========VM==========

(define VM
  (lambda (a x e s)
    (case (car x)
      [(halt)
       (begin 
         (VM-info-output a x e s)
         a)]
      [(empty) 'ok]
      [(refer) ;(n m x) (cadr x) (caddr x) (cadddr x)
       (VM (index (find-link (cadr x) e) (caddr x)) (cadddr x) e s)]
      [(refer-global) ;var (cadr x) next (caddr x)
       (VM (find-global (cadr x)) (caddr x) e s)]
      [(constant) ;(obj x) (cadr x) (caddr x)
       (VM (cadr x) (caddr x) e s)]
      [(close) ;body : (cadr x) x : (caddr x) define
       (VM (closure (cadr x) e) (caddr x) e s)]
      [(functional) ;body : (cadr x) x : (caddr x) define
       (VM (closure (cadr x) e) (caddr x) e s)]
      [(test) ;(then else) (cadr x) (caddr x)
       (VM a (if a (cadr x) (caddr x)) e s)]
      [(frame) ;ret  (cadr x)  x next (caddr x)
       (VM a (caddr x) e (push (cadr x) (push e s)))]
      [(argument) ;(x) (cadr x)       
       (if (pair? (car x))
           (VM a (cadr x) e (push (cadr a) s))
           (VM a (cadr x) e (push a s)))]
      [(apply) ;body (car a) link (cadr a)
       (cond
         [(eq? 'primitive (car a))
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
          (VM a (car a) s (push (cadr a) s))]
         )]
      [(return) ;(n) (cadr x)
       (let ([s (- s (cadr x))])
         (VM a (index s 0) (index s 1) (- s 2)))]

      [else (VM-else a x e s)])))

;==========helping-function==========

(define index
  (lambda (s i)
    (vector-ref stack (- (- s i) 1))))
(define index-set!
  (lambda (s i v)
    (vector-set! stack (- (- s i) 1) v)))
(define (find-link n e)
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

(define (find-global var)
  (letrec ((loop (lambda (env)
                   (cond
                     [(eq? (car (car (car env))) var)  (car (cdr (car env)))]
                     [(loop (cdr env))]
                     [(null? env) (error 'Undefined_varable--find-global)]))))
    (loop GE)))



(define stack (make-vector SS))
(define (push x s)
  (vector-set! stack s x)
  (+ s 1))

(define (VM-else a x e s)
  (error "Unknow instruction -- VM"))

(define (VM-else-with-breakpoint a x e s)
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
  (VM-update stack s)
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
  (display info) (newline))

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
             (c 'ok))))

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
              (exec-k))))
       (display "exec -- meta")(newline)
       ;(pause halt)
       
       (call/cc
        (lambda (k)
          (set! resume-meta k)
          (if (eq? vm-k #f)
              (run exp)
              (vm-k))))
       (display "VM -- meta")(newline)
       (pause halt)
       (display "generate graphic -- meta")(newline)
       (set! resume-meta beginning))))
  
  (define (dispatch m)
    (cond ((eq? m 'ini) (small-step))
          ((eq? m 'nxt) (resume-meta))
          (else (error "message error -- meta -- dispatch"))))
  dispatch)

;(define f (meta '((lambda(arg) (if arg arg 0)) #f)))
;(f 'ini)

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
               ;(eval-output ,inst)
               (eval-update ,inst arg)
               (make-breakpoint-eval)
               (apply org-fun (cons arg restarg)))))))

(define-macro define-act-compile
  (lambda (fun inst)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)
               (list ,inst
                     (apply org-fun (cons arg restarg))))))))

(define-macro define-act-vm
  (lambda ()
    `(set! VM-else
           VM-else-with-breakpoint)))
       
                 
;==========run define-act==========

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
    (define-act-eval eval-assignment "eval-assignment")
    (define-act-eval eval-definition "eval-definition")
    (define-act-eval eval-if "eval-if")
    (define-act-eval eval-if-test "eval-if-test")
    (define-act-eval eval-if-then "eval-if-then")
    (define-act-eval eval-if-else "eval-if-else")
    (define-act-eval eval-lambda "eval-lambda")
    (define-act-eval eval-application "eval-application")
    (define-act-eval eval-application-arg "eval-application-arg")
    (define-act-eval eval-application-body "eval-application-body")

    (define-act-vm)))

(act)

