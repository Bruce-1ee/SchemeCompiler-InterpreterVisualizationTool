#lang scheme
(require r5rs)
(require compatibility/defmacro)


(define (compile exp env next)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp next))
        ((quoted? exp) (compile-quoted exp next))
        ((variable? exp) (compile-variable exp env next))
        ;((definition? exp) (compile-definition exp env next))
        ;((let? exp) (compile-let exp env next))
        ((if? exp) (compile-if exp env next))
        ((lambda? exp) (compile-lambda exp env next))
        ((application? exp)
         (compile-application exp env next))
        (else (error "Unknown expression type -- COMPILE" exp))))

;=============tagged-list========================
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (variable? exp)
  (symbol? exp))
;(define (assignment? exp)
;  (tagged-list? exp 'set!))
;(define (definition? exp)
;  (tagged-list? exp 'define))
;(define (let? exp)
;  (tagged-list? exp 'let))
(define (if? exp)
  (tagged-list? exp 'if))
(define (lambda? exp)
  (or (tagged-list? exp 'lambda)
      (tagged-list? exp 'slambda)
      (tagged-list? exp 'clambda)))

(define (application? exp)
  (pair? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;===========compiler-process=====================
(define (compile-self-evaluating val next)
  (list 'constant val next))
(define (compile-quoted exp next)
  (list 'constant (cadr exp) next))
(define compile-variable
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
(define (compile-if exp env next)
  (let ((test (cadr exp)) (then (caddr exp)) (else (caddr exp)))
    (let ((thenc (compile-then then env next))
          (elsec (compile-else else env next)))
      (compile-test test env (list 'test thenc elsec)))))

(define (compile-test test env next)
  (compile test env next))

(define (compile-then then env next)
  (compile then env next))

(define (compile-else else env next)
  (compile else env next))


(define (compile-lambda exp env next)
  (cond [(eq? (car exp) 'slambda) (compile-slambda exp env next)]
        [(eq? (car exp) 'clambda) (compile-clambda exp env next)]
        [else (error 'unknown_lambda)]))
(define (compile-slambda exp env next)
  (let ((vars (cadr exp)) (body (caddr exp)))
    (list 'functional
          (compile body
                   (cons (compile-extend (car env) vars)
                         (cdr env))
                   (list 'return (+ (length vars) 1)))
          next)))
(define (compile-clambda exp env next)
  (let ((vars (cadr exp)) (body (caddr exp)))
    (let ((free (remove-global (car env) (find-free body vars))))
      (collect-free free env
                    (list 'close
                          (length free)
                          (compile body
                                   (cons (compile-extend (compile-extend (car env) 'CB) vars) ;;;
                                         free)
                                   (list 'return
                                         (length vars)))
                          next)))))
(define (compile-application-org exp env next)
  (let loop ((args (cdr exp))
             (c (compile (car exp) env '(apply))))
    (if (null? args)
        (list 'frame
              next
              c)
        (loop (cdr args)
              (compile (car args)
                       env
                       (list 'argument c))))))

(define (compile-application-bak exp env next)
  (let loop ((args (cdr exp))
             (c (list 'act-fun-body (compile (car exp) env '(apply)))))
    (if (null? args)
        (list 'frame
              next
              (list 'act-args
                    c))
        (loop (cdr args)
              (compile (car args)
                       env
                       (list 'argument c))))))

(define (compile-application exp env next)
  (let loop ((args (cdr exp))
             (c (compile-fun-body exp env)))
    (if (null? args)
        (list 'frame
              next
              (compile-arg c))
        (loop (cdr args)
              (compile (car args)
                       env
                       (list 'argument c))))))

(define (compile-fun-body exp env)
  (compile (car exp) env '(apply)))
(define (compile-arg c)
  c)
      

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
                      (compile-variable (car vars) e
                                        (list 'argument next))))))

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
  (compile (preprocess exp #f) '(() . ()) '(halt)))

(define VM
  (lambda (a x f c s) ;; (a x e s)
    (case (car x)
      ((halt) a)
      
      ((refer) (VM-refer a x f c s))
      
      ((refer-free) (VM-refer-free a x f c s))

      ((refer-global) (VM-refer-global a x f c s))
      
      ((constant) (VM-constant a x f c s))
      
      ((functional) (VM-functional a x f c s))
      
      ((close) (VM-close a x f c s))
      
      ((test) (VM-test a x f c s))
      
      ;((assign) 
      ;(let ((n (cadr x))
      ;      (m (caddr x))
      ;      (x (cadddr x)))
      ;  (index-set! (find-link n f) m a)
      ;  (VM a x f c s)))
      ;((assign-global) ;;;
      ; (error "not yot implemented"))
      
      ((frame) (VM-frame a x f c s))
      
      ((argument) (VM-argument a x f c s))
      
      ((apply) (VM-apply a x f c s))
      
      ((return) (VM-return a x f c s))

      (else (VM-otherwise a x f c s)))))

(define (VM-constant a x f c s)
  (let ((obj (cadr x))
        (x (caddr x)))
    (VM obj x f c s)))

(define (VM-refer a x f c s)
  (let ((n (cadr x))
        (m (caddr x))
        (x (cadddr x)))
    (VM (index (find-link n f) m) x f c s)))

(define (VM-refer-free a x f c s)
  (let ((n (cadr x))
        (x (caddr x)))
    (VM (index-closure c n) x f c s)))

(define (VM-refer-global a x f c s)
  (let ((var (cadr x))
        (x (caddr x)))
    (VM (refer-global-var var) x f c s)))

(define (VM-test a x f c s)
  (let ((then (cadr x))
        (els (caddr x)))
    (VM a (if a then els) f c s)))

(define (VM-functional a x f c s)
  (let ((body (cadr x))
        (x (caddr x)))
    (VM (functional body f) x f c s)))

(define (VM-close a x f c s)
  (let ((n (cadr x))
        (body (caddr x))
        (x (cadddr x)))
    (VM (closure body n s) x f c (- s n))))

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
     (let ((body (cadr a))
           (link (caddr a)))
       (VM a body s c (push link s))))
    ((closure) ;;;
     (VM a (closure-body (cdr a)) s (cdr a) s)) ;;;
    ((primitive) ;;;
     (let ((s (+ s 1)) ;; (push link s)
           (primfun (cadr a)))
       (primfun s)))
    (else
     (error "Not a function"))))

(define (VM-return a x f c s)
  (let ((n (cadr x)))
    (let ((s (- s n)))
      (VM a (index s 0) (index s 1) (index s 2) (- s 3)))))

(define (VM-otherwise a x f c s)
  (if (set-member? (car x) pseins-list)
      (begin
        (set! vm-info (car x))
        (cond ((break? vm-info) (set! break #t)))
        (make-jumppoint-vm)
        (display 'vm:)(display (car x)) (newline)
        (VM a (cadr x) f c s))
      (error "unknow inst")))
 
  

(define functional
  (lambda (body e)
    (list 'functional body e))) ;;;

(define (primitive-fun natfun) ;;;
  (list 'primitive natfun))

(define stack (make-vector 1000))

(define push
  (lambda (x s)
    (vector-set! stack s x)
    (+ s 1)))

(define index
  (lambda (s i)
    (vector-ref stack (- (- s i) 1))))

(define index-set!
  (lambda (s i v)
    (vector-set! stack (- (- s i) 1) v)))

(define closure
  (lambda (body n s)
    (let ((v (make-vector (+ n 1))))
      (vector-set! v 0 body)
      (let f ((i 0))
        (unless (= i n)
          (vector-set! v (+ i 1) (index s i))
          (f (+ i 1))))
      (cons 'closure v)))) ;;;

(define closure-body
  (lambda (c)
    (vector-ref c 0)))

(define index-closure
  (lambda (c n)
    (vector-ref c (+ n 1))))

(define find-link
  (lambda (n e)
    (if (= n 0) e
        (find-link (- n 1) (index e -1)))))

;;

(define (prim-return retval s)
  (VM retval (index s 0) (index s 1) (index s 2) (- s 3)))

(define the-global-environment ;;;
  `((foo . ,(primitive-fun (lambda (s)
                             (let ((ans 10))
                               ;; (return 3)
                               (write s)(newline)
                               (write stack)(newline)
                               (prim-return ans (- s 3))))))
    (+ . ,(primitive-fun (lambda (s)
                           (let ((ans (+ (index s 1)
                                         (index s 2))))
                             (prim-return ans (- s 3))))))
    (- . ,(primitive-fun (lambda (s)
                           (let ((ans (- (index s 1)
                                         (index s 2))))
                             (prim-return ans (- s 3))))))))

(define (refer-global-var var) ;;;
  (cond ((assq var the-global-environment)
         => (lambda (p) (cdr p)))
        (else
         (error "unbound symbol"))))

(define run
  (lambda (exp)
    (VM '() (compile (preprocess exp #f) '(() . ()) '(halt)) 0 '() 0)))

;; ===============================================
;; ===============================================
;; ===============================================
;; ===============================================
;; ===============================================
(define exec
  (lambda (exp env)
    (call/cc
     (lambda (quit)
       (cond ((self-evaluating? exp)(eval-self-evaluating exp))
             ((variable? exp)(eval-variable exp env))
             ((quoted? exp) (eval-quotation exp))
             ;((assignment? exp) (eval-assignment exp env))
             ;((definition? exp) (eval-definition exp env))
             ;((let? exp) (eval-let exp env))
             ((if? exp) (eval-if exp env))
             ((lambda? exp) (eval-lambda exp env))
             ;((begin? exp) (eval-sequence (cdr exp) env)) ;new
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
  (let ((args (eval-application-args (cdr exp) env))
        (body (eval-application-body (car exp) env)))
    (eval-application-apply body args env)))

(define (eval-application-args args env)
  (map (lambda(x) (exec x env)) args))

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

(define (eval1 exp) (exec exp GE))

(define exec-k #f)
(define vm-k #f)
(define resume-meta #f)


(define (cc)
  (call/cc
   (lambda (quit)
     (set! resume-meta quit)))) 
;(cc)

(define (make-jumppoint-eval)
  (call/cc (lambda (breakpoint)
             (set! exec-k breakpoint)
             (resume-meta))))

(define (make-jumppoint-vm)
  (call/cc (lambda (breakpoint)
             (set! vm-k breakpoint)
             (resume-meta))))


(define-macro define-act-eval
  (lambda (fun info act)
    `(let* ((org-fun ,fun))
       (set! ,fun
             (lambda (arg . restarg)
               ;(set! proc-name ,proc)
               (set! inte-info ,act)
               (cond ((break? inte-info) (set! break #t)))
               (make-jumppoint-eval)
               (display 'eval:)(display ,info)(newline)
               ;(,inst)
               (apply org-fun (cons arg restarg)))))))

(define pseins-list '())
(define-macro define-act-compiler
  (lambda (fun pseins)
    `(let* ((org-fun ,fun))
       (set! pseins-list    ;擬似命令のリストに追加する
             (append pseins-list (list ,pseins))) 
       (set! ,fun
             (lambda (arg . restarg)
               (list ,pseins
                     (apply org-fun (cons arg restarg))))))))

(define ins-step-lst (make-vector 6))

(define ins-lst (list 'self-evaluating
                        'quotation
                        'variable
                        'if
                        ;;new
                        ;'test
                        ;'then
                        ;'else
                        ;;
                        'lambda
                        'application))

(define (ini-vector num)
  (if (> num 0)
      (begin
        (vector-set! ins-step-lst (- num 1) (make-vector 2 0))
        (ini-vector (- num 1)))
      (set! ins-step-lst ins-step-lst)))
(ini-vector 6)



(define (get-ins-num ins ins-lst)
  (let loop ((num 0) (lst ins-lst))
    (cond ((null? lst) -1) ;not included
          ((eq? ins (car lst)) num)
          (else (loop (+ num 1) (cdr lst))))))

(define (set-ins-num ins target)       ;target 0 = inte
  (let ((n (get-ins-num ins ins-lst)))  ;target 1 = vm
       (vector-set! (vector-ref ins-step-lst n)
                    target
                    (+ 1 (vector-ref (vector-ref ins-step-lst n)
                                     target)))))

(define (set-ins-num-by-num ins target num)       ;target 0 = inte
  (let ((n (get-ins-num ins ins-lst)))  ;target 1 = vm
       (vector-set! (vector-ref ins-step-lst n)
                    target
                    num)))



;定义伪指令

;; 0 self-evaluating
(define-act-compiler compile-self-evaluating 'act-constant)
(set-ins-num-by-num 'self-evaluating 1 1)

(define-act-eval eval-self-evaluating 'eval-self-evaluating 'act-constant)
(set-ins-num-by-num 'self-evaluating 0 1)

;; 1 quotation
(define-act-compiler compile-quoted 'act-constant)
(set-ins-num-by-num 'quotation 1 1)

(define-act-eval eval-quotation 'eval-quotation 'act-constant)
(set-ins-num-by-num 'quotation 0 1)

;; 2 variable
(define-act-compiler compile-variable 'act-variable)
(set-ins-num-by-num 'variable 1 1)

(define-act-eval eval-variable 'eval-variable 'act-variable)
(set-ins-num-by-num 'variable 0 1)

;; 3 if
(define-act-compiler compile-if 'act-if)
(define-act-compiler compile-test 'act-test)
(define-act-compiler compile-then 'act-then)
(define-act-compiler compile-else 'act-else)
(set-ins-num-by-num 'if 1 3)

(define-act-eval eval-if 'eval-if 'act-if)
(define-act-eval eval-if-test 'eval-test 'act-test)
(define-act-eval eval-if-then 'eval-then 'act-then)
(define-act-eval eval-if-else 'eval-else 'act-else)
(set-ins-num-by-num 'if 0 3)

;; 4 lambda
(define-act-compiler compile-lambda 'act-lambda)
(set-ins-num-by-num 'lambda 1 1)

(define-act-eval eval-lambda 'eval-lambda 'act-lambda)
(set-ins-num-by-num 'lambda 0 1)

;; 5 application
(define-act-compiler compile-application 'act-application)
(define-act-compiler compile-fun-body 'act-fun-body)
(define-act-compiler compile-arg 'act-args)

(set! pseins-list (append pseins-list (list 'act-args)))
(set! pseins-list (append pseins-list (list 'act-fun-body)))
(set-ins-num-by-num 'application 1 3)

(define-act-eval eval-application 'eval-application 'act-application)
(define-act-eval eval-application-args 'eval-arguments 'act-args)
(define-act-eval eval-application-body 'eval-body 'act-fun-body)
(set-ins-num-by-num 'application 0 3)

;; 6 others
;(define-act-create-frame)




(define (meta)
  (call/cc (lambda (quit)
             (let ((count 0))
               (define top #f)
               (call/cc (lambda (t)
                          (set! top t)
                          (set! next t)
                          (quit)))
             
               (call/cc (lambda (breakpoint)
                          (set! count (+ 1 count))
                          (display count)
                          (set! next breakpoint)
                          (exec-k)
                          (display 'abc)
                          ;(quit)
                          ))
               (call/cc (lambda (breakpoint)

                          (set! next top)
                          (vm-k)
                          (quit)))))))


(define next2 #f)
(define proc-name #f)
(define (meta2)
  (call/cc
   (lambda (quit)
     (let ((loop #f)
           (loop2 #f)
           (in-c 0)
           (vm-c 0))
       (call/cc
        (lambda (top)
          (set! next2 top)
          (set! loop top)
          (quit)))
       (display 'loop1) (display proc-name) (newline)
       ;loop
       (if (and (eq? in-c 0) (eq? vm-c 0))
           (begin
             (let ((n (get-ins-num proc-name ins-lst)))
               (set! in-c (vector-ref (vector-ref ins-step-lst n) 0))
               (set! vm-c (vector-ref (vector-ref ins-step-lst n) 1)))
             (loop))
           (begin
             (call/cc
              (lambda (mid)
                (set! loop2 mid)))
             (display 'loop2) (newline)
             ;loop2
             (call/cc
              (lambda (eval-breakpoint)
                (if (not (eq? in-c 0))
                    (begin
                      (set! in-c (- in-c 1))
                      (set! next2 eval-breakpoint)
                      (exec-k)
                      (quit))
                    (quit))))

             (call/cc
              (lambda (vm-breakpoint)
                (if (not (eq? vm-c 0))
                    (begin
                      (set! vm-c (- vm-c 1))
                      (set! next2 vm-breakpoint)
                      (vm-k)
                      (quit))
                    (quit))))
             (if (and (eq? in-c 0)
                      (eq? vm-c 0))
                 (loop)
                 (loop2))

             )


       )))))


(define tsk-lst '())

(define (add-tsk tsk)
  (set! tsk-lst (append tsk-lst (list (list (vector-ref tsk 0) (vector-ref tsk 1))))))
(define (get-tsk target)
  (cond ((null? tsk-lst) #f)
        ((eq? target 0) (caar (reverse tsk-lst)))
        ((eq? target 1) (cadar (reverse tsk-lst)))))
(define (set-tsk target)
  (let* ((lst (reverse tsk-lst))
        (e (caar lst))
        (v (cadar lst)))
    (set! lst (reverse (cdr lst)))
    (cond ((eq? target 0)
           (set! tsk-lst (append lst (list (list (- e 1) v)))))
          ((eq? target 1)
           (set! tsk-lst (append lst (list (list e (- v 1)))))))))
(define (dummy-tsk?)
  (if (and (eq? (get-tsk 0) 0)
           (eq? (get-tsk 1) 0))
      #t
      #f))
(define (del-tsk)
  (let ((lst (reverse tsk-lst)))
    (set! tsk-lst (reverse (cdr lst)))))

(define next3 #f)
(define (meta3)
  (call/cc
   (lambda (quit)
     (let ((loop #f))

       (call/cc
        (lambda (top)
          (set! next3 top)
          (set! loop top)
          (quit)))
       ;loop
       ;(display tsk-lst) (newline)
       (let ((n (get-ins-num proc-name ins-lst)))
         (if (not (eq? n -1))
             (begin
               (add-tsk (vector-ref ins-step-lst n))
               (set! proc-name #f)
               (loop))
             (begin  
               (call/cc
                (lambda (eval-breakpoint)
                  (set! next3 eval-breakpoint)
                  (if (eq? (get-tsk 0) #f)
                      (begin
                        (display 'done-eval)
                        (quit))
                      (if (not (eq? (get-tsk 0) 0))
                          (begin
                            (set-tsk 0) 
                            (exec-k))
                          (begin
                            (display 'wait-eval) (newline)
                            (quit))))))
               ;(display tsk-lst) (newline)
               (call/cc
                (lambda (vm-breakpoint)
                  (set! next3 vm-breakpoint)
                  (if (eq? (get-tsk 0) #f)
                      (begin
                        (display 'done-vm)
                        (quit))
                      (if (not (eq? (get-tsk 1) 0))
                          (begin
                            (set-tsk 1)
                            (vm-k))
                          (begin
                            (display 'wait-vm) (newline)
                            (quit))))))))
         ;(display tsk-lst) (newline)
         (cond ((dummy-tsk?) (del-tsk)))
           
         (loop)

           )))))

;====break===

(define breakpoints (vector (vector 'act-constant #f)
                            (vector 'act-variable #f)
                            (vector 'act-if #f)
                            (vector 'act-test #f)
                            (vector 'act-then #f)
                            (vector 'act-else #f)
                            (vector 'act-lambda #f)
                            (vector 'act-application #f)
                            (vector 'act-args #f)
                            (vector 'act-fun-body #f)))

(define (get-breakpoint-pos name)
  (let loop ((n 0))
    (cond ((>= n 10) (error 'bad_name))
          ((eq? (vector-ref (vector-ref breakpoints n) 0) name) n)
          (else (loop (+ n 1))))))

(define (breakpoint-switch name)
  (let* ((n (get-breakpoint-pos name))
         (v (vector-ref breakpoints n)))
    (vector-set! v 1 (not (vector-ref v 1)))))

(define (breakpoint-on)
  (let ((len (vector-length breakpoints)))
    (let loop ((n 0))
      (cond ((>= n len) 'done)
            (else
             (begin
               (vector-set! (vector-ref breakpoints n) 1 #t)
               (loop (+ n 1))))))))

(define (breakpoint-off)
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

(define vm-info #f)
(define inte-info #f)

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
                     (vector 'act-fun-body 0 0)))


(define (get-act act-name)
  (define (loop n)
    (cond ((>= n (vector-length acts)) 'not_included)
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

(define (is-same-positon?)
  (if (or (eq? vm-info #f)
          (eq? inte-info #f))
      #f
      (and (eq? vm-info inte-info)
           (eqv? (get-act-num vm-info 'vm)
                 (get-act-num inte-info 'exec)))))



(define (p t)
  (display t) (newline))

(define (meta4 program)
  (call/cc
   (lambda (return)
     (call/cc
      (lambda (top)
        (set! resume-meta top)))
     
     (call/cc
      (lambda (c)
        (cond ((eq? break #t)
               (set! break #f)
               (set! next c)
               (return)))))
      
         (cond ((eq? vm-k #f) (run program))
               ((eq? exec-k #f) (eval1 program))
               ((is-same-positon?)
                (act-add1 inte-info 'exec)
                (exec-k))
               (else
                (act-add1 vm-info 'vm)
                (vm-k))))))
         
  

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

;(meta3)

;(next2)
;多次调用next2之后会发生错误
;原因是当前的procname为SUBAPPLICATION，但没有处理该SUBAPPLICATION的操作
;具体的原因是在处理app的时候掺杂了SELF-EVAL的操作，然而procname只能存储一个数据
;当selfeval处理结束的时候，无法返回之前未完成的app操作
;解决办法是使用一个栈的数据结构记录之前未完成的操作


