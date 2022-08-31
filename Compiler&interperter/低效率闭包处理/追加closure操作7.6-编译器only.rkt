#lang scheme
(require r5rs)
(require racket/trace)

(define SS 30)
(define closure-counter 0)
(define free-v '())

(define (compile exp env free next)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp next))
        ((quoted? exp) (compile-quoted exp next))
        ((variable? exp) (compile-variable exp env free next))
        ((definition? exp) (compile-definition exp env next))
        ((let? exp) (compile-let exp env next))
        ((if? exp) (compile-if exp env free next))
        ((lambda? exp) (compile-lambda exp env next))
        ((application? exp)
         (compile-application exp env free next))
        (else (error "Unknown expression type -- COMPILE" exp))))

(define (compile-self-evaluating exp next) (list 'constant exp next))
(define (compile-quoted exp next) (list 'constant (cadr exp) next))
(define (compile-variable exp env free next)
  ;(compile-lookup
  (lookup
   exp env free next
   ;(lambda (n m)(compile-variable-local n m next))
   ;(lambda (var)(compile-varable-global var next))))

   ))
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

(define (compile-if exp env free next)
  (let* ((test (cadr exp))
         (then (caddr exp))
         (else (cadddr exp))
         (thenc (compile-if-then then env free next))
         (elsec (compile-if-else else env free next)))
    (compile-if-test test env free next thenc elsec)))
(define (compile-if-then then env free next) (compile then env free next))
(define (compile-if-else else env free next) (compile else env free next))
(define (compile-if-test test env free next then else)
  (compile test env free (list 'test then else)))
    
(define (compile-lambda exp env next)
  
  (let* ((vars (cadr exp))
         (body (caddr exp))
         (free-vars (find-free exp env))
         (clo-vars (find-clovars free-vars (extend-compiler env vars)))
         (ret
          (list 'close
                (compile body
                         (extend-compiler env vars)
                         clo-vars
                         (list 'return (+ (length vars) 1)))
                next)))

    (make-closure2 env free-vars ret)))
  ;  (collect-free vars free-vars env ret))
    ;ret)
;{functional {refer-global a {return 1}} {halt}}
;(trace compile-lambda)
    
(define (compile-application exp env free next)
  (letrec ((loop (lambda (args c)
                         (if (null? args)
                             (compile-application-frame next c)
                             (loop (cdr args)
                                   (compile-application-argument (car args) env free c))))))
                                   ;;(compile (car args) env (list 'argument c))
          (loop (cdr exp) (compile-application-name exp env free next))))
(define (compile-application-frame next c) (list 'frame next c))
(define (compile-application-name exp env free next) (compile (car exp) env free (list 'apply (length (cdr exp)))))
(define (compile-application-argument arg env free c) (compile arg env free (list 'argument c)))
  
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




;------新加入--------

(define collect-free
  (lambda (vars frees e next)
    (if (null? frees)
        next
        (collect-free (cdr frees) e
                      (make-closure (car frees) e closure-counter
                                        next)))))

(define (make-closure2 env frees next)

  (letrec ((loop
            (lambda (frees)
              
              (if (null? frees)
                  next
                  (list 'make-closure
                        (get-index (car frees) (car env) 0)
                        (loop (cdr frees)))))))
    (if (null? env)
        next
        (loop frees))))

  
(define (get-index var vars i)
  (cond
    [(null? vars) 'error]
    [(eq? var (car vars)) i]
    [else (get-index var (cdr vars) (+ i 1))]))


(define (make-closure var e n next) ;n : closure-number
  (letrec ((nxtrib (lambda ()
                     (if (null? e)
                         #f
                         (let ((ret (car e)))
                           (set! e (cdr e))
                           ret))))
           (nxtelt (lambda (vars n m)
                     (cond
                       [(eq? vars #f) #f]
                       [(null? vars) (nxtelt (nxtrib) (+ n 1) 0)]
                       [(eq? var (car vars)) (cons n m)]
                       [else (nxtelt (cdr vars) n (+ m 1))]))))
    (let ((res  (nxtelt (nxtrib) 0 0)))
      (if (eq? res #f)
          next
          (list 'make-closure n (list 'argument next))))))

(define (lookup var e free next) ;n : closure-number
  (letrec ((nxtrib (lambda ()
                     (if (null? e)
                         #f
                         (let ((ret (car e)))
                           (set! e (cdr e))
                           ret))))
           (nxtelt (lambda (vars n m)
                     (cond
                       [(eq? vars #f) #f]
                       [(null? vars) (nxtelt (nxtrib) (+ n 1) 0)]
                       [(eq? var (car vars)) (list 'refer-loacl n m next)]
                       [else (nxtelt (cdr vars) n (+ m 1))]))))
    (let ((res  (nxtelt (nxtrib) 0 0))
          (clo (lookup-free var free 0)))
      (if clo
          (list 'refer-closure clo next)
          (if (eq? res #f)
              (list 'refer-global var next)
              res)))))


(define (lookup-free var frees i)
  (cond
    [(null? frees) #f]
    [(eq? var (car frees)) i]
    [else (lookup-free var (cdr frees) (+ i 1))]))



    
(define (find-clovars vars env)

  (define (main vars e ret)

    (define (nxtelt var e)
      (cond
        [(null? e) 'not-found]
        [(eq? (car e) var) (begin (set! ret (cons var ret)) 'found)]
        [else (nxtelt var (cdr e))]))

    (define (nxtrib var e)
      (cond
        [(null? e) 'not-found]
        [else 
         (if (eq? 'not-found (nxtelt var (car e)))
             (nxtrib var (cdr e))
             'found)]))
    (if (null? vars)
        (reverse ret)
        (begin (nxtrib (car vars) e)
               (main (cdr vars) e ret))))
  (main vars env '()))

  


 
    

    
(define find-free
  (lambda (x b)
    (cond
      [(symbol? x) (if (set-member? x b) '() (list x))]
      [(pair? x)
       (cond
         [(quoted? x) '()]
         [(lambda? x) (find-free (caddr x) (set-union (cadr x) b))]
         [(if? x) (set-union (find-free (cadr x) b)
                             (set-union (find-free (caddr x) b)
                                        (find-free (cadddr x) b)))]         
         [else
          (letrec ((next (lambda (x)
                           (if (null? x)
                               '()
                               (set-union (find-free (car x) b)
                                          (next (cdr x)))))))
            (next x))])]
      [else '()])))



(define set-member?
  (lambda (x s)
    (cond
      [(null? s) #f]
      [(eq? x (car s)) #t]
      [else (set-member? x (cdr s))])))
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







    

(define extend-compiler
  (lambda (e r)
    (cons r e)))

(define eval-extend 
  (lambda (env vars vals)
    (cons (cons vars vals) env)))

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

(define (compile-let exp env next)
  (let ((e (let->lambda exp)))
    (compile e env next)))


(define GE
  (eval-extend
   '()
   '(+ - * = display) ; <- ( + - * / )
   (map (lambda (f) (cons 'primitive f))
        (list + - * = display))))  ; <- ( + - * / )



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

(define (sc x)
  (compile x '() '() '(halt)))




;==========VM==========


(define VM

  (lambda (a x e c s)
    (display "a:")(display a)(newline)
    (display "x:")(display (car x))(newline)
    (display "e:")(display e)(newline)
    (display "c:")(display c)(newline)
    (display "s:")(display s)(newline)
    (display "stack:")(display stack)(newline)(newline)
    (case (car x)
      [(halt)  (vm-halt a x e c s)]
      [(empty) (vm-empty a x e c s)]
      [(refer-loacl) (vm-refer-loacl a x e c s)]
      [(refer-global) (vm-refer-global a x e c s)]
      [(refer-closure) (vm-refer-closure a x e c s)]
      [(constant) (vm-constant a x e c s)]
      [(close) (vm-close a x e c s)]
      [(make-closure) (vm-make-closure a x e c s)]
      [(functional) (vm-functional a x e c s)]
      [(test) (vm-test a x e c s)]
      [(frame) (vm-frame a x e c s)]
      [(argument) (vm-argument a x e c s)]
      [(apply) (vm-apply a x e c s)]
      [(return) (vm-return a x e c s)]
      [else (vm-else a x e c s)])))

(trace VM)

(define (vm-halt a x e c s) a)
(define (vm-empty a x e c s) 'ok)

(define (vm-refer-loacl a x e c s)
  (let ((n (cadr x))
        (m (caddr x))
        (x (cadddr x)))
    (VM (index (find-link n e) m) x e c s)))

(define (vm-refer-global a x e c s)
  (let ((var (cadr x))
        (x (caddr x)))
    (VM (find-global var GE) x e c s)))

(define (vm-constant a x e c s)
  (let ((obj (cadr x))
        (x (caddr x)))
    (VM obj x e c s)))

;待修改
(define (vm-close a x e c s) 
  (let ((body (cadr x))
        (next (caddr x)))
    (if (null? c)
        (VM (closure body e) next e c s)
        (VM (closure body c) next e '() s))))

(define (vm-functional a x e c s)
  (let ((body (cadr x))
        (x (caddr x)))
    (VM (closure body e) x e c s)))

(define (vm-test a x e c s)
  (let ((then (cadr x))
        (else (caddr x)))
    (VM a (if a then else) e c s)))

(define (vm-frame a x e c s)
  (let ((ret (cadr x))
        (x (caddr x)))
    ;(display "frame") (newline)
    ;(add-frame-counter)
    (VM a x e c (push ret (push e s)))))

(define (vm-argument a x e c s)
  (if (pair? (car x))
      (VM a (cadr x) e (push (cadr a) c s))
      (VM a (cadr x) e c (push a s))))

;未修改
(define (vm-apply a x e c s)
    (cond
      [(eq? 'primitive (car a))
       ;(display "apply") (newline)
       ;(sub-frame-counter) ;自动删除
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
           (VM a (cadr a) e c s)
           (VM a (cadr (cadr a)) e c (push e s)))]
      [else
       (VM a (car a) s c (push (cadr a) s))]))

(define (vm-return a x e c s)
  ;(display "return") (newline)
  ;(sub-frame-counter) ;自动删除
  (let ([s (- s (cadr x))])
         (VM a (index s 0) (index s 1) c (- s 2))))


(define (vm-make-closure a x e c s)
  (let ((i (cadr x))
        (next (caddr x)))
    (VM a
        next
        e
        (append c (list (index e i)))
        s)))
 
(define (vm-refer-closure a x e c s)
  (let ((i (cadr x))
        (next (caddr x)))
    (VM (index-closure c i)
        next
        e
        c
        s)))

(define (index-closure clo i)
  (if (eq? i 0)
      (car clo)
      (index-closure (cdr clo) (- i 1))))
    



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


(define (primitive-apply fun n s)
  (letrec ( (loop (lambda (num argl)
                    (if (= num 0)
                        (apply fun argl)
                        (loop (- 1 num) (index s num))))))
    (loop n '())))




(define stack (make-vector SS))
(define (push x s)
  (vector-set! stack s x)
  (+ s 1))

(define (vm-else a x e s)
  (error "Unknow instruction -- VM"))

(define run
  (lambda (x)
    (VM '() (compile x '() '() '(halt)) '() '()  0)))

