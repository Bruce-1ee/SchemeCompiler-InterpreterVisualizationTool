#lang scheme
(require r5rs)
(require racket/trace)
 
(define exec
      (lambda (exp env)
;        (display env) (newline)
        (if (not (pair? exp))
            (cond ((or (number? exp) (boolean? exp) (string? exp))
                   exp)
                  ((symbol? exp)
                   (car (lookup exp env))))
              (case (car exp)
                [(quote) (cadr exp)]
                [(lambda)  ; (lambda (a b) a)
                 (let ((vars (cadr exp))
                       (body (caddr exp)))
                   (display (cadr exp)) (newline)
                   (list 'function
                         vars  
                         body  
                         env))]
                [(if)
                 (let ((test (cadr exp))
                       (then (caddr exp))
                       (else (cadddr exp)))
                   (if (exec test env)
                       (exec then env)
                       (if (null? else)
                           #f
                           (exec else env))))]
                [(define)
                 (cond ((not (symbol? (cadr exp)))
                        (error "Not a variable -- DEFINE"))
                       (else (define-variable!
                               (cadr exp)
                               (exec (caddr exp) env)
                               env)
                             'ok))]
                [(set!)
                 (let ((var (cadr exp))
                       (val (caddr exp)))
                   (set-car! (lookup var env) (exec val env)))]

                [(begin)
                 (exec-sequence (cdr exp) env)]
                 
                [(call/cc)
                 (call/cc
                  (lambda (k)
                    ((exec exp env)
                     (list (lambda (args) (k (car args)))))))]
                ; [call/cc (exp) (call/cc (exec exp env))] ;idk what is this
                [else  ;exp: ( (lambda () 1) )
                 (display (car exp)) (newline) (display env) (newline)
                 (exec-apply (exec (car exp) env)
                             (map (lambda (x) (exec x env))
                                  (cdr exp)))]
                ))))

(define (get-body func)
  (car (cdr (cdr func))))

(define (exec-apply func arguments) ;函数执行
  (display arguments)
  (case (car func)
    ((primitive) ;primitive函数
     (apply-primitive-function func arguments))
    ((function)  ;一般函数
     (exec (caddr func)
           (extend (cadddr func) (cadr func) arguments)))
    (else (error "error exec-apply"))))
(define (apply-primitive-function func args)
  (apply (cdr func) args))
(define extend ;将变量绑定到环境
  (lambda (env vars vals)
    (cons (cons vars vals) env)))
(define lookup ;从环境中寻找变量的值
  (lambda (var e)
    (letrec 
        ((nxtrib
              (lambda (e)
                (letrec ((nxtelt (lambda (vars vals)
                                   (cond
                                     [(null? vars) (nxtrib (cdr e))]
                                     [(eq? (car vars) var) vals]
                                     [else (nxtelt (cdr vars) (cdr vals))]))))
                  (nxtelt (caar e) (cdar e))))))
      (nxtrib e))))
(define (define-variable! var val env) ;define
  (let ((frame (car env)))
    (define (scan vars vals) ;如果未被绑定则新建绑定
      (cond ((null? vals)    ;如果已被绑定则更改内容
             (set-car! frame (cons var (car frame)))
             (set-cdr! frame (cons val (cdr frame))))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (car frame) (cdr frame))))

(define (exec-sequence exps env)
  (cond ((null? (cdr exps)) (exec (car exps) env))
        (else (exec (car exps) env)
              (exec-sequence (cdr exps) env))))
  
(define GE (extend  '()
                    '() ;在这里添加primitive函数的绑定
                    (map (lambda (f) (cons 'primitive f))
                         (list ))))  ;这里

(define meta
      (lambda (exp)
         (exec exp GE)))

;(meta '(lambda () 1))
;(meta '(+ 1 1))
;(meta '( (lambda() 1) ))
;(meta '(begin 1 2　3 4))
;(meta '(define fib (lambda (n)
;                     (begin 
;                       (define fib-cal
;                         (lambda (a b n)
;                           (if (= n 0)
;                               0
;                               (if (= n 1)
;                                   a
;                                   (if (= n 2)
;                                       b
;                                       (fib-cal b (+ b a) (- n 1)))))))
;                       (fib-cal 1 1 n))))
;)
;(meta '(fib 9000))

;(meta '((lambda (a b c)
;          ((lambda (d e)
;            ((lambda (f) f) d)) b c)) (lambda (g) (set! a g)) 2 3))

