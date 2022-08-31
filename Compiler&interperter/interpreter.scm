#lang scheme
(require r5rs)
;=========INTERPRETER=========
(define exec
  (lambda (exp env)
    (display env)(newline)
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
     ;(send-arguments-to-js arguments arguments (cdr func) 0)
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

(define (eval exp) (exec exp GE))
