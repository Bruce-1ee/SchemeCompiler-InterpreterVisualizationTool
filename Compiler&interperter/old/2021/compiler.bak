#lang scheme
(require r5rs)
(require racket/trace)

(define SS 30) ;stackSize 10


(define (compile x e next)
;    (display x) (newline)
;    (display e) (newline)
    (cond
      [(symbol? x)
       (compile-lookup x e
                       (lambda (n m)
                         (list 'refer n m next))
                       (lambda (var)
                         (list 'refer-free var next)))]
      [(pair? x)
       (case (car x)
         [(quote) ;(obj) (cadr x)
          (list 'constant (cadr x) next)]
         
         [(define) ;var (cadr x) val (caddr x)

          ;(if (null? e)
              ;全局变量
              (cond    
                [(number? (caddr x))
                 (list 'def (cadr x) (list 'constant (caddr x) next))]
                [else
                 (list 'def (cadr x) (compile (caddr x) e '(apply 0))) ])
               ; ((symbol? (caddr x)) (set! GE (cons (cons (cadr x) (find-free (caddr x))) GE)) (list 'ok))
               ; (else  (set! GE (cons (cons (cadr x) (cons 'function (list (compile (caddr x) e (list 'return 0))))) GE)) (list 'ok)))
              ;局部变量
              ;(compile (caddr x) 
              ;         (begin (set-car! e (reverse (cons (cadr x) (reverse (car e))))) e)
              ;         (list 'argument next)) 
              ]

        ; [(begin)
        ;  (let ((c (compile (car exps) e (list 'empty)))
        ;        (env e))
        ;    (letrec ((loop (lambda(exps)
        ;                     (cond
        ;                       [(null? (cdr exps)) (make-sec c next)]
        ;                       [(eq? 'define (car (car exps))) (make-sec c (compile
        ;                                                        (cdr exps)
         ;                                                       (begin (set-car! e (reverse (cons (cadr x) (reverse (car e))))) e)
        ;                                                        (list 'empty)))]
        ;                       [(else (make-sec c (compile (cdr exps)
                           

         [(begin)
          (letrec ((loop (lambda(exps)
                           (if (null? (cdr exps))
                               (compile (car exps) e next)
                               (compile (car exps) e (loop (cdr exps)))))))
            (loop (cdr x)))]
         

         [(lambda) ;(vars body)
          (let ((vars (cadr x))
                (body (caddr x)))
            (list 'close
                  
                  (compile body
                           (extend e vars)
                           (list 'return (+ (length vars) 1)))
                  next))]
         [(if) ;(test then else)
          (let ((test (cadr x))
                (then (caddr x))
                (else (cadddr x)))
            (let ([thenc (compile then e next)]
                  [elsec (compile else e next)])
              (compile test e (list 'test thenc elsec))))]       

         [else
          (letrec ((loop (lambda (args c)
                           (if (null? args)
                               (list 'frame next c)
                               (loop (cdr args)
                                     (compile (car args)
                                              e
                                              (list 'argument c)))))))
            (loop (cdr x) (compile (car x) e (list 'apply (length (cdr x))))))]
         
         

         )]
         
        
      [else
       (list 'constant x next)]))


(define (compile-lookup var e return-local return-free)
    (letrec ((nxtrib (lambda(e rib)
      (if (null? e)
          (return-free var)
          (begin 
            (letrec ((nxtelt (lambda(vars elt)
               (cond
                 [(null? vars) (nxtrib (cdr e) (+ rib 1))]
                 [(eq? (car vars) var) (return-local rib elt)]
                 [else (nxtelt (cdr vars) (+ elt 1))]))))
              (nxtelt (car e) 0)))))))
      
      (nxtrib e 0)))


(define (reverselist ls)
  (define (reverse ls acc)
    (if (null? ls)
        acc
        (reverse (cdr ls) (cons (car ls) acc))))
  (reverse ls '()))



;;
;;(+ e1 e2)
;;'( compiled_e2 (list 'primitive-argument (compiled_e1 (list 'primitive-argument 
;;     (list 'refer-primitive 0 (list 'primitive-apply (length args) next)))))))
;;
(define (compile-primitive exp len env next )
  (let* ((fun (car exp))
        (args (cdr exp))
        (n (letrec ((loop1 (lambda(env n)
                             (cond
                               [(eq? (car env) fun) n ]
                               [else (loop1 (cdr env) (+ n 1))]))))
              (loop1 (cdr GE) 0)))
         (len (length args))
         )
   (letrec ((loop2 (lambda (args c)
                     (if (null? args)
                           c 
                         (loop2 (cdr args) (compile (car args) env (list 'argument c)))))))
     (loop2 args (list 'refer-primitive n (list 'primitive len next ))))))


             

(define extend
  (lambda (e r)
    (cons r e)))

(define VM
  (lambda (a x e s)
;    (display x) (newline)
    (display stack) (newline)
;    (display s) (newline)
;    (display a) (newline)
    
    (case (car x)
      [(halt)  a]
      [(empty) 'ok]
      [(refer) ;(n m x) (cadr x) (caddr x) (cadddr x)
       (VM (index (find-link (cadr x) e) (caddr x)) (cadddr x) e s)]

      [(def) ;var (cadr x) val (caddr x)  ; {def f {close {refer 0 0 {return 2}} {halt}}}
       (let ((ret (if (eq? 'close (car (caddr x)))
                      (cons 'function (caddr x))
                      (VM a (caddr x) e s))))
         (set! GE (cons (cons (cadr x) ret ) GE)))]

      [(refer-free) ;var (cadr x) next (caddr x)
       (VM (find-free (cadr x)) (caddr x) e s)]     
      [(constant) ;(obj x) (cadr x) (caddr x)
       (VM (cadr x) (caddr x) e s)]  
      [(close) ;body : (cadr x) x : (caddr x) define
       (VM (closure (cadr x) e) (caddr x) e s)]
      [(test) ;(then else) (cadr x) (caddr x)
       (VM a (if a (cadr x) (caddr x)) e s)]
      [(frame) ;ret  (cadr x)  x next (caddr x)
       (display 'create_frame:) (display s)(newline)
       (let ((stackpointer (push (cadr x) (push e s))))
         (display stack) (newline)
         (newline)
         (VM a (caddr x) e stackpointer ))]
      
      [(argument) ;(x) (cadr x)
       (display 'argument:)(display a) (newline)
       (display 'stack:)(display s) (newline)
        (display stack)(newline)
        (newline)
        
        (VM a (cadr x) e (push a s))]

      
      [(apply) ;body (car a) link (cadr a)
       (display a) (newline)
       (cond
         [(eq? 'primitive (car a))
          (display 'frame_finished:) (display (- s 1)) (newline)
          
          (letrec ((loop (lambda(argl n)
                           (cond
                             [(= n 0) (VM (apply (cdr a) argl)
                                          (index (- s  (cadr x)) 0)
                                          (index (- s  (cadr x)) 1)
                                          (- (- s (cadr x)) 2))]
                             [else  (loop (cons (index s (- n 1)) argl) (- n 1))]))))
            (loop '() (cadr x)))]
         
         [(eq? 'function (car a))
          (VM a (cdr a) e s)]
          ;(if (eq? 'frame (car (cadr a)))
          ;    (VM a (cadr a) e s)
          ;    (VM a (cadr (cadr a)) e (push e s)))]
         [else
          (display "else:::::")(display (cadr a)) (newline)
          (VM a (car a) s (push (cadr a) s))]
         )]
      
      [(return) ;(n) (cadr x)
       (display 'frame_finished:) (display (- s 1)) (newline)
      ; (display stack) (newline)
       (let ([s (- s (cadr x))])
         (VM a (index s 0) (index s 1) (- s 2)))]
      
      ;[else (display x) (newline) (error 'unknow_return)]
      )))

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

(define (primitive-apply fun n s)
  (letrec ( (loop (lambda (num argl)
                    (if (= num 0)
                        (apply fun argl)
                        (loop (- 1 num) (index s num))))))
    (loop n '())))

(define (find-free1 var)
  (letrec ((loop (lambda (env)
                   (cond
                     [(eq? (car (car env)) var) (list 'free-varable (cdr (car env)))]
                     [(loop (cdr env))]
                     [(null? env) (error 'Undefined_varable--find-free)]))))
    (loop GE)))

(define (find-free var)
  (letrec ((loop (lambda (env)
                   (cond
                     [(eq? (car (car env)) var)  (cdr (car env))]
                     [(loop (cdr env))]
                     [(null? env) (error 'Undefined_varable--find-free)]))))
    (loop GE)))




;;
;; 栈操作定义
;;
(define stack (make-vector SS))
(define (push x s)
  (vector-set! stack s x)
  (+ s 1))



(define GE (map (lambda (a b) (cons a b))
                 (list '+ '- '* '/ '= )
                 (map (lambda (f) (cons 'primitive f))
                         (list + - * / =))
                 ;(list + - * / =  )
                 ))


(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) '()  0 )))

(define run evaluate)

(define (test x)
  (compile x '() '(halt)))






;(run '((lambda(a)((lambda()((lambda() a)))))1))
;(test '( (lambda() 1)))
;(test '( (lambda(a) a) 1 )  )
;(run '( (lambda(a) a) 1))
;(test '(f a 1))
;(test '(+ 1 1))
;(test '(f a) )
;(test '((lambda () a)))
;(run '( (lambda (f x) (f f x)) (lambda (self n) (if (= n 0) 1 (* n (self self (- n 1))))) 3))
;(run '(define f 1))
;(test '(+ f f))
;(test '(lambda () (define a 1)))

;(run '((lambda(a z) ((lambda(b c) ((lambda (d e) z) 1)) 2 3)) 4 5))

;(test '(define f (lambda (a) a)) )
;(test '(f 10))
;(run '(f 10))
         
;(test '(lambda (a) (begin (define b 2) b)))


 
;(test '(define f (lambda (a) (lambda () a))))
;(test '(define g (f 1)))


;(define test1 (run '(define f (lambda(n)
;                  (if (= n 1)
;                      1
;                      (+ (f (- n 1)) n))))))
;
;(run '( (lambda (a b) ((lambda (c) (+ b c)) (+ a b))) 1 2))

;(run '((lambda(a) a) 1))







