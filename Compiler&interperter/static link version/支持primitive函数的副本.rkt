#lang scheme
(require r5rs)
(require racket/trace)

(define SS 30) ;stackSize 10


(define compile
  (lambda (x e next)
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

         [(aaaaa)    ;(compile (car x) e '(apply))

            (if (symbol? (car x) )
                (letrec ((loop2 (lambda(args c)
                                  ;(display c)
                                  (if (null? args)
                                      (list 'frame next (list 'close (list 'primitive c (list 'apply)) ))
                                      (loop2 (cdr args)
                                             (compile (car args)
                                                      e
                                                      (list 'argument c)))))))
                  ;(loop2 (cdr x) (compile (car x) e (list 'return (+ 1(length (cdr x))))))
                  (loop2 (cdr x) (compile (car x) e  (list 'return (+ 1(length (cdr x))))))
                  )
                (letrec ((loop (lambda (args c)
                                 (if (null? args)
                                     (list 'frame next c)
                                     (loop (cdr args)
                                           (compile (car args)
                                                    e
                                                    (list 'argument c)))))))
                  (loop (cdr x) (compile (car x) e '(apply))))
                )
                
                ]

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
       (list 'constant x next)])))


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

(define (primitive? var)
  (letrec ((loop (lambda (env)
                   (cond
                     [(null? env) #f]
                     [(eq? (car env) var) #t]
                     [else (loop (cdr env))]))))
    (loop (cdr GE))))  ; GE env : ( (body) (name) )



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
;    (display a) (newline)
;    (display stack) (newline)
;    (display  (car x) )(newline)
    (case (car x)
      [(halt)  a]
      
      [(refer) ;(n m x) (cadr x) (caddr x) (cadddr x)
       (VM (index (find-link (cadr x) e) (caddr x)) (cadddr x) e s)]

      [(refer-free) ;var (cadr x) next (caddr x)
       (VM (find-free (cadr x)) (caddr x) e s)]
      
      [(constant) ;(obj x) (cadr x) (caddr x)
       (VM (cadr x) (caddr x) e s)]
      
      [(close) ;body : (cadr x) x : (caddr x) define
       (VM (closure (cadr x) e) (caddr x) e s)]
      
      [(test) ;(then else) (cadr x) (caddr x)
       (VM a (if a (cadr x) (caddr x)) e s)]

      [(frame) ;(ret x)  (cadr x) (caddr x)
       
       (display stack) (newline)
       (VM a (caddr x) e (push (cadr x) (push e s)))]
      
      [(argument) ;(x) (cadr x)

       (display stack) (newline)
       (if (eq? (car x) 'free-varable)
           (VM a (cadr x) e (push (cadr a) s))
           (VM a (cadr x) e (push a s))) ]
      
      [(apply) ;body (car a) link (cadr a)
     ;  (display stack) (newline)
       (if (eq? (car a) 'free-varable)
           (letrec ((loop (lambda(argl n)
                            (cond
                              [(= n 0) (VM (apply (cadr a) argl)
                                           (index (- s  (cadr x)) 0)
                                           (index (- s  (cadr x)) 1)
                                           (- (- s (cadr x)) 2))]
                              [else  (loop (cons (index s (- n 1)) argl) (- n 1))]))))
             (loop '() (cadr x)) )
           (VM a (car a) s (push (cadr a) s))
       )]
      
      [(return) ;(n) (cadr x)
       (display stack) (newline)
       (let ([s (- s (cadr x))])
         (VM a (index s 0) (index s 1) (- s 2)))]
      
      [else (display x) (newline) (error 'unknow_return)]
      )))

(define index
  (lambda (s i)
    (vector-ref stack (- (- s i) 1))))
(define index-set!
  (lambda (s i v)
    (vector-set! stack (- (- s i) 1) v)))
(define find-link
  (lambda (n e)
    (if (= n 0) e
        (find-link (- n 1) (index e -1)))))
(define closure
  (lambda (body e)
    (list body e)))

(define (primitive-apply fun n s)
  (letrec ( (loop (lambda (num argl)
                    (if (= num 0)
                        (apply fun argl)
                        (loop (- 1 num) (index s num))))))
    (loop n '())))

(define (find-free var)
  (letrec ((loop (lambda (env)
                   (cond
                     [(eq? (car (car env)) var) (list 'free-varable (cdr (car env)))]
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
                 (list + - * / =   )))

(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) '()  0 )))

(define run evaluate)

(define (test x)
  (compile x '() '(halt)))

;(test '( (lambda () 1) ))
;(test '( (lambda (a) (+ a 3)) 1))
;(test '((lambda(a) a) 1))


;(run '((lambda(a)((lambda()((lambda() a)))))1))
;(test '( (lambda() 1)))
;(test '( (lambda(a) a) 1 )  )
;(run '( (lambda(a) a) 1))
;(test '(f a 1))
;(test '(+ 1 1))
;(test '(f a) )
;(test '((lambda () a)))
(run '( (lambda (f x) (f f x))
        (lambda (self n) (if (= n 0) 1 (* n (self self (- n 1)))))
        1) )
;(run '((lambda(a z) ((lambda(b c) ((lambda (d e) z) 1)) 2 3)) 4 5))




  




