#lang scheme
(require r5rs)
(require racket/trace)

(define SS 30) ;stackSize 10


(define (compile x e s c next)
  (cond
    [(symbol? x)
     (let ((nxt (if (set-member? x s)
                    (list 'indirect next)
                    next)))
       (if (set-member? x c)
           (closure-lookup c x next)
           (compile-lookup x e
                           (lambda (n m)
                             (list 'refer-local n m nxt))
                           (lambda (var)
                             (list 'refer-free var nxt)))))]
    [(pair? x)
       (case (car x)
         [(quote) ;(obj) (cadr x)
          (list 'constant (cadr x) next)]
         [(define) ;var (cadr x) val (caddr x)
          (list 'def (cadr x) (compile (caddr x) e s c next))] 
         [(begin)
          (letrec ((loop (lambda(exps)
                           (if (null? (cdr exps))
                               (compile (car exps) e s c next)
                               (compile (car exps) e s c (loop (cdr exps)))))))
            (loop (cdr x)))]
         [(lambda) ;(vars body)
          (let* ([vars (cadr x)]
                 [body (caddr x)]
                 [sets (find-sets body vars)]
                 ;[free-var-num (find-free-refer body vars)]
                 [free-var (find-free-refer body c )]
                 )
            (display "c:") (display c) (newline)
            (display "free:") (display free-var)(newline)
            (if (find-closure body)
                (list 'close
                      (length vars)
                      (compile body
                               (extend e vars)
                               (set-union sets s)
                               (extend-closure vars c)
                               (list 'return (+ (length vars) 1)))
                      next)
                (list 'functional
                      (compile body
                               (extend e vars)
                               (set-union sets s)
                               c
                               (list 'return (+ (length vars) free-var 1)))
                      next)))]
         [(if) ;(test then else)
          (let ((test (cadr x))
                (then (caddr x))
                (else (cadddr x)))
            (let ([thenc (compile then e s c next)]
                  [elsec (compile else e s c next)])
              (compile test e s c (list 'test thenc elsec))))]
         [(set!)
          (let ((var (cadr x))
                (x (caddr x)))
            (compile-lookup var e
                            (lambda (n m)
                              (compile x e s c (list 'assign-local n m next)))
                            (lambda (var)
                              (compile x e s c (list 'assign-free var next)))))]

         [else
          (letrec ((loop (lambda (args c)
                           (if (null? args)
                               (list 'frame next c)
                               (loop (cdr args)
                                     (compile (car args)
                                              e
                                              s
                                              c
                                              (list 'argument c)))))))
            (loop (cdr x) (compile (car x) e s c (list 'apply (length (cdr x))))))]
         
         

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

(define (find-sets x v)
    (cond
      [(symbol? x) '()]
      [(pair? x)
       (let ((t (car x)))
         (case t ;;;;;;;;;
           [(quote) '()]
           [(lambda) 
            (find-sets (caddr x) (set-minus v (cadr x)))]
           [(if)   ;; test (cadr x)  (then (caddr x)  else (cadddr x)
            (set-union (find-sets (cadr x) v)
                       (set-union (find-sets (caddr x) v)
                                  (find-sets (cadddr x) v)))]
           [(set!)   ;;var (cadr x) x (caddr x)
            (set-union (if (set-member? (cadr x) v) (list (cadr x)) '())
                       (find-sets (caddr x) v))]
           [else
            (letrec ((next (lambda (x)
                             (if (null? x)
                                 '()
                                 (set-union (find-sets (car x) v)
                                            (next (cdr x)))))))
              (next x))]))]
      [else '()]))

(define (make-boxes sets vars next)
  (letrec ((f (lambda (vars n)
                (if (null? vars)
                    next
                    (if (set-member? (car vars) sets)
                        (list 'box n (f (cdr vars) (+ n 1)))
                        (f (cdr vars) (+ n 1)))))))
    (f vars 0)))

(define (set-member? x s) ;判断元素x是否在集合s里
  (cond
    [(null? s) #f]
    [(eq? x (car s)) #t]
    [else (set-member? x (cdr s))]))
(define (set-cons x s)   ;将元素x加入集合s里 无重复
  (if (set-member? x s)
      s
      (cons x s)))
(define (set-union s1 s2) ;合并两个集合 无重复
  (if (null? s1)
      s2
      (set-union (cdr s1) (set-cons (car s1) s2))))
(define (set-minus s1 s2) ;s1 - s2 集合做差
  (if (null? s1)
      '()
      (if (set-member? (car s1) s2)
          (set-minus (cdr s1) s2)
          (cons (car s1) (set-minus (cdr s1) s2)))))
(define (set-intersect s1 s2) ;找出共同元素
  (if (null? s1)
      '()
      (if (set-member? (car s1) s2)
          (cons (car s1) (set-intersect (cdr s1) s2))
          (set-intersect (cdr s1) s2))))

(define (count-free vars c)
  (letrec ((loop (lambda (vars n)
                   (cond
                     ((null? vars) n)
                     ((set-member? (car vars) c) (loop (cdr vars) (+ n 1)))
                     (else (loop (cdr vars) n))))))
    (loop vars 0)))
                       



(define (find-closure x)
  (cond
      [(symbol? x) #f]
      [(pair? x)
       (let ((t (car x)))
         (case t ;;;;;;;;;
           [(quote) #f]
           [(lambda) #t]
           [(if)   ;; test (cadr x)  (then (caddr x)  else (cadddr x)
            (and (find-closure (caddr x))
                 (find-closure (cadddr x)))]
           [(set!) #f]
           [else #t]))]
      [else #f]))

(define (find-free-refer x c )
  (letrec ([loop (lambda(x)
                   (cond
                     [(symbol? x) (if (set-member? x c) 1 0) ]
                     [(pair? x)
                      (let ((t (car x)))
                        (case t ;;;;;;;;;
                          [(quote) 0]
                          [(lambda) 0]
                          [(if)   ;; test (cadr x)  (then (caddr x)  else (cadddr x)
                           (+ (loop (cadr x))
                              (loop (caddr x))
                              (loop (cadddr x)))]
                          [(set!) (+ (loop (caddr x)))]
                          [else 0]))]
                     [else 0]))])
           (loop x)))



(define (closure-lookup c x next)
  (letrec ((loop (lambda (vars n)
                   (cond
                     [(null? vars) 'error]
                     [(eq? x (car vars)) (list 'refer-closure n next)]
                     [else (loop (cdr vars) (+ n 1))]))))
    (loop c 0)))
(define free?
  (lambda (x b)
    (cond
      [(symbol? x) (if (set-member? x b) #f #t)]
      [(pair? x)
       (let ((t (car x)))
         (case t
           [(quote) #f]
           [(lambda) ;vars : (cadr x) body: (caddr x)
               (free? (caddr x) (set-union (cadr x) b))]
           [(if) ;; test (cadr x)  (then (caddr x)  else (cadddr x)
            (or (free? (cadr x) b)
                       (or (free? (caddr x) b)
                           (free? (cadddr x) b)))]
           [(set!)   ;;var (cadr x) epx (caddr x)
            (or (if (set-member? (cadr x) b) #f #t)
                (free? (caddr x) b))]
           [else
            (letrec ((next (lambda (x)
                             (if (null? x)
                                 #f
                                 (or (free? (car x) b)
                                     (next (cdr x)))))))
              (next x))]))]
      [else #f])))


(define (reverselist ls)
  (define (reverse ls acc)
    (if (null? ls)
        acc
        (reverse (cdr ls) (cons (car ls) acc))))
  (reverse ls '()))

   

(define extend
  (lambda (e r)
    (cons r e)))
(define (extend-closure vars clo-vars)
  (letrec ((loop (lambda (vars ret)           
                   (cond
                     [(null? vars) ret]
                     [else (loop (cdr vars) (cons (car vars) ret))]))))
    (loop (reverselist vars) clo-vars)))

(define VM
  (lambda (a x e c s)
    (newline)
    (newline)
    (display "x:")(display x) (newline)
    (display stack) (newline)
    (display "s:")(display s) (newline)
    (display "a:")(display a) (newline)
    (display "c:")(display c) (newline)
    (newline)
    
    (case (car x)
      [(halt)  a]
      [(empty) 'ok]
      [(refer-local) ;(n m x) (cadr x) (caddr x) (cadddr x)
       (VM (index (find-link (cadr x) e) (caddr x)) (cadddr x) e c s)]
      [(refer-closure) ;n: cadr x
       (VM (find-closure-var (cadr x) c)
           (caddr x)
           e
           c
           (push (find-closure-var (cadr x) c) s))]
       

      [(def) ;var (cadr x) val (caddr x)  ; {def f {functional {refer-local 0 0 {return 2}} {halt}}}
       (let ((ret (if (eq? 'functional (car (caddr x)))
                      (list 'function (car (caddr x)) (cadr (caddr x)) (list 'apply 0)) 
      ; (let ((ret (if (eq? 'functional (car (caddr x)))
      ;                (cons 'function (caddr x))
                      (VM a (caddr x) e c s))))
         (set! GE (cons (cons (cadr x) ret ) GE)))]

      [(refer-free) ;var (cadr x) next (caddr x)
       (display (find-free (cadr x))) (newline)
       (VM (find-free (cadr x)) (caddr x) e c s)]
      [(indirect)     ;; x: (cadr x)
       (VM (unbox a) (cadr x) e c s)]
      [(constant) ;(obj x) (cadr x) (caddr x)
       (VM (cadr x) (caddr x) e c s)]
      [(close) ; n : cadr body: caddr  x:cadddr
       (letrec ((loop (lambda(ret num)
                        (cond
                          [(eq? num (cadr x) ) ret]
                          [else (loop (cons (index (find-link 0 s) num) ret) (+ num 1))]))))
         (let ((vars
                (if (< s 1)
                    '()
                    (reverselist (loop '() 0)))))
           (VM (list 'closure (closure (caddr x) vars)) (cadddr x) e vars s)))]
       
      [(functional) ;body : (cadr x) x : (caddr x) define
       (VM (closure (cadr x) e) (caddr x) e c s)]
      [(box)          ;;n:  (cadr x) x: (caddr x)
       (index-set! s (+ (cadr x)  1) (box (index s (+  (cadr x)  1))))
       (VM a (caddr x) e c s)]
      [(test) ;(then else) (cadr x) (caddr x)
       (VM a (if a (cadr x) (caddr x)) e c s)]
      [(assign-local)  ;; n: (cadr x) x: (caddr x)
       (set-box! (index (find-link (cadr x) e) (caddr x)) a)
       (VM a (cadddr x) e c s)]
      [(frame) ;ret  (cadr x)   next (caddr x)
       (display 'create_frame:) (display s)(newline)
       (let ((stackpointer (push (cadr x) (push e s))))
         (display stack) (newline)
         (newline)
         (VM a (caddr x) e c stackpointer ))]   ;
      
      [(argument) ;(x) (cadr x)
       (display 'argument:)(display a) (newline)
       (display 'stack:)(display s) (newline)
        (display stack)(newline)
        (newline)
        
        (VM a (cadr x) e c (push a s))]

      
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
                                          c
                                          (- (- s (cadr x)) 2))]
                             [else  (loop (cons (index s (- n 1)) argl) (- n 1))]))))
            (loop '() (cadr x)))]
         
         [(eq? 'function (car a))
          (VM a (cdr a) e c s)]

         [(eq? 'closure (car a))
          (display "closure:") (display (car (cadr a))) 
          (VM (closure (cadr (car (cadr a))) e )  (list 'return (length c))  e c s)]   ;(caddr (car (cadr a)))
         ; (VM a (caddr (car (cadr a))) e c (push (cadddr (car (cadr a))) (push e s)) )]
         ; (VM a (car (cadr a))  e (cdr (cadr a)) s)]
         [else
          (display "else:::::")(display a) (newline)
          (VM a (car a) s c (push (cadr a) s))]
         )]
      
      [(return) ;(n) (cadr x)
       (display 'frame_finished:) (display (- s 1)) (newline)
       
       (let ([s (- s (cadr x))])
         (display (index s 1)) (newline)
         (VM a (index s 0) (index s 1) c (- s 2)))]
      
      ;[else (display x) (newline) (error 'unknow_return)]
      )))

(define index  ;(index s 0) s指向栈顶待插入位置，其结果为栈顶元素
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

(define find-closure-var
  (lambda (n c)
    (letrec ((loop (lambda (n c)
                     (cond
                       [(= n 0) (car c)]
                       [else (loop (- n 1) (cdr c))]))))
      (loop n c))))

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
    (VM '() (compile x '() '() '() '(halt)) '() '() 0 )))

(define run evaluate)

(define (test x)
  (compile x '() '() '() '(halt)))

;(run '((lambda(a) a) 1))
;(run '((lambda (a) ((lambda(b) a )  (set! a 1))) 99))
;(run '(define g (lambda(a)
;                   (lambda ()
;                    ((lambda (set)
;                       a)
;                     (set! a (+ a 1)))
;                    ))))

;(run '(( (lambda (a) (lambda () a)) 1 )))

(run '(((lambda (a b c e d f) (lambda () c)) 1 2 3 4 5 6)))




















