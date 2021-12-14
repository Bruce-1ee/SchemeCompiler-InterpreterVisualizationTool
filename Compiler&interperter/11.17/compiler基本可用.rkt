#lang scheme
(require r5rs)
(require racket/trace)

;11.4 在find-set, find-free函数中未添加begin相关代码

;;
;;
(define SS 10) ;stackSize 10


 
(define (compile x e s next)
;  (display e)　(newline)
    (cond
      [(symbol? x)
       (compile-refer x e
                      (if (set-member? x s)
                          (list 'indirect next)
                          next))]
      [(pair? x)
       (let ((fp (car x)))
         (case fp
           [(quote) (list 'constan (cadr x) next)]
           [(lambda)
            (let ((vars (cadr x))
                  (body (caddr x)))
              (let ([free (find-free body vars)]
                    [sets (find-sets body vars)])
                (collect-free free e
                              (list 'close
                                    (length free)
                                    (make-boxes sets vars
                                                (compile body
                                                         (cons vars free)
                                                         (set-union
                                                          sets
                                                          (set-intersect s free))
                                                         (list 'return (length vars))))
                                    next))))]
           [(if)
            (let ((test (cadr x))
                  (then (caddr x))
                  (else (cadddr x)))
              (let ([thenc (compile then e s next)]
                    [elsec (compile else e s next)])
                (compile test e s (list 'test thenc elsec))))]
           [(set!)
            (let ((var (cadr x))
                  (x (caddr x)))
              (compile-lookup var e
                              (lambda (n)
                                (compile x e s (list 'assign-local n next)))
                              (lambda (n)
                                (compile x e s (list 'assign-free n next)))))]

           [(begin)  ;begin (car exp) exps (cdr exp)
            (let ((body (reverselist (cdr x))))  ;反转body并将最后一项与next链接，之后的各项倒序链接
              (letrec ((loop (lambda(exps nxt)
                               (if (null? (cdr exps))
                                   (list 'begin (compile (car exps) e s nxt) )
                                   (loop (cdr exps) (compile (car exps) e s nxt))))))
                (loop body next)))]

          
           [(call/cc)
            (let ([c (list 'conti
                           (list 'argument
                                 (compile x e s
                                          (if (tail? next)
                                              (list 'shift
                                                    1
                                                    (cadr next)
                                                    '(apply))
                                              '(apply)))))])
              (if (tail? next)
                  c
                  (list 'frame next c)))]
           
           [else
            (letrec ((loop (lambda (args c)
                             (if (null? args)
                                 (if (tail? next)
                                     c
                                     (list 'frame next c))
                                 (loop (cdr args)
                                       (compile (car args)
                                                e
                                                s
                                                (list 'argument c)))))))
              (loop (cdr x) (compile (car x) e s
                                     (if (tail? next)
                                         (list 'shift
                                               (length (cdr x))
                                               (cadr next)
                                               '(apply))
                                         '(apply)))))]))]      
           [else (list 'constant x next)]))

;;
;; 栈操作定义
;;
(define stack (make-vector SS))
(define (push x s)
  (vector-set! stack s x)
  (+ s 1))
(define (index s i)
  (vector-ref stack (- (- s i) 1)))
(define (index-set! s i v)
  (vector-set! stack (- (- s i) 1) v))

;;
;;变量查找
;;
(define (compile-refer x e next)
  (compile-lookup x e
                  (lambda (n) (list 'refer-local n next))
                  (lambda (n) (list 'refer-free n next))))
(define (compile-lookup x e return-local return-free)
  (letrec ((nxtlocal (lambda (locals n)
                       (if (null? locals)
                           (letrec ((nxtfree (lambda(free n)
                                               (if (eq? (car free) x)
                                                   (return-free n)
                                                   (nxtfree (cdr free) (+ n 1))))))
                             (nxtfree (cdr e) 0))                 
                           (if (eq? (car locals) x)
                               (return-local n)
                               (nxtlocal (cdr locals) (+ n 1)))))))
    (nxtlocal (car e) 0)))
(define collect-free
  (lambda (vars e next)
    (if (null? vars)
        next
        (collect-free (cdr vars) e
                      (compile-refer (car vars) e
                                     (list 'argument next))))))
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
           [(begin)
            (letrec ((loop (lambda (body)
                             (if (null? body)
                                 v
                                 (loop (cdr body) (set-union v (find-sets (car body) v)))))))
              (loop (cdr x)))]
           ;[call/cc (exp) (find-sets exp v)]
           [else
            (letrec ((next (lambda (x)
                             (if (null? x)
                                 '()
                                 (set-union (find-sets (car x) v)
                                            (next (cdr x)))))))
              (next x))]))]
      [else '()]))
(define find-free
  (lambda (x b)
    (cond
      [(symbol? x) (if (set-member? x b) '() (list x))]
      [(pair? x)
       (let ((t (car x)))
         (case t
           [(quote) '()]
           [(lambda) ;vars : (cadr x) body: (caddr x)
               (find-free (caddr x) (set-union (cadr x) b))]
           [(if) ;; test (cadr x)  (then (caddr x)  else (cadddr x)
            (set-union (find-free (cadr x) b)
                       (set-union (find-free (caddr x) b)
                                  (find-free (cadddr x) b)))]
           [(set!)   ;;var (cadr x) epx (caddr x)
            (set-union (if (set-member? (cadr x) b) '() (list (cadr x)))
                       (find-free (caddr x) b))]
           ;[call/cc (exp) (find-free exp b)]
           [else
            (letrec ((next (lambda (x)
                             (if (null? x)
                                 '()
                                 (set-union (find-free (car x) b)
                                            (next (cdr x)))))))
              (next x))]))]
      [else '()])))
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
(define tail?
  (lambda (next)
    (eq? (car next) 'return)))
(define (reverselist ls)
  (define (reverse ls acc)
    (if (null? ls)
        acc
        (reverse (cdr ls) (cons (car ls) acc))))
  (reverse ls '()))


(define VM
      (lambda (a x f c s)
        (display stack)
        (newline)
 ;       (display x) (newline)
 ;       (display f) (newline)
 ;       (display c) (newline)
 ;       (display s) (newline)
 ;       (newline)
        (let ((t (car x)))
          (case t
            [(halt) a]
            [(refer-local)  ;; n: (cadr x) x: (caddr x)
             (VM (index f (cadr x)) (caddr x) f c s)]
            [(refer-free)   ;; n: (cadr x) x: (caddr x)
             (VM (index-closure c (cadr x)) (caddr x) f c s)]
            [(indirect)     ;; x: (cadr x)
             (VM (unbox a) (cadr x) f c s)]
            [(constant)     ;; obj: (cadr x) x:(caddr x)
             (VM (cadr x) (caddr x) f c s)]
            [(close)        ;; n: (cadr x) body: (caddr x) x: (cadddr x)
             (VM (closure (caddr x) (cadr x) s) (cadddr x) f c (- s (cadr x)))]
            [(box)          ;;n:  (cadr x) x: (caddr x)
             (index-set! s (cadr x) (box (index s (cadr x))))
             (VM a (caddr x) f c s)]
            [(test)          ;;then: (cadr x) else: (caddr x)
             (VM a (if a (cadr x) (caddr x)) f c s)]
            [(begin)   ;后期添加breakpoint时在此输出具体信息
             (VM a (car (cdr x)) f c s)]
            [(assign-local)  ;; n: (cadr x) x: (caddr x)
             (set-box! (index f (cadr x)) a)
             (VM a (caddr x) f c s)]
            [(assign-free)  ;; n: (cadr x) x: (caddr x)
             (set-box! (index-closure c (cadr x)) a)
             (VM a (caddr x) f c s)]
            ;[conti (x)
            ; (VM (continuation s) x f c s)]
            ;[nuate (stack x)
            ; (VM a x f c (restore-stack stack))]
            [(frame)   ;;ret: (cadr x) x:(caddr x)
             ;(display "new frame")
             (VM a (caddr x) f c (push (cadr x) (push f (push c s))))]
            [(argument) ;;x: (cadr x)
             (VM a (cadr x) f c (push a s))]
            [(shift)   ;;n: (cadr x) m: (caddr x) x:(cadddr x)
             (VM a (cadddr x) f c (shift-args (cadr x) (caddr x) s))]
            [(apply) 
             (VM a (closure-body a) s a s)]
            [(return)  ;;n: (cadr x)
             (let ([s (- s (cadr x))])
                (VM a (index s 0) (index s 1) (index s 2) (- s 3)))]))))

(define (closure body n s)
  (let ([v (make-vector (+ n 1))])
    (vector-set! v 0 body)
    (letrec ((f (lambda (i)
                  (unless (= i n)
                    (vector-set! v (+ i 1) (index s i))
                    (f (+ i 1))))))
      (f 0))
    v))
(define closure-body
  (lambda (c)
    (vector-ref c 0)))
(define index-closure
  (lambda (c n)
    (vector-ref c (+ n 1))))
(define (shift-args n m s)
  (letrec ((nxtarg (lambda (i)
                     (unless (< i 0)
                       (index-set! s (+ i m) (index s i))
                       (nxtarg (- i 1))))))
    (nxtarg (- n 1)))
  (- s m))


;;
;;
;;测试
;;
;;

(define GE (list (list 'nnn '+) ))   ;(compile x e s next)
(define evaluate
     (lambda (x)
         (VM '() (compile x '() '() '(halt)) 0 '() 0)))
(define run evaluate)
(define test
  (lambda (x)
    (compile x '() '() '(halt))))

(define exe
     (lambda (x)
         (VM '() (compile x GE '() '(halt)) 0 '() (push + 1))))

;(exe '(+ 1 1))

;(test '(lambda(a b) c))

;(test '((lambda (a b) (set! a b)) 1 2))
;(eva '((lambda(a b) (set! a b)) 1 99))

;(test '( (lambda (f x) (f f x)) (lambda (self n) (if (= n 0) 1 (* n (self self (- n 1))))) 5))
(run '( (lambda(x) ( (lambda (a b) (set! x 66) ) 99 88) ) 77))
(test '( (lambda(x) ( (lambda (a b) (set! a 66) ) 99 88) ) 77))
;(run '( (lambda(f g) (f g))  (lambda(g) (g)) (lambda() 123)))
         
;(run '( (lambda(eq mul min)  ( (lambda (f x) (f f x)) (lambda (self n) (if (eq n 0) 1 (mul n (self self (min n 1))))) 5) ) = * -) )
;(test '((lambda (a) a) +))
                                                              