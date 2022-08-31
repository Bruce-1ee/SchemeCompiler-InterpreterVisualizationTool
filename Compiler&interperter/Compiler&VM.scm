#lang scheme


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
  (or (tagged-list? exp 'slambda)
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
    (let ((thenc (compile then env next))
          (elsec (compile else env next)))
      (compile test env (list 'test thenc elsec)))))
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
(define (compile-application exp env next)
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
      
      ((return) (VM-return a x f c s)))))

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





