;;

(define (preprocess x funpos?)
  (cond ((symbol? x) x)
        ((pair? x)
         (case (car x)
           ((quote) x)
           ((lambda)
            ;`(,(if funpos? 'slambda 'clambda) ,(cadr x) ,@(preprocess (cddr x) #f)))
            ;`(,(if funpos? 'slambda 'clambda) ,(cadr x) ,(preprocess (caddr x) #f)))
           `(,(if funpos? 'slambda 'clambda) ,(cadr x)
             ,@(map (lambda (e) (preprocess e #f))
                    (cddr x))))
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

(define (t1)
  (preprocess '((lambda (x)
                  ((lambda (y)
                     ((lambda (f)
                        (f 10)))
                     (lambda (a) (+ x a))))
                  3)
                2)
              #f))

;; 4.1.5/4.3

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

(define extend
  (lambda (e r)
    (cons r e)))

(define find-link
  (lambda (n e)
    (if (= n 0) e
        (find-link (- n 1) (index e -1)))))

(define collect-free
  (lambda (vars e next)
    (if (null? vars)
        next
        (collect-free (cdr vars) e
                      (compile-refer (car vars) e
                                     (list 'argument next))))))

(define compile-refer
  (lambda (x e next)
    (compile-lookup x e
                    (lambda (n m) (list 'refer n m next)) ;;;
                    (lambda (n) (list 'refer-free n next))
                    (lambda () (list 'refer-global x next))))) ;;;

(define compile-lookup ;;;
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

(define (remove-global e free) ;;;
  (define (flatten l)
    (cond ((null? l) l)
	  ((not (pair? l)) (list l))
	  (else (append (flatten (car l)) (flatten (cdr l))))))
  (set-intersect (flatten e) free))

(define 3imp-compile
  (lambda (x e next)
    (cond
     ((symbol? x)
      (compile-refer x e next))
     ((pair? x)
      (case (car x)
        ((quote)
         (let ((obj (cadr x)))
           (list 'constant obj next)))
        ((slambda)
         (let ((vars (cadr x)) (body (caddr x)))
           (list 'functional ;; 'close
                 (3imp-compile body
                               (cons (extend (car e) vars)
                                     (cdr e))
                               (list 'return (+ (length vars) 1)))
                 next)))
        ((clambda)
         (let ((vars (cadr x)) (body (caddr x)))
           (let ((free (remove-global (car e) (find-free body vars)))) ;;;
             (collect-free free e
                           (list 'close
                                 (length free)
                                 (3imp-compile body
                                               (cons (extend (extend (car e) 'CB) vars) ;;;
                                                     free)
                                               (list 'return
                                                     (length vars)))
                                 next)))))
        ((if)
         (let ((test (cadr x)) (then (caddr x)) (else (cadddr x)))
           (let ((thenc (3imp-compile then e next))
                 (elsec (3imp-compile else e next)))
             (3imp-compile test e (list 'test thenc elsec)))))
        ((set!) ;;; atomawasi
         (let ((var (cadr x)) (x (caddr x)))
           (3imp-compile-lookup var e
                                (lambda (n m)
                                  (3imp-compile x e (list 'assign n m next)))
                               ;;;
                                (lambda ()
                                  (3imp-compile x e (list 'assign-global var next))))))
        (else
         ;;;
         ;(list 'begin-app
         ;;;
         (let loop ((args (cdr x))
                    (c (3imp-compile (car x) e '(apply))))
           (if (null? args)
               (list 'frame
                     ;;;
                     ;(list 'end-app
                     ;;;
                     next
                     ;;;
                     ;)
                     ;;;
                     c)
               (loop (cdr args)
                     (3imp-compile (car args)
                                   e
                                   (list 'argument c)))))
         ;;;
         ;)
         ;;;
         )))
     (else
      (list 'constant x next)))))

(define evaluate
  (lambda (x)
    (VM '() (3imp-compile (preprocess x #f) '(() . ()) '(halt)) 0 '() 0)))

(define (3imp-comp x)
  (3imp-compile (preprocess x #f) '(() . ()) '(halt)))

#|
> (evaluate '((lambda (x y +) (+ x y)) 1 2 3))
(frame
 (halt)
 (constant
  3
  (argument
   (constant
    2
    (argument
     (constant
      1
      (argument
       (close
        (frame (return 4) (refer 0 1 (argument (refer 0 0 (argument (refer 0 2 (apply)))))))
        (apply)))))))))

> (evaluate '((lambda (x y +) (+ x y)) 1 2 3))
(begin-app
 (frame
  (end-app (halt))
  (constant
   3
   (argument
    (constant
     2
     (argument
      (constant
       1
       (argument
        (close
         (begin-app
          (frame (end-app (return 4)) (refer 0 1 (argument (refer 0 0 (argument (refer 0 2 (apply))))))))
         (apply))))))))))
> 
|#

;; 4.1.6/4.4

(define VM
  (lambda (a x f c s) ;; (a x e s)
    (case (car x)
      ((halt) a)
      ((refer)
       (let ((n (cadr x))
             (m (caddr x))
             (x (cadddr x)))
         (VM (index (find-link n f) m) x f c s)))
      ((refer-free)
       (let ((n (cadr x))
             (x (caddr x)))
         (VM (index-closure c n) x f c s)))
      ((refer-global) ;;;
       (let ((var (cadr x))
             (x (caddr x)))
         (VM (refer-global-var var) x f c s)))
      ((constant)
       (let ((obj (cadr x))
             (x (caddr x)))
         (VM obj x f c s)))
      ((functional) ;; (close)
       (let ((body (cadr x))
             (x (caddr x)))
         (VM (functional body f) x f c s)))
      ((close)
       (let ((n (cadr x))
             (body (caddr x))
             (x (cadddr x)))
         (VM (closure body n s) x f c (- s n))))
      ((test)
       (let ((then (cadr x))
             (els (caddr x)))
         (VM a (if a then els) f c s)))
      ((assign) ;; atomawasi
       (let ((n (cadr x))
             (m (caddr x))
             (x (cadddr x)))
         (index-set! (find-link n f) m a)
         (VM a x f c s)))
      ((assign-global) ;;;
       (error "not yot implemented"))
      ((frame)
       (let ((ret (cadr x))
             (x (caddr x)))
         (VM a x f c (push ret (push f (push c s))))))
      ((argument)
       (let ((x (cadr x)))
         (VM a x f c (push a s))))
      ((apply)
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
      ((return)
       (let ((n (cadr x)))
         (let ((s (- s n)))
           (VM a (index s 0) (index s 1) (index s 2) (- s 3))))))))

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

;;

(macro (unless form)
  `(if (not ,(cadr form)) (begin ,@(cddr form))))

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

;;

; (evaluate '((lambda (x y z) (- z x)) 1 2 3))

; (evaluate '((lambda (f g x y z) (f (g x y) z)) + - 25 56 12))

(define x '((lambda (x)
              ((lambda (y)
                 ((lambda (f) (f 10))
                  (lambda (a) (+ x a))))
               3))
            2))

(define y '((lambda (x)
              ((lambda (y)
                 ((lambda (f) (f 10))
                  (lambda (a) (+ y (+ x a)))))
               (+ x 3)))
            2))

(define z '((lambda (x)
              ((lambda (y)
                 ((lambda (f) (f (- 10 x)))
                  (lambda (a) (+ x a))))
               (+ x 3)))
            2))
