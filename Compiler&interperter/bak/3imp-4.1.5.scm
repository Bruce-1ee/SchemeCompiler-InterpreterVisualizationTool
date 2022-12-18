;; 4.1.5

(define 3imp-compile
  (lambda (x e next)
    (cond
     ((symbol? x)
      (3imp-compile-lookup x e
		      (lambda (n m)
			(list 'refer n m next))
                      ;;;
                       (lambda ()
                         (list 'refer-global x next))))
     ((pair? x)
      (case (car x)
	    ((quote)
	     (let ((obj (cadr x)))
	       (list 'constant obj next)))
	    ((lambda)
	     (let ((vars (cadr x)) (body (caddr x)))
	       (list 'close
		      (3imp-compile body
			       (extend e vars)
			       (list 'return (+ (length vars) 1)))
		      next)))
	    ((if)
	     (let ((test (cadr x)) (then (caddr x)) (else (cadddr x)))
	       (let ((thenc (3imp-compile then e next))
		     (elsec (3imp-compile else e next)))
		 (3imp-compile test e (list 'test thenc elsec)))))
	    ((set!)
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
                         ;;; (list 'frame next c)
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

(define 3imp-compile-lookup
  (lambda (var e return return-free) ;;;
    (let nxtrib ((e e) (rib 0))
      (if (null? e) ;;;
          (return-free) ;;;
          (let nxtelt ((vars (car e)) (elt 0))
            (cond ((null? vars) (nxtrib (cdr e) (+ rib 1)))
                  ((eq? (car vars) var) (return rib elt))
                  (else (nxtelt (cdr vars) (+ elt 1)))))))))

(define extend
  (lambda (e r)
    (cons r e)))

(define find-link
  (lambda (n e)
    (if (= n 0) e
        (find-link (- n 1) (index e -1)))))

(define evaluate
  (lambda (x)
    (VM '() (3imp-compile x '() '(halt)) 0 0)
    ;(3imp-compile x '() '(halt))
    ))

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

;; 4.1.6

(define VM
  (lambda (a x e s)
    (case (car x)
      ((halt) a)
      ((refer)
       (let ((n (cadr x))
             (m (caddr x))
             (x (cadddr x)))
         (VM (index (find-link n e) m) x e s)))
      ((refer-global) ;;;
       (let ((var (cadr x))
             (x (caddr x)))
         (VM (refer-global-var var) x e s)))
      ((constant)
       (let ((obj (cadr x))
             (x (caddr x)))
         (VM obj x e s)))
      ((close)
       (let ((body (cadr x))
             (x (caddr x)))
         (VM (functional body e) x e s)))
      ((test)
       (let ((then (cadr x))
             (els (caddr x)))
         (VM a (if a then els) e s)))
      ((assign)
       (let ((n (cadr x))
             (m (caddr x))
             (x (cadddr x)))
         (index-set! (find-link n e) m a)
         (VM a x e s)))
      ((assign-global) ;;;
       (error "not yot implemented"))
      ((frame)
       (let ((ret (cadr x))
             (x (caddr x)))
         (VM a x e (push ret (push e s)))))
      ((argument)
       (let ((x (cadr x)))
         (VM a x e (push a s))))
      ((apply)
       (cond ((eq? (car a) 'functional) ;;;
              (let ((body (cadr a))
                    (link (caddr a)))
                (VM a body s (push link s))))
             ((eq? (car a) 'primitive) ;;;
	      (let ((s (+ s 1)) ;; (push link s)
		    (primfun (cadr a)))
		(primfun s)))
             (else
              (error "Not a function"))))
      ((return)
       (let ((n (cadr x)))
         (let ((s (- s n)))
           (VM a (index s 0) (index s 1) (- s 2))))))))

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

(define (prim-return retval s)
  (VM retval (index s 0) (index s 1) (- s 2)))

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
