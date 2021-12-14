#lang racket
(define compile
   (lambda (x e s next)
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
             [quote (obj) (list 'constant obj next)]
             [lambda (vars body)
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
                          next)))]
             [if (test then else)
                 (let ([thenc (compile then e s next)]
                       [elsec (compile else e s next)])
                   (compile test e s (list 'test thenc elsec)))]
             [set! (var x)
                   (compile-lookup var e
                                   (lambda (n)
                                     (compile x e s (list 'assign-local n next)))
                                   (lambda (n)
                                     (compile x e s (list 'assign-free n next))))]
             [call/cc (x)
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
              (recur loop ([args (cdr x)]
                           [c (compile (car x) e s
                                       (if (tail? next)
                                           (list 'shift
                                                 (length (cdr x))
                                                 (cadr next)
                                                 '(apply))
                                           '(apply)))])
                (if (null? args)
                    (if (tail? next)
                        c
                        (list 'frame next c))
                    (loop (cdr args)
                          (compile (car args)
                                   e
                                   s
                                   (list 'argument c)))))])]
         [else (list 'constant x next)])))

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
                    (lambda (n) (list 'refer-local n next))
                    (lambda (n) (list 'refer-free n next)))))
(define compile-lookup
  (lambda (x e return-local return-free)
    (recur nxtlocal ([locals (car e)] [n 0])
      (if (null? locals)
          (recur nxtfree ([free (cdr e)] [n 0])
            (if (eq? (car free) x)
                (return-free n)
                (nxtfree (cdr free) (+ n 1))))
          (if (eq? (car locals) x)
              (return-local n)
              (nxtlocal (cdr locals) (+ n 1)))))))


(define closure
  (lambda (body n s)
    (let ([v (make-vector (+ n 1))])
      (vector-set! v 0 body)
      (recur f ([i 0])
        (unless (= i n)
          (vector-set! v (+ i 1) (index s i))
          (f (+ i 1))))
      v)))


(define closure-body
  (lambda (c)
    (vector-ref c 0)))
(define index-closure
  (lambda (c n)
    (vector-ref c (+ n 1))))

(define find-sets
  (lambda (x v)
    (cond
      [(symbol? x) '()]
      [(pair? x)
       (record-case x
                    [quote (obj) '()]
                    [lambda (vars body)
                      (find-sets body (set-minus v vars))]
                    [if (test then else)
                        (set-union (find-sets test v)
                                   (set-union (find-sets then v)
                                              (find-sets else v)))]
                    [set! (var x)
                          (set-union (if (set-member? var v) (list var) '())
                                     (find-sets x v))]
                    [call/cc (exp) (find-sets exp v)]
                    [else
                     (recur next ([x x])
                       (if (null? x)
                           '()
                           (set-union (find-sets (car x) v)
                                      (next (cdr x)))))])]
      [else '()])))

(define make-boxes
  (lambda (sets vars next)
    (recur f ([vars vars] [n 0])
      (if (null? vars)
          next
          (if (set-member? (car vars) sets)
              (list 'box n (f (cdr vars) (+ n 1)))
              (f (cdr vars) (+ n 1)))))))


(define find-free
  (lambda (x b)
    (cond
      [(symbol? x) (if (set-member? x b) '() (list x))]
      [(pair? x)
       (record-case x
                    [quote (obj) '()]
                    [lambda (vars body)
                      (find-free body (set-union vars b))]
                    [if (test then else)
                        (set-union (find-free test b)
                                   (set-union (find-free then b)
                                              (find-free else b)))]
                    [set! (var exp)
                          (set-union (if (set-member? var b) '() (list var))
                                     (find-free exp b))]
                    [call/cc (exp) (find-free exp b)]
                    [else
                     (recur next ([x x])
                       (if (null? x)
                           '()
                           (set-union (find-free (car x) b)
                                      (next (cdr x)))))])]
      [else '()])))



(define shift-args
  (lambda (n m s)
    (recur nxtarg ([i (- n 1)])
      (unless (< i 0)
        (index-set! s (+ i m) (index s i))
        (nxtarg (- i 1))))
    (- s m)))





                                                              