
;(meta '((lambda(a b c) (if a b c)) 11 22 33))

;https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book-Z-H-21.html#%_sec_3.2.2

; (define square
;   (lambda(x)
;     (* x x)))
; (define sum-of-squares
;   (lambda(x y)
;     (+ (square x) (square y))))
; (define f
;   (lambda(a)
;     (sum-of-squares (+ a 1) (* a 2))))
; (f 5)

(glo-define '(define square (lambda(x)
  (* x x))))
(glo-define '(define sum-of-squares (lambda(x y)
  (+ (square x) (square y)))))
(glo-define '(define f (lambda(a)
  (sum-of-squares (+ a 1) (* a 2)))))
(meta '(f 5))

;https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book-Z-H-21.html#%_sec_3.2.3

; (define make-withdraw
;   (lambda (balance)
;     (lambda (amount)
;       (if (>= balance amount)
;           ((lambda (dum) balance)
;            (set! balance (- balance amount)))
;           "Insufficient funds"))))
; (define w1 (make-withdraw 100))
; (define w2 (make-withdraw 200))
; (w1 50)

(glo-define '(define make-withdraw (lambda (balance) (lambda (amount) (if (>= balance amount) ((lambda (dum) balance) (set! balance (- balance amount))) "Insufficient funds")))))
(glo-define '(define w1 (make-withdraw 100))) 
(glo-define '(define w2 (make-withdraw 200)))
(meta '(w1 50))


(meta '( (lambda(a1 a2 a3)
          (if a1 (+ a2 a3) (* a2 a3)))
          #f ((lambda() 99)) ((lambda(n) (* 2 n)) 55) ))


(meta '( (lambda(a) (set! a 99)) 44))

(meta '((lambda (a0 a1 a2)
    ((lambda (b0 b1 b2)
       ((lambda (c0 c1 c2)
          ((lambda (d0 d1 d2)
             (-  (* c1 a2) (+ a2 b2)))
           30 31 32))
        20 21 22))
     10 11 12))
  1 2 3))


(meta '((lambda (x f)
               ((lambda (dum) x)
                (f (lambda ()
                     ((lambda (dum) x)
                      (set! x (+ x 1)))))))
             10
             (lambda (g) (g))))

(breakpoint-switch 'act-application)
(meta '((lambda(a1 a2 a3 a4)
  ((lambda (b)
     ((lambda (c)
        (+ b a4)) 99)) 88)) 77 66 55 44))



