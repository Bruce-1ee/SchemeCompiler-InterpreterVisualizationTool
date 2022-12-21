
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