#lang scheme
(require compatibility/defmacro)

(define-macro break-after
  (lambda (exp)
    `(let ((ret (eval ,exp)))
       (display 'yeah) (newline)
       ret)))