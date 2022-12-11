# 2022年4月20日
## 上传了第一版的meta-system
1. define-act: 定义想要被插入断点的函数 eg: (define-act eval-if)
2. 手动加入了所有可被插入断点的函数的定义



# 2022年8月10日
## 函数对应记录

interpreter                         ->          VM

(eval-self-evaluating exp)          ->          'act-constant
(eval-quotation exp)                ->          'act-constant
(eval-variable exp env)             ->          'act-variable    
(eval-if exp env)                   ->          'act-if
(eval-if-test test env)             ->          'act-test
(eval-if-then then env)             ->          'act-then
(eval-if-else else env)             ->          'act-else
(eval-lambda exp env)               ->          'act-lambda
(eval-application exp env)          ->          'act-application
(eval-application-args args env)    ->          'act-args
(eval-application-body name env)    ->          'act-fun-body
                                                'act-apply
                                                'act-return






((lambda () (let ((make-closure
        (lambda (num)
          (lambda () num))))
  (let ((c1 (make-closure 10))
       (c2 (make-closure 20)))
  (c1)))))


((lambda (f1 f2) ((lambda() (f2))) ) (lambda (arg) arg) (lambda() 999))

(eval1 '((lambda (make-closure)
   ((lambda (c1 c2) (c1))
    (make-closure 10)
    (make-closure 20)))
 (lambda (num) (lambda () num))))

'((lambda (make-closure)
   ((lambda (c1 c2) (make-closure 999))
    (make-closure 10)
    (make-closure 20)))
 (lambda (num) (lambda () num)))

'((lambda (make-closure)
   ((lambda (c1) (c1))
    (make-closure 10)))
 (lambda (num) (lambda () num)))


((lambda (a) a) ((lambda() 99)))

((lambda (c) (c))
((lambda(num) (lambda() num)) 99))



;3.2.2  Applying Simple Procedures
'((lambda (square)
   ((lambda (sum-of-squares)
      ((lambda(a)
         (sum-of-squares (+ a 1) (* a 2))) 5))
      (lambda(x y) (+ (square x) (square y)))))
 (lambda(x) (* x x)))

;3.2.3  Frames as the Repository of Local State
'((lambda(make-withdraw) ((make-withdraw 100) 99))
(lambda(balance)
  (lambda (amount)
    (if (>= balance amount)
        (- balance amount)
        "Insufficient funds"))))

'((lambda (fact n)
   (fact fact n))
 (lambda (self n)
   (if (= n 0)
       1
       (* n (self self (- n 1)))))
 5)
 
因为闭包的实现方式是讲参数再次插入到