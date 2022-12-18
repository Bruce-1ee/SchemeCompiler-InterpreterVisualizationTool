'((lambda (f) ((lambda(n) (f n)))) (lambda(a) a))

'(if a b c)

(make-label '(if a b c))

(eval1 '((lambda (f) ((lambda(n) (f n)) 99)) (lambda(a) a)))

(test-fun (make-label '((lambda (f) ((lambda(n) (f n)) 99)) (lambda(a) a))))

str = testArg.toString();

parseCode(str);