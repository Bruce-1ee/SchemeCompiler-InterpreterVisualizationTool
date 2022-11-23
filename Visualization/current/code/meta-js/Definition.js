meta = '(define true #t)\n(define false #f)\n(define (error str . exp)\n  (display str)\n  (display " ")\n  (if (null? exp)\n    (display " ")\n    (display exp))\n  (newline))\n\n(define (send-stack stack s)\n  (js-call (js-eval "updateStack") stack s))\n\n(define (eval-update name expr)\n    (js-call (js-eval "updateEvalInfo") name expr))\n\n(define (add-frame-counter)\n  (js-call (js-eval "addFrameCounter")))\n\n(define (sub-frame-counter)\n  (js-call (js-eval "subFrameCounter")))\n\n(define (make-envrionment-frame)\n  (js-call (js-eval "makeEnvrionmentFrame")))\n\n(define (send-arguments-to-js args vals body type)\n  (js-call (js-eval "getArgumentsFromScheme") args vals body type))\n\n\n(define (test-fun val)\n  (js-call (js-eval "testFun") val))\n\n;(define (test-arg vals args)\n;  (js-call (js-eval "newFrame") vals args))\n\n(define (interpreter-new-frame vals args)\n  (js-call (js-eval "interNewFrame") vals args))\n\n(define (stack-createFrame)\n (js-invoke (js-ref (js-eval "view") "stack") "createFrame" ))\n\n\n; view.stack.push(val)\n(define (js-push-element-into-stack val)\n  (js-invoke (js-ref (js-eval "view") "stack") "push" val))\n\n  ; view.stack.push(val type)\n(define (js-stack-push val type)\n  (js-invoke (js-ref (js-eval "view") "stack") "push" val type))\n\n; view.stack.pushArgument(val)\n(define (js-push-argument val)\n  (js-invoke (js-ref (js-eval "view") "stack") "pushArgument" val))\n\n(define (js-pop-element)\n  (js-invoke (js-ref (js-eval "view") "stack") "pop"))\n\n; view.stack.pushStaticLink(val)\n(define (js-push-pushStaticLink val)\n  (js-invoke (js-ref (js-eval "view") "stack") "pushStaticLink" val))\n\n(define (stack-deleteFrame)\n (js-invoke (js-ref (js-eval "view") "stack") "deleteFrame" ))\n\n\n;view.closure.createClosure(l)\n(define (js-closure-createClosure val)\n  (js-invoke (js-ref (js-eval "view") "closure") "createClosure" val))\n\n;view.environment.addClosure(l)\n(define (js-env-addClosure val)\n  (js-invoke (js-ref (js-eval "view") "environment") "addClosure" val))\n\n;drawexpression(exp)\n(define (js-draw-expression exp)\n  (js-call (js-eval "drawexpression") exp))\n\n\n;callFrame.evalCallFunc()\n(define (js-call-frame-eval-add num)\n  (js-invoke (js-eval "callFrame") "evalCallFunc" num))\n\n;callFrame.evalReturn()\n(define (js-call-frame-eval-sub num)\n  (js-invoke (js-eval "callFrame") "evalReturn" num))\n\n;callFrame.vmCallFunc()\n(define (js-call-frame-vm-add num)\n  (js-invoke (js-eval "callFrame") "vmCallFunc" num))\n\n;callFrame.vmReturn()\n(define (js-call-frame-vm-sub)\n  (js-invoke (js-eval "callFrame") "vmReturn"))\n\n;callFrame.showFrame()\n(define (js-call-frame-show)\n  (js-invoke (js-eval "callFrame") "showFrame"))\n\n\n\n(define (draw-interpreter-info info)\n  (js-call (js-eval "drawInterpreterInfo") info))\n\n(define (draw-VM-Info info)\n  (js-call (js-eval "drawVMInfo") info))\n\n\n(define (compile exp env next)\n  (cond ((self-evaluating? exp) (compile-self-evaluating exp next))\n        ((quoted? exp) (compile-quoted exp next))\n        ((variable? exp) (compile-variable exp env next))\n        ;((definition? exp) (compile-definition exp env next))\n        ;((let? exp) (compile-let exp env next))\n        ((if? exp) (compile-if exp env next))\n        ((lambda? exp) (compile-lambda exp env next))\n        ((application? exp)\n         (compile-application exp env next))\n        (else (error "Unknown expression type -- COMPILE" exp))))\n\n;=============tagged-list========================\n(define (self-evaluating? exp)\n  (cond ((number? exp) true)\n        ((string? exp) true)\n        ((boolean? exp) true)\n        (else false)))\n(define (quoted? exp)\n  (tagged-list? exp \'quote))\n(define (variable? exp)\n  (symbol? exp))\n;(define (assignment? exp)\n;  (tagged-list? exp \'set!))\n;(define (definition? exp)\n;  (tagged-list? exp \'define))\n;(define (let? exp)\n;  (tagged-list? exp \'let))\n(define (if? exp)\n  (tagged-list? exp \'if))\n(define (lambda? exp)\n  (or (tagged-list? exp \'lambda)\n      (tagged-list? exp \'slambda)\n      (tagged-list? exp \'clambda)))\n\n(define (application? exp)\n  (pair? exp))\n\n(define (tagged-list? exp tag)\n  (if (pair? exp)\n      (eq? (car exp) tag)\n      false))\n\n;===========compiler-process=====================\n(define (compile-self-evaluating val next)\n  (list \'constant val next))\n(define (compile-quoted exp next)\n  (list \'constant (cadr exp) next))\n(define compile-variable\n  (lambda (exp env next)\n    (compile-lookup exp env\n                    (lambda (n m) (list \'refer n m next))\n                    (lambda (n) (list \'refer-free n next))\n                    (lambda () (list \'refer-global exp next)))))\n(define compile-lookup\n  (lambda (x e return return-free return-global)\n    (let ((free (cdr e)))\n      (let nxtrib ((e (car e)) (rib 0) (ccb #f))\n        (if (null? e)\n            (return-global)\n            (let nxtelt ((vars (car e)) (n 0) (ccb ccb))\n              (cond ((null? vars) (nxtrib (cdr e) (+ rib 1) ccb))\n                    ((eq? vars \'CB) (nxtrib (cdr e) rib #t))\n                    ((eq? (car vars) x)\n                     (if ccb\n                         (return-free (- (length free) (length (memq x free))))\n                         (return rib n)))\n                    (else (nxtelt (cdr vars) (+ n 1) ccb)))))))))\n(define (compile-if exp env next)\n  (let ((test (cadr exp)) (then (caddr exp)) (else (caddr exp)))\n    (let ((thenc (compile-then then env next))\n          (elsec (compile-else else env next)))\n      (compile-test test env (list \'test thenc elsec)))))\n\n(define (compile-test test env next)\n  (compile test env next))\n\n(define (compile-then then env next)\n  (compile then env next))\n\n(define (compile-else else env next)\n  (compile else env next))\n\n(define (compile-lambda exp env next)\n  (cond [(eq? (car exp) \'slambda) (compile-slambda exp env next)]\n        [(eq? (car exp) \'clambda) (compile-clambda exp env next)]\n        [else (error \'unknown_lambda)]))\n(define (compile-slambda exp env next)\n  (let ((vars (cadr exp)) (body (caddr exp)))\n    (list \'functional\n          (compile body\n                   (cons (compile-extend (car env) vars)\n                         (cdr env))\n                   (list \'return (+ (length vars) 1)))\n          next)))\n(define (compile-clambda exp env next)\n  (let ((vars (cadr exp)) (body (caddr exp)))\n    (let ((free (remove-global (car env) (find-free body vars))))\n      (collect-free free env\n                    (list \'close\n                          (length free)\n                          (compile body\n                                   (cons (compile-extend (compile-extend (car env) \'CB) vars) ;;;\n                                         free)\n                                   (list \'return\n                                         (length vars)))\n                          next)))))\n(define (compile-application exp env next)\n  (let loop ((args (cdr exp))\n             (c (compile-fun-body exp env)))\n    (if (null? args)\n        (list \'frame\n              next\n              (compile-arg c))\n        (loop (cdr args)\n              (compile (car args)\n                       env\n                       (list \'argument c))))))\n\n(define (compile-fun-body exp env)\n  (compile (car exp) env \'(apply)))\n(define (compile-arg c) c)\n\n;==========helping-function=========================\n(define compile-extend\n  (lambda (e r)\n    (cons r e)))\n(define (remove-global e free) ;;;\n  (define (flatten l)\n    (cond ((null? l) l)\n          ((not (pair? l)) (list l))\n          (else (append (flatten (car l)) (flatten (cdr l))))))\n  (set-intersect (flatten e) free))\n(define find-free\n  (lambda (x b)\n    (cond\n      ((symbol? x) (if (set-member? x b) \'() (list x)))\n      ((pair? x)\n       (case (car x)\n         ((quote) \'())\n         ((lambda slambda clambda)\n          (let ((vars (cadr x))\n                (body (caddr x)))\n            (find-free body (set-union vars b))))\n         ((if)\n          (let ((test (cadr x))\n                (then (caddr x))\n                (els (cadddr x)))\n            (set-union (find-free test b)\n                       (set-union (find-free then b)\n                                  (find-free els b)))))\n         ((call/cc)\n          (let ((exp (cadr x))) (find-free exp b)))\n         (else\n          (let next ((x x))\n            (if (null? x)\n                \'()\n                (set-union (find-free (car x) b)\n                           (next (cdr x))))))))\n      (else \'()))))\n(define collect-free\n  (lambda (vars e next)\n    (if (null? vars)\n        next\n        (collect-free (cdr vars) e\n                      (compile-variable (car vars) e\n                                        (list \'argument next))))))\n\n(define set-member?\n  (lambda (x s)\n    (cond\n      ((null? s) #f) ;;;\n      ((eq? x (car s)) #t) ;;;\n      (else (set-member? x (cdr s))))))\n\n(define set-cons\n  (lambda (x s)\n    (if (set-member? x s)\n        s\n        (cons x s))))\n\n(define set-union\n  (lambda (s1 s2)\n    (if (null? s1)\n        s2\n        (set-union (cdr s1) (set-cons (car s1) s2)))))\n\n(define set-minus\n  (lambda (s1 s2)\n    (if (null? s1)\n        \'()\n        (if (set-member? (car s1) s2)\n            (set-minus (cdr s1) s2)\n            (cons (car s1) (set-minus (cdr s1) s2))))))\n\n(define set-intersect\n  (lambda (s1 s2)\n    (if (null? s1)\n        \'()\n        (if (set-member? (car s1) s2)\n            (cons (car s1) (set-intersect (cdr s1) s2))\n            (set-intersect (cdr s1) s2)))))\n\n(define (preprocess x funpos?)\n  (cond ((symbol? x) x)\n        ((pair? x)\n         (case (car x)\n           ((quote) x)\n           ((lambda)\n            `(,(if funpos? \'slambda \'clambda)\n              ,(cadr x)\n              ,@(map (lambda (e) (preprocess e #f)) (cddr x)))) \n           ((if)\n            `(if ,(preprocess (cadr x) #f)\n                 ,(preprocess (caddr x) funpos?)\n                 ,(preprocess (cadddr x) funpos?)))\n           ((set!)\n            `(set! ,(cadr x) ,(preprocess (caddr x) #f)))\n           (else\n            `(,(preprocess (car x) #t)\n              ,@(map (lambda (x) (preprocess x #f))\n                     (cdr x))))))\n        (else x)))\n\n(define (sc exp)\n  (compile (preprocess exp #f) \'(() . ()) \'(halt)))\n\n(define VM\n  (lambda (a x f c s) ;; (a x e s)\n    (case (car x)\n      ((halt) a)\n      ((refer) (VM-refer a x f c s))\n      ((refer-free) (VM-refer-free a x f c s))\n      ((refer-global) (VM-refer-global a x f c s))\n      ((constant) (VM-constant a x f c s))\n      ((functional) (VM-functional a x f c s))\n      ((close) (VM-close a x f c s))\n      ((test) (VM-test a x f c s))\n      \n      ;((assign) \n      ;(let ((n (cadr x))\n      ;      (m (caddr x))\n      ;      (x (cadddr x)))\n      ;  (index-set! (find-link n f) m a)\n      ;  (VM a x f c s)))\n      ;((assign-global) ;;;\n      ; (error "not yot implemented"))\n      \n      ((frame) (VM-frame a x f c s))\n      ((argument) (VM-argument a x f c s))\n      ((apply) (VM-apply a x f c s))\n      ((return) (VM-return a x f c s))\n      (else (VM-otherwise a x f c s)))))\n\n(define (VM-constant a x f c s)\n  (let ((obj (cadr x))\n        (x (caddr x)))\n    (VM obj x f c s)))\n\n(define (VM-refer a x f c s)\n  (let ((n (cadr x))\n        (m (caddr x))\n        (x (cadddr x)))\n    (VM (index (find-link n f) m) x f c s)))\n\n(define (VM-refer-free a x f c s)\n  (let ((n (cadr x))\n        (x (caddr x)))\n    (VM (index-closure c n) x f c s)))\n\n(define (VM-refer-global a x f c s)\n  (let ((var (cadr x))\n        (x (caddr x)))\n    (VM (refer-global-var var) x f c s)))\n\n(define (VM-test a x f c s)\n  (let ((then (cadr x))\n        (els (caddr x)))\n    (VM a (if a then els) f c s)))\n\n(define (VM-functional a x f c s)\n  (let ((body (cadr x))\n        (x (caddr x)))\n    (VM (functional body f) x f c s)))\n\n(define (VM-close a x f c s)\n  (let ((n (cadr x))\n        (body (caddr x))\n        (x (cadddr x)))\n    (VM (closure body n s) x f c (- s n))))\n\n(define (VM-frame a x f c s)\n  (let ((ret (cadr x))\n        (x (caddr x)))\n    (VM a x f c (push ret (push f (push c s))))))\n\n(define (VM-argument a x f c s)\n  (let ((x (cadr x)))\n    (VM a x f c (push a s))))\n\n(define (VM-apply a x f c s)\n  (case (car a)\n    ((functional) ;;;\n     (VM-apply-functional a x f c s))\n    ((closure) ;;;\n     (VM a (closure-body (cdr a)) s (cdr a) s)) ;;;\n    ((primitive) ;;;\n     (VM-apply-primitive a x f c s))\n    (else\n     (error "Not a function"))))\n\n(define (VM-apply-functional a x f c s)\n  (let ((body (cadr a))\n        (link (caddr a)))\n    (VM a body s c (push link s))))\n\n(define (VM-apply-primitive a x f c s)\n  (let ((s (+ s 1)) ;; (push link s)\n           (primfun (cadr a)))\n       (primfun s)))\n\n(define (VM-return a x f c s)\n  (let ((n (cadr x)))\n    (let ((s (- s n)))\n      (VM a (index s 0) (index s 1) (index s 2) (- s 3)))))\n\n(define (VM-otherwise a x f c s)\n      (error "unknow inst"))\n \n(define functional\n  (lambda (body e)\n    (list \'functional body e))) ;;;\n\n(define (primitive-fun natfun) ;;;\n  (list \'primitive natfun))\n\n(define stack (make-vector 1000))\n\n(define push\n  (lambda (x s)\n    (vector-set! stack s x)\n    (+ s 1)))\n\n(define index\n  (lambda (s i)\n    (vector-ref stack (- (- s i) 1))))\n\n(define index-set!\n  (lambda (s i v)\n    (vector-set! stack (- (- s i) 1) v)))\n\n(define closure-bak\n  (lambda (body n s)\n    (let ((v (make-vector (+ n 1))))\n      (vector-set! v 0 body)\n      (let f ((i 0))\n        (unless (= i n)\n          (vector-set! v (+ i 1) (index s i))\n          (f (+ i 1))))\n      (cons \'closure v)))) ;;;\n\n(define closure\n  (lambda (body n s)\n    (let ((v (make-clo body n s)))\n      (cons \'closure v)))) ;;;\n  \n(define (make-clo body n s)\n  (let ((v (make-vector (+ n 1))))\n      (vector-set! v 0 body)\n      (let f ((i 0))\n        (unless (= i n)\n          (vector-set! v (+ i 1) (index s i))\n          (f (+ i 1))))\n    v))\n\n(define closure-body\n  (lambda (c)\n    (vector-ref c 0)))\n\n(define index-closure\n  (lambda (c n)\n    (vector-ref c (+ n 1))))\n\n(define find-link\n  (lambda (n e)\n    (if (= n 0) e\n        (find-link (- n 1) (index e -1)))))\n\n;;\n\n(define (prim-return retval s)\n  (VM retval (index s 0) (index s 1) (index s 2) (- s 3)))\n\n(define the-global-environment ;;;\n  `((foo . ,(primitive-fun (lambda (s)\n                             (let ((ans 10))\n                               ;; (return 3)\n                               (write s)(newline)\n                               (write stack)(newline)\n                               (prim-return ans (- s 3))))))\n    (+ . ,(primitive-fun (lambda (s)\n                           (let ((ans (+ (index s 1)\n                                         (index s 2))))\n                             (prim-return ans (- s 3))))))\n    (- . ,(primitive-fun (lambda (s)\n                           (let ((ans (- (index s 1)\n                                         (index s 2))))\n                             (prim-return ans (- s 3))))))))\n\n(define (refer-global-var var) ;;;\n  (cond ((assq var the-global-environment)\n         => (lambda (p) (cdr p)))\n        (else\n         (error "unbound symbol"))))\n\n(define run\n  (lambda (exp)\n    (VM \'() (compile (preprocess exp #f) \'(() . ()) \'(halt)) 0 \'() 0)))\n\n;; ===============================================\n;; ===============================================\n;; ===============================================\n;; ===============================================\n;; ===============================================\n(define exec\n  (lambda (exp env)\n    (call/cc\n     (lambda (quit)\n       (cond ((self-evaluating? exp)(eval-self-evaluating exp))\n             ((variable? exp)(eval-variable exp env))\n             ((quoted? exp) (eval-quotation exp))\n             ;((assignment? exp) (eval-assignment exp env))\n             ;((definition? exp) (eval-definition exp env))\n             ;((let? exp) (eval-let exp env))\n             ((if? exp) (eval-if exp env))\n             ((lambda? exp) (eval-lambda exp env))\n             ;((begin? exp) (eval-sequence (cdr exp) env)) ;new\n             ((application? exp) (eval-application exp env))\n             (else (error "Unknown expression type -- EXEC" exp)))))))\n\n\n(define eval-extend \n  (lambda (env vars vals)\n    (cons (cons vars vals) env)))\n(define (eval-lookup var env)\n  (define (env-loop env)\n    (define (scan vars vals)\n      (cond ((null? vars)\n             (env-loop (cdr env)))\n            ((eq? var (car vars))\n             (car vals))\n            (else (scan (cdr vars) (cdr vals)))))\n    (if (eq? env \'())\n        (error "Unbound variable" var)\n        (let ((frame (car env)))\n          (scan (car frame)\n                (cdr frame)))))\n  (env-loop env))\n\n\n(define GE\n  (eval-extend\n   \'()\n   \'(+ - * = console-log) ; <- ( + - * / )\n   (map (lambda (f) (cons \'primitive f))\n        (list + - * = console-log))))  ; <- ( + - * / )\n\n\n;==========new==========\n(define (begin? exp) (tagged-list? exp \'begin))\n(define (last-exp? seq) (null? (cdr seq)))\n(define (first-exp seq) (car seq))\n(define (rest-exps seq) (cdr seq))\n\n(define (eval-sequence exps env)\n  (cond ((last-exp? exps) (exec (first-exp exps) env))\n        (else (exec (first-exp exps) env)\n              (eval-sequence (rest-exps exps) env))))\n\n\n\n\n;==========解析用函数==========\n\n(define (eval-self-evaluating exp) exp)\n(define (eval-quotation) (cadr exp))\n(define (eval-variable exp env) (eval-lookup exp env))\n(define (eval-assignment exp env) (let ((var (cadr exp))\n        (val (caddr exp)))\n    (set-car! (eval-lookup var env) (exec val env))))\n\n(define (eval-definition exp env)\n  (if (not (symbol? (cadr exp)))\n      (error "Not a variable -- DEFINE"))\n      (define-variable! (cadr exp) (exec (caddr exp) env) env)\n  \'ok)\n(define (define-variable! var val env)\n  (let ((frame (car env)))\n    (define (scan vars vals)\n      (cond ((null? vals) (set-car! frame (cons var (car frame))) (set-cdr! frame (cons val (cdr frame))))\n            ((eq? var (car vars)) (set-car! vals val))\n            (else (scan (cdr vars) (cdr vals)))))\n    (scan (car frame) (cdr frame))))\n\n(define (eval-if exp env)\n  (let ((test (cadr exp))\n        (then (caddr exp))\n        (else (cadddr exp)))\n    (if (eval-if-test test env)\n        (eval-if-then then env)\n        (if (null? else)\n            #f\n            (eval-if-else else env)))))\n(define (eval-if-test test env) (exec test env))\n(define (eval-if-then then env) (exec then env))\n(define (eval-if-else else env) (exec else env))\n(define (eval-lambda-bak exp env)\n  (let ((vars (cadr exp))\n        (body (caddr exp)))\n    (list \'function vars body env)))\n\n(define (eval-lambda exp env)\n  (cond [(eq? (car exp) \'slambda) (eval-slambda exp env)]\n        [(eq? (car exp) \'clambda) (eval-clambda exp env)]\n        [else (error \'unknown_lambda)]))\n\n(define (eval-slambda exp env)\n  (let ((vars (cadr exp))\n        (body (caddr exp)))\n    (list \'function vars body env)))\n\n(define (eval-clambda exp env)\n  (let ((vb (eval-clambda-helping-fun exp)))\n    ;; draw make-closure \n    (list \'function (car vb) (cadr vb) env)))\n\n(define (eval-clambda-helping-fun exp)\n    (let ((vars (cadr exp))\n          (body (caddr exp)))\n        (list vars body)))\n\n(define (eval-application exp env)\n  (let ((args (eval-application-args (cdr exp) env))\n        (body (eval-application-body (car exp) env)))\n    (eval-application-apply body args env)))\n\n(define (eval-application-args args env)\n  (reverse (map (lambda(x) (exec x env)) (reverse args))))\n\n(define (eval-application-body name env) (exec name env))\n\n(define (eval-application-apply func arguments env)\n  (case (car func)\n    ((primitive)\n     (eval-application-apply-primitive func arguments))\n    ((function)\n     (eval-application-apply-functional func arguments))\n    (else (error "Not a function -- eval-application-apply"))))\n\n(define (eval-application-apply-functional func arguments)\n  (exec (caddr func)\n        (eval-extend (cadddr func) (cadr func) arguments)))\n\n(define (eval-application-apply-primitive func args)\n  (apply (cdr func) args))\n\n\n(define (eval-let exp env)\n  (let ((e (let->lambda exp)))\n        (exec e env)))\n\n;==========new==========\n(define getargs\n  (lambda (args ret)\n    (if (null? args)\n        ret\n        (getargs (cdr args) (append ret (list (car (car args))))))))\n(define getvals\n  (lambda (args ret)\n    (if (null? args)\n        ret\n        (getvals (cdr args) (append ret (list (cadr (car args))))))))\n\n(define let->lambda\n  (lambda (exp)\n    (let* ((args (cadr exp))\n          (body (caddr exp))\n          (lambda-args (getargs args \'()))\n          (lambda-vals (getvals args \'())))\n    (cons (list \'lambda lambda-args body) lambda-vals))))\n\n(define (eval1 exp) (exec (preprocess exp #f) GE))\n\n\n\n;====macro======\n\n(define (cc)\n  (call/cc\n   (lambda (quit)\n     (set! resume-meta quit)))) \n;(cc)\n\n(define (make-jumppoint-eval)\n  (call/cc (lambda (breakpoint)\n             (set! exec-k breakpoint)\n             (resume-meta \'ok))))\n\n(define (make-jumppoint-vm)\n  (call/cc (lambda (breakpoint)\n             (set! vm-k breakpoint)\n             (resume-meta \'ok))))\n\n\n(define-macro define-act-eval\n  (lambda (fun info act)\n    `(let* ((org-fun ,fun))\n       (set! ,fun\n             (lambda (arg . restarg)\n              (set! inte-info ,act)\n              (cond ((break? inte-info) (set! break #t)))\n              (make-jumppoint-eval)\n              (draw-interpreter-info ,info)\n              ;(console-log \'eval:)(console-log ,info)(newline)\n              (apply org-fun (cons arg restarg)))))))\n\n(define-macro define-act-compiler\n  (lambda (fun pseins)\n    `(let* ((org-fun ,fun))\n       (set! ,fun\n             (lambda (arg . restarg)\n               (list ,pseins\n                     (apply org-fun (cons arg restarg))))))))\n\n\n(define-macro define-vm-otherwise\n  (lambda ()\n    `(let* ((org-fun VM-otherwise))\n       (set! VM-otherwise\n             (lambda (a x f c s)\n               (if (get-act (car x)) ;如果存在于acts的VECTOR里那么就执行,不然就报错\n                   (begin\n                    (set! vm-info (car x))\n                    (cond ((break? vm-info) (set! break #t)))\n                    (make-jumppoint-vm)\n                    (draw-VM-Info (car x))\n                    ;(console-log \'vm:)(console-log (car x)) (newline)\n                    (VM a (cadr x) f c s))\n                   (org-fun a x f c s)))))))\n\n\n(define-macro define-vm-make-clo\n  (lambda ()\n    `(let* ((org-fun make-clo))\n       (set! make-clo\n             (lambda (body n s)\n              (let ((v (org-fun body n s)))\n                (test-fun v)\n                v\n              )\n               )))))\n\n\n(define-macro define-eval-exec\n  (lambda ()\n    `(let* ((org-fun exec))\n       (set! exec\n             (lambda (exp env)\n              ;发送exp信息\n              (if (list? exp)\n                  (js-draw-expression (car exp))\n                  (js-draw-expression exp))\n              (org-fun exp env)\n \n               )))))\n\n;eval-application　(length (cdr (reverse env)))\n(define-macro define-eval-application\n  (lambda ()\n    `(let* ((org-fun eval-application))\n       (set! eval-application\n             (lambda (exp env)\n              (js-call-frame-eval-add (length  env))\n              ;(js-call-frame-eval-add)\n              (js-call-frame-show)\n              (let ((res (org-fun exp env)))\n                (js-call-frame-eval-sub (length  env))\n                (js-call-frame-show)\n                res\n              )\n              \n \n               )))))\n\n\n\n\n\n;===============画面相关=================\n\n(define-macro embed-vm-draw-find-link\n  (lambda ()\n    `(let* ((org-fun find-link))\n       (set! find-link\n             (lambda (n e)\n              (draw-VM-Info "finding link")\n              (org-fun n e))))))\n\n(define-macro embed-eval-draw-frame-functional\n  (lambda ()\n    `(let* ((org-fun eval-application-apply-functional))\n       (set! eval-application-apply-functional\n             (lambda (func arguments)\n\n              (interpreter-new-frame arguments func)\n              (org-fun func arguments))))))\n\n(define-macro embed-vm-draw-frame \n  (lambda ()\n    `(let* ((org-fun VM-frame))\n       (set! VM-frame\n             (lambda (a x f c s)\n              (js-call-frame-vm-add (trav-link f 1))\n              (js-call-frame-show)\n              (stack-createFrame)\n              (js-stack-push c "closure")\n              (js-stack-push f "frame")\n              (js-stack-push (cadr x) "return")\n              (org-fun a x f c s))))))\n\n(define-macro embed-vm-draw-arg-push \n  (lambda ()\n    `(let* ((org-fun VM-argument))\n       (set! VM-argument\n             (lambda (a x f c s)\n              (js-stack-push a "argument")\n              (org-fun a x f c s))))))\n\n(define-macro embed-vm-draw-apply-functional \n  (lambda ()\n    `(let* ((org-fun VM-apply-functional))\n       (set! VM-apply-functional\n             (lambda (a x f c s)\n              (js-stack-push (caddr a) "link")\n              (org-fun a x f c s))))))\n\n(define-macro embed-vm-draw-apply-primitive\n  (lambda ()\n    `(let* ((org-fun VM-apply-primitive))\n       (set! VM-apply-primitive\n             (lambda (a x f c s)\n              (js-stack-push 0 "empty")\n              (org-fun a x f c s))))))\n\n(define-macro embed-vm-draw-return \n  (lambda ()\n    `(let* ((org-fun VM-return))\n       (set! VM-return\n             (lambda (a x f c s)\n              (js-call-frame-vm-sub)\n              (js-call-frame-show)\n              (loop (+ 3 (cadr x)) js-pop-element)\n              (stack-deleteFrame)\n              (org-fun a x f c s))))))\n\n(define-macro embed-vm-draw-prim-return \n  (lambda ()\n    `(let* ((org-fun prim-return))\n       (set! prim-return\n             (lambda (retval s)\n              (js-call-frame-vm-sub)\n              (js-call-frame-show)\n              (loop 6 js-pop-element) ;3 + 3 c，f，x， arg1,arg2,link\n              (stack-deleteFrame)\n              (org-fun retval s))))))\n\n(define-macro embed-vm-draw-make-clo \n  (lambda ()\n    `(let* ((org-fun make-clo))\n       (set! make-clo\n             (lambda (body n s)\n              (let ((v (org-fun body n s)))\n                (js-closure-createClosure v)\n                v\n              )\n               )))))\n\n(define-macro embed-eval-draw-clambda\n  (lambda ()\n    `(let* ((org-fun eval-clambda-helping-fun))\n       (set! eval-clambda-helping-fun\n             (lambda (exp)\n              (let ((vb (org-fun exp)))\n                (js-env-addClosure (list->vector (list (list->vector (car vb)) (cadr vb))))\n                vb\n              )\n               )))))\n\n(define-macro embed-vm-draw-close\n  (lambda ()\n    `(let* ((org-fun VM-close))\n       (set! VM-close\n             (lambda (a x f c s)\n              (loop (cadr x) js-pop-element)\n              (org-fun a x f c s))))))              \n\n\n(define (loop n fun)\n  (cond ((> n 0) (fun) (loop (- n 1) fun))))\n\n(define (trav-link f n)\n  (let ((ele (vector-ref stack f)))\n    (cond ((eq? f 0) n)\n          (else (trav-link ele (+ n 1))))))\n;定义伪指令\n\n;; 0 self-evaluating\n(define-act-compiler compile-self-evaluating \'act-constant)\n\n\n(define-act-eval eval-self-evaluating \'eval-self-evaluating \'act-constant)\n\n\n;; 1 quotation\n(define-act-compiler compile-quoted \'act-constant)\n\n\n(define-act-eval eval-quotation \'eval-quotation \'act-constant)\n\n\n;; 2 variable\n(define-act-compiler compile-variable \'act-variable)\n\n\n(define-act-eval eval-variable \'eval-variable \'act-variable)\n\n\n;; 3 if\n(define-act-compiler compile-if \'act-if)\n(define-act-compiler compile-test \'act-test)\n(define-act-compiler compile-then \'act-then)\n(define-act-compiler compile-else \'act-else)\n\n\n(define-act-eval eval-if \'eval-if \'act-if)\n(define-act-eval eval-if-test \'eval-test \'act-test)\n(define-act-eval eval-if-then \'eval-then \'act-then)\n(define-act-eval eval-if-else \'eval-else \'act-else)\n\n\n;; 4 lambda\n(define-act-compiler compile-lambda \'act-lambda)\n\n(define-act-eval eval-lambda \'eval-lambda \'act-lambda)\n\n\n;; 5 application\n(define-act-compiler compile-application \'act-application)\n(define-act-compiler compile-fun-body \'act-fun-body)\n(define-act-compiler compile-arg \'act-args)\n\n\n(define-act-eval eval-application \'eval-application \'act-application)\n(define-act-eval eval-application-args \'eval-arguments \'act-args)\n(define-act-eval eval-application-body \'eval-body \'act-fun-body)\n\n\n;; 6 others\n\n(define-vm-otherwise)\n\n(embed-eval-draw-frame-functional)\n(embed-vm-draw-frame)\n(embed-vm-draw-arg-push)\n(embed-vm-draw-apply-functional)\n(embed-vm-draw-return)\n\n(embed-vm-draw-apply-primitive)\n(embed-vm-draw-prim-return)\n\n(embed-vm-draw-make-clo) \n\n(embed-eval-draw-clambda)\n\n(define-eval-exec)\n\n(define-eval-application)\n(embed-vm-draw-close)\n\n;====break===\n\n(define breakpoints (vector (vector \'act-constant #f)\n                            (vector \'act-variable #f)\n                            (vector \'act-if #f)\n                            (vector \'act-test #f)\n                            (vector \'act-then #f)\n                            (vector \'act-else #f)\n                            (vector \'act-lambda #f)\n                            (vector \'act-application #f)\n                            (vector \'act-args #f)\n                            (vector \'act-fun-body #f)))\n\n(define (get-breakpoint-pos name)\n  (let loop ((n 0))\n    (cond ((>= n 10) (error \'bad_name))\n          ((eq? (vector-ref (vector-ref breakpoints n) 0) name) n)\n          (else (loop (+ n 1))))))\n\n(define (breakpoint-switch name)\n  (let* ((n (get-breakpoint-pos name))\n         (v (vector-ref breakpoints n)))\n    (vector-set! v 1 (not (vector-ref v 1)))))\n\n(define (breakpoint-on)\n  (let ((len (vector-length breakpoints)))\n    (let loop ((n 0))\n      (cond ((>= n len) \'done)\n            (else\n             (begin\n               (vector-set! (vector-ref breakpoints n) 1 #t)\n               (loop (+ n 1))))))))\n\n(define (breakpoint-off)\n  (let ((len (vector-length breakpoints)))\n    (let loop ((n 0))\n      (cond ((>= n len) \'done)\n            (else\n             (begin\n               (vector-set! (vector-ref breakpoints n) 1 #f)\n               (loop (+ n 1))))))))\n\n(define (break? name)\n   (let* ((n (get-breakpoint-pos name))\n          (v (vector-ref breakpoints n)))\n     (vector-ref v 1)))\n\n;========meta======\n\n\n(define exec-k #f)\n(define vm-k #f)\n(define resume-meta #f)\n\n\n(define vm-info #f)\n(define inte-info #f)\n\n(define next #f)\n(define break #f)\n\n(define acts (vector (vector \'act-constant 0 0)\n                     (vector \'act-variable 0 0)\n                     (vector \'act-if 0 0)\n                     (vector \'act-test 0 0)\n                     (vector \'act-then 0 0)\n                     (vector \'act-else 0 0)\n                     (vector \'act-lambda 0 0)\n                     (vector \'act-application 0 0)\n                     (vector \'act-args 0 0)\n                     (vector \'act-fun-body 0 0)))\n\n\n(define (get-act act-name)\n  (define (loop n)\n    (cond ((>= n (vector-length acts)) #f)\n          ((eq? (vector-ref (vector-ref acts n) 0) act-name) n)\n          (else (loop (+ n 1)))))\n  (loop 0))\n\n(define (get-target target)\n  (cond ((eq? target \'exec) 1)\n        ((eq? target \'vm) 2)\n        (else (error \'unknow_target))))\n\n(define (get-act-num act-name target)\n  (let ((t (get-target target))\n        (n (get-act act-name)))\n    (vector-ref (vector-ref acts n) t)))\n\n(define (act-add1 act-name target)\n  (let* ((t (get-target target))\n         (n (get-act act-name))\n         (v (vector-ref acts n))\n         (org (get-act-num act-name target)))\n    (vector-set! v t (+ org 1))))\n\n(define (is-same-position?)\n  (if (or (eq? vm-info #f)\n          (eq? inte-info #f))\n      #f\n      (and (eq? vm-info inte-info)\n           (eqv? (get-act-num vm-info \'vm)\n                 (get-act-num inte-info \'exec)))))\n\n\n\n(define (p t)\n  (console-log t) (newline))\n\n(define (meta program)\n  (call/cc\n   (lambda (break-meta)\n     (call/cc\n      (lambda (top)\n        (set! resume-meta top)))\n     \n     (call/cc\n      (lambda (c)\n        (cond ((eq? break #t)\n               (set! break #f)\n               (set! next c)\n               (break-meta \'ok\')))))\n      \n         (cond ((eq? vm-k #f) (run program))\n               ((eq? exec-k #f) (resume-meta (eval1 program)))\n               ((is-same-position?)\n                (act-add1 inte-info \'exec)\n                (exec-k \'ok))\n               (else\n                (act-add1 vm-info \'vm)\n                (vm-k \'ok))))))\n         \n  \n\n;(breakpoint-switch \'act-if)\n;(meta4 \'((lambda (a) (if a 99 0)) 1))\n\n;(breakpoint-on)\n;(breakpoint-off)\n\n\n;(run \'(if 1 2 3))\n;(eval1 \'(if 1 2 3))\n\n;(run \'(+ 1 1))\n;(eval1 \'(+ 1 1))\n\n;(run \'1)\n;(eval1 \'1)\n\n'