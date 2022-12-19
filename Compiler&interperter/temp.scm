
(define (make-arg-str name str)
   (string-append "<" str (number->string (+ 1 num)) ">"))

(define (make-short-argument arg)
   (cond ((not (eq? -1 (get-box-num arg box-table)))
          (make-arg-str (get-box-num arg box-table) "box"))
         ((not (eq? -1 (get-closure-number arg closure-table)))
          (make-arg-str (get-closure-number arg closure-table) "clo"))
         (else arg)))
