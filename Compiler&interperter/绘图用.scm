(define-macro define-act-create-frame
  (lambda ()
    `(let* ((org-fun VM-frame))
       (set! VM-frame
             (lambda (a x f c s)
               (console-log "act-create-frame")(newline)
               (stack-new-frame)
               (org-fun a x f c s)
               (js-push-element-into-stack c)
               (js-push-element-into-stack f)
               (js-push-element-into-stack (cadr x))
               (make-breakpoint-vm)
               )))))


(define-macro define-act-push-element
  (lambda ()
    `(let* ((org-fun VM-argument))
       (set! VM-argument
             (lambda (a x f c s)
               (console-log "act-push-element")(newline)
               (org-fun a x f c s)
               (js-push-element-into-stack a))
               (make-breakpoint-vm)
               ))))
              
(define-macro define-act-delete-frame
  (lambda ()
    `(let* ((org-fun VM-return))
       (set! VM-return
             (lambda (a x f c s)
               (console-log "act-delete-frame")(newline)
               (org-fun a x f c s)
               (js-pop-element))
               (make-breakpoint-vm)
               (js-pop-element))
               (make-breakpoint-vm)
               (js-pop-element))
               (make-breakpoint-vm)
               ))))


