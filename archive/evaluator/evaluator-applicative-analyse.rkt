#lang sicp

;; Changes to the eval:
;; * Added support for let expressions as derived
;; * Added support for let* expressions
;; * Added support for named let expressions
;; * Added support for while constructs
;; * Added support for scan-out-defines (I think).
;; * Added support for letrec.
;; * Updated to use analyze

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


;; eval

(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp)
         (analyze-quoted exp))
        ((variable? exp)
         (analyze-variable exp))
        ((assignment? exp)
         (analyze-assignment exp))
        ((definition? exp)
         (analyze-definition exp))
        ((if? exp)
         (analyze-if exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence
          (begin-actions exp)))
        ((while? exp)
         (analyze-while exp))
        ((cond? exp)
         (analyze (cond->if exp)))
        ((let? exp)
         (analyze (let->combination exp)))
        ((let*? exp)
         (analyze (let*->nested-lets exp)))
        ((letrec? exp)
         (analyze (letrec->let exp)))
        ((application? exp)
         (analyze-application exp))
        (else
         (error "Unknown expression
                 type: ANALYZE"
                exp))))

;; cases

;; ;; given cases

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env)
    (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze
                (assignment-value exp))))
    (lambda (env)
      (set-variable-value!
       var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze
                (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence
                (lambda-body exp))))
    (lambda (env)
      (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

;; execute-application 'replaces' (meta-)apply.
;; For, the role of apply was to apply a procedure to some arguments.
;; This is exactly what execute-application does.
;; One feature of apply was checking the procedure was understood.
;; However, this has already been done with analyse.

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else (error "Unknown procedure type:
                      EXECUTE-APPLICATION"
                     proc))))

;; ;; custom cases

(define (analyze-while exp)
  (let ((w-p (analyze (while-cond exp)))
        (w-b (analyze (while-body exp))))
    (define (a-while env)
      (if (true? (w-p env))
          (begin (w-b env)
                 (a-while env))
          'false))
    (lambda (env) (a-while env))))

;; procedure arguments

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values
             (rest-operands exps)
             env))))

;; conditionals

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; sequences

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps)
                        env))))

;; assignments and definitions

(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; representing Expressions

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))


(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (make-definition name params body)
    (list 'define (cons name params) body))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate
                 consequent
                 alternative)
  (list 'if
        predicate
        consequent
        alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;; derived expressions

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp
                      (cond-actions first))
                     (expand-clauses
                      rest))))))

; let's

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-var-terms exp)
  (map car (cadr exp)))

(define (let-var-values exp)
  (map cadr (cadr exp)))

(define (let-body exp)
  (cddr exp))

(define (let->combination exp)
  (cond ((named-let? exp)
         (cons (make-lambda (named-let-var-terms exp)
                            (list
                             (make-definition (named-let-name exp)
                                              (named-let-var-terms exp)
                                              (named-let-body exp))
                             (cons (named-let-name exp)
                                   (named-let-var-terms exp))))
               (named-let-var-vals exp)))
        (else
         (cons (make-lambda (let-var-terms exp)
                            (let-body exp))
               (let-var-values exp)))))

;; star let's

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (make-let vars body)
  (append (list 'let vars) body))

(define (let*->nested-lets exp)
    (define (expand-to-lets var-terms var-values body)
      (if (null? (cdr var-terms))
	  (make-let (list (list (car var-terms) (car var-values)))
		    body)
	  (make-let (list (list (car var-terms) (car var-values)))
		    (list (expand-to-lets (cdr var-terms)
				    (cdr var-values)
				    body)))))
  (expand-to-lets (let-var-terms exp) (let-var-values exp) (let-body exp)))

;; named let's

  (define (named-let? exp)
    (variable? (cadr exp)))

  (define (named-let-vars exp)
    (caddr exp))

  (define (named-let-name exp)
    (cadr exp))

  (define (named-let-var-terms exp)
    (map car (caddr exp)))

  (define (named-let-var-vals exp)
    (map cadr (caddr exp)))

  (define (named-let-body exp)
    (cadddr exp))

;; evaluator data structures

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters
        (scan-out-defines body)
        env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied"
                 vars
                 vals)
          (error "Too few arguments supplied"
                 vars
                 vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((val (car vals)))
               (if (eq? '*unassigned* val)
                   (error "Unassigned variable")
                   val)))
             (else (scan (cdr vars)
                         (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame!
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; running the evaluator as a program


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '* *)
        (list '= =)
        (list '- -)
        (list '< <)
        (list 'display display)
        (list 'newline newline)
        ; ⟨more primitives⟩
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))


(define input-prompt  ";;; M-Eval input:")

(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (eval input
                 the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline)
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))


(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))



;; while

(define (while? exp)
  (tagged-list? exp 'while))

(define (while-cond exp)
  (cadr exp))

  (define (while-body exp)
    (cons 'begin (cddr exp)))

(define (eval-while exp env)
  (if (true? (eval (while-cond exp) env))
      (begin
        (eval (while-body exp) env)
        (eval-while exp env))
      'false))


;; scan-out-defines
(define (scan-out-defines body)
  (let* ((term-list (cons 'terms nil))
         (last-term term-list))
    (define (collect-replace exp)
      (cond ((and (definition? exp) (symbol? (cadr exp)))
             (let ((cell (cons (definition-variable exp) nil)))
               (set-cdr! last-term cell)
               (set! last-term cell))
             (set-car! exp 'set!)
             exp)
            (else exp)))
    (if (> 1 (length term-list))
        (let ((new-bod (map collect-replace body)))
          (make-let
           (map (lambda (t) (cons t '*unassigned*)) (cdr term-list))
           (list new-bod)))
        body)))

;; letrec

    (define (letrec? exp)
    (tagged-list? exp 'letrec))


  (define (letrec-var-terms exp)
		 (map car (cadr exp)))

  (define (letrec-vars exp)
    (cadr exp))

  (define (letrec-assignments exp)
    (cadr exp))

  (define (change-to-set! assignments)
    (if (not (null? assignments))
	(begin
	  (let ((assignment (car assignments))
		(insert (cons 'set! nil)))
	    (set-cdr! insert assignment)
	    (set-car! assignments insert))
	  (change-to-set! (cdr assignments)))))

  (define (merge-sets-body exp)
    (let ((end-of-assingments (letrec-assignments exp)))
      (define (ensure-end)
	(if (not (null? (cdr end-of-assingments)))
	    (begin
	      (set! end-of-assingments (cdr end-of-assingments))
	      (ensure-end))))
      (ensure-end)
      (set-cdr! end-of-assingments (cddr exp)))
    (set-cdr! exp (cons (cadr exp) nil)))



  (define (letrec->let exp)
      (let ((outervars (letrec-var-terms exp)))
	(change-to-set! (letrec-assignments exp))
	(merge-sets-body exp)
	(cons (make-lambda outervars (cadr exp))
	      (map (lambda (x) ''*unassigned*) outervars))))



;; driver

(define the-global-environment
  (setup-environment))

(driver-loop)



