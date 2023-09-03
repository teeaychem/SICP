#lang sicp

;; Changes to the eval:
;; * Added support for let expressions as derived
;; * Added support for let* expressions
;; * Added support for named let expressions
;; * Added support for while constructs
;; * Added support for scan-out-defines (I think).
;; * Added support for letrec.
;; * Changed to normal order evaluation, with memoisation
;; * Added non-strict/semi- primitive procedures (cons, car, cdr).
;; * Added lazy lists.

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; lazy cons

(define (cons-l x y)  (lambda (m) (m x y)))

(define (car-l z) ((force-it z) (lambda (p q) p)))

(define (cdr-l z) ((force-it z) (lambda (p q) q)))

(define (list-l elems)
  (if (null? elems)
      nil
      (cons-l (car elems) (list-l (cdr elems)))))
;; eval

(define (eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp)
         (eval-if exp env))
        ((while? exp)
         (eval-while exp env))
        ((lambda? exp)
         (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence
          (begin-actions exp)
          env))
        ((cond? exp)
         (eval (cond->if exp) env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((letrec? exp)
         (eval (letrec->let exp) env))
        ((application? exp)
         (meta-apply (actual-value (operator exp) env)
                     (operands exp)
                     env))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))

;; actual-value

(define (actual-value exp env)
  (force-it (eval exp env))) ; force any thunks

;; apply

(define (meta-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values
           arguments
           env)))  ; no choice but to apply arguments with primitive, so need arg vals.
        ((semi-primitive-procedure? procedure)
         (apply-semi-primitive-procedure
          procedure
          (list-of-delayed-args
           arguments
           env))
         )
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args
            arguments
            env)   ; extend env with delayed args.
                   ;; have ref to a thunk, but nothing more.
           (procedure-environment procedure))))
        (else (error "Unknown procedure
                      type: APPLY"
                     procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value
             (first-operand exps)
             env)
            (list-of-arg-values
             (rest-operands exps)
             env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it
             (first-operand exps)
             env)
            (list-of-delayed-args
             (rest-operands exps)
             env))))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env)) ;; exp is already 'delayed' (as un-evaled), but also keep env.

(define (thunk? obj) (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj) ; if thunk, goal is to memo
         (let ((result ; result stores result of evaluating the thunk
                (actual-value
                 (thunk-exp obj)
                 (thunk-env obj))))
           (set-car! obj 'evaluated-thunk) ; change the current tag.
           (set-car! (cdr obj) result) ; update the result
           (set-cdr! (cdr obj) '()) ; env is no longer needed, as no more calls to eval.
                                    ; could leave, but I guess forgetting will help clean-up
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;; conditionals

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) ; always want to eval the predicate.
                           env))              ;; everything else is handled recursively
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
    ;(delay-it (definition-value exp) env) ; defer eval of definitions
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
  (list-l (cadr exp)))

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

(define (procedure-body p)
  ;;(scan-out-defines
  (caddr p)
  ;;)
  )

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
               (cond ((eq? '*unassigned* val)
                      (error "Unassigned variable"))
                     ;; ((tagged-list? val 'thunk)
                     ;;  (force-it val))
                     (else val)
                   )))
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
  (list (list 'null? null?)
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


(define input-prompt  ";;; L-Eval input:")

(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value ; input may be just a thunk, so need to force.
                   input
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
          (append (primitive-procedure-names) (semi-primitive-procedure-names))
          (append (primitive-procedure-objects) (semi-primitive-procedure-objects))
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
	   (last-term term-list)
	  (go? #f))
      (define (collect-replace exp)
	(cond ((and (definition? exp))
	       (let ((cell (cons (definition-variable exp) nil)))
		 (set-cdr! last-term cell)
		(set! last-term cell)
		(set! go? #t))
	       (set-car! exp 'set!)
	       exp)
	      (else exp)))
      (let ((new-bod
	     ;; (if (not (pair? (car body)))
	     ;; (collect-replace body)
	     (map collect-replace body)
	     ;; )
	     ))
	(if go?
	    (let->combination (make-let
			       (map (lambda (t) (list t ''*undefined*)) (cdr term-list))
			       (list new-bod)))
	    body))))

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
          (map (lambda (x) '*unassigned*) outervars))))

;; semi primitive procedures

(define (semi-primitive-procedure? proc)
  (tagged-list? proc 'semi-primitive))

(define (semi-primitive-implementation proc)
  (cadr proc))

(define semi-primitive-procedures
  (list (list 'car car-l)
        (list 'cdr cdr-l)
        (list 'cons cons-l)
        (list 'list list-l)
        ; ⟨more primitives⟩
        ))

(define (semi-primitive-procedure-names)
  (map car semi-primitive-procedures))

(define (semi-primitive-procedure-objects)
  (map (lambda (proc)
         (list 'semi-primitive (cadr proc)))
       semi-primitive-procedures))

(define (apply-semi-primitive-procedure proc args)
  (apply
   (semi-primitive-implementation proc) args))


;; driver

(define the-global-environment
  (setup-environment))

(driver-loop)



