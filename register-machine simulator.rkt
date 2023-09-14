#lang sicp

;; * the register-machine simulator from 5.2
;; * functionality to inspect machine via filtering provided instructions (Ex. 5.12)
;; * added instruction to print stack stats (Ex. 5.14).
;; * inspector separated from machine and extended
;; * option to progress a fixed number of executions directly or interactively
;; * added tracing to fixed executions
;; * extended tracing to print contents of registers before and after execution.
;; * relative locations stored with instructions.
;; * inspector is broken
;; * ability to inject procedures before instruction specififed by relative offset to label
;; * more general way to print inputs and outputs of simulations
;; * added the explicit-control evaluator
;; - support for transforming conds to ifs in the explicit-control evaluator
;; + main expand-clauses logic written in the explicit-control evaluator
;; * evaluator updated to require same save/restore regs to help debug.

;; misc

(define (run-wiith-args-and-display-reg-vals machine reg-arg-pairs regs)
  (for-each (lambda (pair)
              (begin
                (for-each display (list "Register " (car pair) " was set to value " (cdr pair)))
                (newline)) pair)
            reg-arg-pairs)
  (for-each (lambda (pair)
              (set-register-contents! machine (car pair) (cdr pair)))
            reg-arg-pairs)
  (start machine)
  (for-each (lambda (reg)
              (begin
                (for-each display
                          (list "Register: " reg " end with value: " (get-register-contents machine reg)))
                (newline)))
            regs)
  (newline))

(define (contains-elem l e)
  (cond ((null? l) #f)
        ((equal? (car l) e) #t)
        (else (contains-elem (cdr l) e))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (for-each proc l)
  (cond ((null? (cdr l)) (proc (car l)))
        (else
         (proc (car l)) (for-each proc (cdr l)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (filter-unique test transform sequence)
  (let ((seen '()))
    (define (seen? elem sequence)
      (cond ((null? sequence) #f)
            ((equal? (car sequence) elem) #t) ;; need equal? rather than eq? as testing strings not pointers
            (else (seen? elem (cdr sequence)))))
    (define (do-it sequence)
      (cond ((null? sequence) nil)
            ((seen? (transform (car sequence)) seen)
             (do-it (cdr sequence)))
            ((test (car sequence))
             (begin
               (set! seen (cons (transform (car sequence)) seen))
               (cons (transform (car sequence))
                     (do-it (cdr sequence)))))
            (else (do-it (cdr sequence)))))
    (do-it sequence)))

;; make machine

(define (make-machine register-names
                      ops
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register)
                 register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; registers

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get)
             contents)
            ((eq? message 'get-label-dest)
             (if (tagged-list? contents 'primitive-label)
                 (caddr contents)
                 (error "Attempting to extract destination from something other than a primitive label")))
            ((eq? message 'get-simple)
             (if (tagged-list? contents 'primitive-label)
                 (cadr contents)
                 contents))
            ((eq? message 'set)
             (lambda (value)
               (set! contents value)))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; the stack

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth
                     'current-depth '= current-depth))
      (newline))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize)
             (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;; the basic machine

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda ()
                         (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda ()
                         (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc)
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register:" name)
            (set! register-table
                  (cons
                   (list name (make-register name))
                   register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val
               (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute-n-trace n trace)
        (let* ((insts (get-contents pc))
               (f-registers
                (if (null? insts)
                    '()
                    (filter-unique (lambda (x) x) (lambda (x) x) (found-registers (instruction-text (car insts))))
                    )))
          (if (or (= n 0) (null? insts))
              'done
              (begin
                (if trace
                    (begin
                      (for-each display (list "Todo : " (instruction-index (car insts)) " : "  (instruction-text (car insts))))
                      (cond ((not (null? f-registers))
                             (newline)
                             (display " - Before execution:")
                             (for-each (lambda (x)
                                         (newline)
                                         (display " - + ")
                                         (display x)
                                         (display " has value: ")
                                         (display ((lookup-register x) 'get-simple))) f-registers)
                             ))
                      (newline)))
                ((instruction-execution-proc (car insts)))
                (set! instruction-count (+ instruction-count 1))
                (if trace
                    (begin
                      (cond ((not (null? f-registers))
                             (display " - After execution:")
                             (for-each (lambda (x)
                                         (newline)
                                         (display " - + ")
                                         (display x)
                                         (display " has value: ")
                                         (display ((lookup-register x) 'get-simple))) f-registers)
                             ))
                      (newline)))
                (execute-n-trace (- n 1) trace)))))
      (define (manual-execute-trace trace)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                (else
                 (begin
                   (display "steps to execute: ")
                   (let ((input (read)))
                     (cond ((number? input)
                            (execute-n-trace input trace)
                            (manual-execute-trace trace))
                           (else
                            (display "undefined input")))))))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                (else
                 (begin
                   ((instruction-execution-proc (car insts)))
                   (set! instruction-count (+ instruction-count 1))
                   (execute))))))
      (define (inject-before-index label offset proc)
        (define (inject-before-index-helper instruction-pointer label offset)
          (if (null? instruction-pointer)
              #f
              (let ((inst (car instruction-pointer)))
                (cond ((and (contains-elem (instruction-index-labels inst) label)
                            (equal? (instruction-index-offset inst) offset))
                       (let ((ori-inst (instruction-execution-proc inst)))
                         (set-cdr! (instruction-execution-proc-head inst)
                                   (lambda ()
                                     (begin
                                       (proc)
                                       (ori-inst))))))
                      (else (inject-before-index-helper (cdr instruction-pointer) label offset))))))
        (inject-before-index-helper the-instruction-sequence label offset))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'execute-n-trace)
               (set-contents! pc the-instruction-sequence)
               (lambda (n trace?) (execute-n-trace n trace?)))
              ((eq? message 'manual-execute-trace)
               (set-contents! pc the-instruction-sequence)
               (lambda (trace?) (manual-execute-trace trace?)))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack)
               stack)
              ((eq? message 'operations)
               the-ops)
              ((eq? message 'the-instruction-sequence)
               the-instruction-sequence)
              ((eq? message 'instruction-count)
               instruction-count)
              ((eq? message 'inject-before-index)
               inject-before-index)
              ((eq? message 'reset-instruction-count)
               (set! instruction-count 0))
              (else (error "Unknown request: MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents
   (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents!
   (get-register machine register-name)
   value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;; inspector

(define (machine-inspector machine)
  (let* ((the-instruction-sequence (machine 'the-instruction-sequence))
         (input-inst-seq (map (lambda (x) (car x)) the-instruction-sequence)))
    (define (get-unique inst-type)
      (filter-unique (lambda (x) (tagged-list? x inst-type)) (lambda (x) x) input-inst-seq))
    (define (display-insts pre sequence)
      (for-each (lambda (x) (display pre) (display x) (newline)) sequence))
    (define (dispatch message)
      (cond
        ((eq? message 'instructions)
         (display-insts "" input-inst-seq))
        ((eq? message 'filter-by-type)
         (lambda (type)
           (for-each display (list "Instructions of type '" type "':")) (newline)
           (display-insts
            " - "
            (filter-unique (lambda (x) (tagged-list? x type)) cadr input-inst-seq))))
        ((eq? message 'entry-regs)
         (display "Registers used to hold entry points:") (newline)
         (display-insts
          " - "
          ;; second, filter registers and return register name
          (filter-unique (lambda (x) (tagged-list? x 'reg)) cadr
                         ;; first, filter gotos and return argument
                         (filter-unique (lambda (x) (tagged-list? x 'goto)) cadr input-inst-seq))))
        ((eq? message 'save-rest)
         (display "Registers saved and restrored from:") (newline)
         (display-insts
          " - "
          ;; filter for save/rest and return reg name
          (filter-unique
           (lambda (x) (or (tagged-list? x 'save) (tagged-list? x 'restore)))
           cadr
           input-inst-seq)))
        ((eq? message 'sources)
         (lambda (reg)
           (for-each display (list "Sources of register '" reg "':")) (newline)
           (display-insts
            " - "
            (filter-unique
             (lambda (x) (and (tagged-list? x 'assign)
                              (equal? (cadr x) reg)))
             cddr
             input-inst-seq))))
        ((eq? message 'instruction-count)
         (for-each display (list (machine 'instruction-count) " instructions executed"))
         (newline))
        ))
    dispatch))

;; the assembler
(define (append-elem! l e)
  (cond ((null? l)
         (set! l (list e)))
        ((null? (cdr l))
         (set-cdr! l (cons e nil)))
        (else (append-elem! (cdr l) e))))

(define (end-of l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        (else (end-of (cdr l)))))

(define (append-elem-if-new e l)
  (cond ((null? l) (list e))
        ((equal? (car l) e) l)
        (else (cons (car l) (append-elem-if-new e (cdr l))))))

(define (append-if-new l1 l2)
  (cond ((null? l1) l2)
        (else (append-if-new (cdr l1) (append-elem-if-new (car l1) l2)))))

(define (found-registers exp)
  (cond ((null? exp) '())
        ((tagged-list? exp 'assign)
         (cons (assign-reg-name exp) (found-registers (assign-value-exp exp))))
        ((or (tagged-list? exp 'restore) (tagged-list? exp 'save))
         (cdr exp))
        ((tagged-list? (car exp) 'reg) ; car exp as reg is only as some arg.
         (cons (cadar exp) (found-registers (cdr exp))))
        (else (found-registers (cdr exp)))))

(define (assemble controller-text machine)
  (let ((text-pointer controller-text)
        (next-inst nil)
        (label-buffer (list "No label"))
        (count-buffer 0)
        (prev-label #f)
        (instructions (list nil)) ; instruction is offset by nil
        (instructions-pointer '())
        (labels '()))
    (define (extract-info-loop)
      (cond ((null? text-pointer)
             (set! instructions (cdr instructions))
             (for-each (lambda (x) (set-cdr! x (cddr x))) labels) ; fix instruction offset
             (update-insts! instructions labels machine))
            (else
             (set! next-inst (car text-pointer))
             (cond ((symbol? next-inst)
                    (cond ((assoc next-inst labels) ; just searching the labels already have
                           (error "Multiply defined label:" next-inst))
                          (else
                           (if (not prev-label)
                               (set! label-buffer '()))
                           (set! prev-label #t)
                           (set! count-buffer 0)
                           (set! label-buffer (cons next-inst label-buffer))
                           (set! labels (cons (make-label-entry next-inst (end-of instructions))
                                              labels)))))
                   (else
                    (set! prev-label #f)
                    (append-elem! instructions (make-instruction (cons label-buffer count-buffer) next-inst))
                    (set! instructions-pointer (cdr instructions))
                    (set! count-buffer (+ count-buffer 1))))
             (set! text-pointer (cdr text-pointer))
             (extract-info-loop))))
    (extract-info-loop)
    instructions))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (begin
         (set-instruction-execution-proc!
          inst
          (make-execution-procedure
           (instruction-text inst)
           labels
           machine
           pc
           flag
           stack
           ops))))
     insts)))

(define (make-instruction location text)
  (list (cons 'location location) (cons 'text text) (cons 'proc '())))

(define (instruction-index inst)
  (cdar inst))

(define (instruction-index-labels inst)
  (cadar inst))

(define (instruction-index-offset inst)
  (cddar inst))

(define (instruction-text inst)
  (cdadr inst))

(define (instruction-execution-proc-head inst)
  (caddr inst))


(define (instruction-execution-proc inst)
  (cdaddr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! (caddr inst) proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE" label-name))))

;; generating execution procedures for instructions

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        ((eq? (car inst) 'display-stack-stats)
         (make-stack-stats inst machine labels ops pc))
        (else (error "Unknown instruction type: ASSEMBLE" inst))))

;; assign instructions

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register
          machine
          (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()   ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; test, branch, and goto instructions

(define (make-stack-stats inst machine labels flag pc)
  (lambda ()
    ((machine 'stack) 'print-statistics)
    (advance-pc pc)))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition
                machine
                labels
                operations)))
          (lambda ()
            (set-contents!
             flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label
                   labels
                   (label-exp-label dest))))
             (lambda ()
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register
                   machine
                   (register-exp-reg dest))))
             (lambda ()
               (set-contents!
                pc
                (reg 'get-label-dest)
                ))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; other instructions

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
        (reg (get-register
              machine
              reg-name)))
    (lambda ()
      (push stack (cons reg-name (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
        (reg (get-register
              machine
              reg-name)))
    (lambda ()
      (let* ((stack-pop (pop stack))
             (stack-name (car stack-pop))
             (stack-val (cdr stack-pop)))
        (cond ((eq? reg-name stack-name)
               (set-contents! reg stack-val)
               (advance-pc pc))
              (else
               (error "Bad RESTORE request" inst)))))))

;; (define (make-save inst machine stack pc)
;;   (let ((reg (get-register
;;               machine
;;               (stack-inst-reg-name inst))))
;;     (lambda ()
;;       (push stack (get-contents reg))
;;       (advance-pc pc))))

;; (define (make-restore inst machine stack pc)
;;   (let ((reg (get-register
;;               machine
;;               (stack-inst-reg-name inst))))
;;     (lambda ()
;;       (set-contents! reg (pop stack))
;;       (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action
                machine
                labels
                operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))

(define (perform-action inst)
  (cdr inst))

;; execution procedures for subexpressions

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label
                 labels
                 (label-exp-label exp))))
           (lambda ()
             (list 'primitive-label exp insts))))
        ((register-exp? exp)
         (let ((r (get-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

(define (register-exp? exp)
  (tagged-list? exp 'reg))

(define (register-exp-reg exp)
  (cadr exp))

(define (constant-exp? exp)
  (tagged-list? exp 'const))

(define (constant-exp-value exp)
  (cadr exp))

(define (label-exp? exp)
  (tagged-list? exp 'label))

(define (label-exp-label exp)
  (cadr exp))

(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                (if (or (register-exp? e) (constant-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "Operation on something other than reg/const: ASSEMBLE" e)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE" symbol))))

;; end

;; the explicit-control evaluator

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args) (apply (primitive-implementation proc) args))

(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-if predicate
                 consequent
                 alternative)
  (list 'if
        predicate
        consequent
        alternative))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (cond? exp) (tagged-list? exp 'cond))
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
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp
                      (cond-actions first))
                     (expand-clauses rest))))))

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
        (list '> >)
        (list 'display display)
        (list 'newline newline)
        (list 'not not)
        (list 'memq memq)
        (list 'eq? eq?)
        (list 'remainder remainder)
        ; ⟨more primitives⟩
        ))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))
(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp)) (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
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

(define the-global-environment
  (setup-environment))
(define (get-global-environment) the-global-environment)

(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'primitive-implementation primitive-implementation)
        (list 'compound-procedure? compound-procedure?)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-body procedure-body)
        (list 'procedure-environment procedure-environment)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'quoted? quoted?)
        (list 'text-of-quotation text-of-quotation)
        (list 'assignment? assignment?)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'if-predicate if-predicate)
        (list 'if-consequent if-consequent)
        (list 'if-alternative if-alternative)
        (list 'lambda? lambda?)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'begin? begin?)
        (list 'begin-actions begin-actions)
        (list 'last-exp? last-exp?)
        (list 'first-exp first-exp)
        (list 'rest-exps rest-exps)
        (list 'application? application?)
        (list 'operator operator)
        (list 'operands operands)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'rest-operands rest-operands)
        (list 'make-procedure make-procedure)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'empty-arglist empty-arglist)
        (list 'adjoin-arg adjoin-arg)
        (list 'last-operand? last-operand?)
        (list 'extend-environment extend-environment)
        (list 'true? true?)
        (list 'false? false?)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'no-more-exps? no-more-exps?)
        (list 'cond? cond?)
        (list 'cond->if cond->if)
        (list 'car car)
        (list 'cdr cdr)
        (list 'null? null?)
        (list 'cond-else-clause? cond-else-clause?)
        (list 'cond-predicate cond-predicate)
        (list 'make-if make-if)
        (list 'sequence->exp sequence->exp)
        (list 'cond-clauses cond-clauses)
        ))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(
     ;; running the evaluator
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input)
              (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     print-result
     (perform (op announce-output)
              (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))
     unknown-procedure-type

     ;; clean up stack (from apply-dispatch):
     (restore continue)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))

     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     ;; the core of the explicit-control evaluator
     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op cond?) (reg exp))
     (branch (label cond->if))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ;; evaluating simple expressions
     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))

     ev-variable
     (assign val
             (op lookup-variable-value)
             (reg exp)
             (reg env))
     (goto (reg continue))

     ev-quoted
     (assign val
             (op text-of-quotation)
             (reg exp))
     (goto (reg continue))

     ev-lambda
     (assign unev
             (op lambda-parameters)
             (reg exp))
     (assign exp
             (op lambda-body)
             (reg exp))
     (assign val
             (op make-procedure)
             (reg unev)
             (reg exp)
             (reg env))
     (goto (reg continue))

     ;; evaluating procedure applications
     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev)             ; the operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))    ; the operator
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)
     ev-appl-operand-loop
     (save argl)
     (assign exp
             (op first-operand)
             (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue
             (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))
     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl
             (op adjoin-arg)
             (reg val)
             (reg argl))
     (assign unev
             (op rest-operands)
             (reg unev))
     (goto (label ev-appl-operand-loop))
     ev-appl-last-arg
     (assign continue
             (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))

     ev-appl-accum-last-arg
     (restore argl)
     (assign argl
             (op adjoin-arg)
             (reg val)
             (reg argl))
     (restore proc)
     (goto (label apply-dispatch))

     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))

     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev
             (op procedure-parameters)
             (reg proc))
     (assign env
             (op procedure-environment)
             (reg proc))
     (assign env
             (op extend-environment)
             (reg unev)
             (reg argl)
             (reg env))
     (assign unev
             (op procedure-body)
             (reg proc))
     (goto (label ev-sequence))

     ;; sequence evaluation and tail recursion
     ev-begin
     (assign unev
             (op begin-actions)
             (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue
             (label ev-sequence-continue))
     (goto (label eval-dispatch))

     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev
             (op rest-exps)
             (reg unev))
     (goto (label ev-sequence))
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ;; non-tail-recursion
     ;; ev-sequence
     ;; (test (op no-more-exps?) (reg unev))
     ;; (branch (label ev-sequence-end))
     ;; (assign exp (op first-exp) (reg unev))
     ;; (save unev)
     ;; (save env)
     ;; (assign continue
     ;;         (label ev-sequence-continue))
     ;; (goto (label eval-dispatch))
     ;; ev-sequence-continue
     ;; (restore env)
     ;; (restore unev)
     ;; (assign unev (op rest-exps) (reg unev))
     ;; (goto (label ev-sequence))
     ;; ev-sequence-end
     ;; (restore continue)
     ;; (goto (reg continue))
     ;; conditionals, assignments, and definitions
     ev-if
     (save exp)   ; save expression for later
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))

     ;; evaluate the predicate:
     (goto (label eval-dispatch))
     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ;; assignments and definitions
     ev-assignment
     (assign unev
             (op assignment-variable)
             (reg exp))
     (save unev)   ; save variable for later
     (assign exp
             (op assignment-value)
             (reg exp))
     (save env)
     (save continue)
     (assign continue
             (label ev-assignment-1))

     ; evaluate the assignment value:
     (goto (label eval-dispatch))
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!)
              (reg unev)
              (reg val)
              (reg env))
     (assign val
             (const ok))
     (goto (reg continue))
     ev-definition
     (assign unev
             (op definition-variable)
             (reg exp))
     (save unev)   ; save variable for later
     (assign exp
             (op definition-value)
             (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))

     ; evaluate the definition value:
     (goto (label eval-dispatch))
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!)
              (reg unev)
              (reg val)
              (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ;; cond-if
     cond->if
     ;; will use exp, unev, argl.
     ;; goal is to overwrite exp. save only others
     (save unev)
     (save argl)
     (save continue)
     (assign unev (op cond-clauses) (reg exp)) ;; work until unev is empty
     (assign continue (label expand-clauses-over)) ;; once done, restore here.
     (goto (label expand-clauses-loop)) ;; start the loop

     expand-clauses-loop
     (test (op null?) (reg unev)) ;; any clauses?
     (branch (label expand-clauses-no-clauses)) ;; branch
     (assign argl (op car) (reg unev)) ;; first clause
     (assign unev (op cdr) (reg unev)) ;; rest of clauses
     (test (op cond-else-clause?) (reg argl)) ;; do we have an else clause?
     (branch (label expand-clauses-else)) ;; if so, go to else
     (goto (label expand-clauses-intermediate)) ;; otherwise intermediate case

     expand-clauses-intermediate
     (save continue)
     (save argl) ;; save for later
     (assign continue (label join-clauses)) ;; update continue to join
     (goto (label expand-clauses-loop))

     join-clauses ;; make an if and then continue
     (restore argl)
     (save val)
     (assign val (op cdr) (reg argl))
     (assign val (op sequence->exp) (reg val))
     (assign argl (op car) (reg argl))
     (assign exp (op make-if) (reg argl) (reg val) (reg exp))
     (restore val)
     (restore continue)
     (goto (reg continue))

     ;; else clause
     expand-clauses-else
     (test (op null?) (reg unev))
     (branch (label expand-clauses-else-ok)) ;; else is last
     (goto (label expand-clauses-else-not-last)) ;; else is not last
     ;; ok to process else
     expand-clauses-else-ok
     (assign argl (op cdr) (reg argl)) ;; get the contents
     (assign exp (op sequence->exp) (reg argl)) ;; return the updated expression
     (goto (reg continue)) ;; whatever continue is
     ;; else is not last so signal error
     expand-clauses-else-not-last
     (assign val (const "ELSE clause isn't last: COND->IF"))
     (goto (label signal-error))
     ;; if no else clause, then exp is #f and done
     expand-clauses-no-clauses
     (assign exp (const #f))
     (goto (reg continue))

     ;; when things are done
     expand-clauses-over
     (restore continue)
     (restore argl)
     (restore unev)
     (goto (label eval-dispatch))
     )))

;; ((eceval 'manual-execute-trace) #t)
(start eceval)


;; So, goal is to make an new expression.
;; Hence, expression is what we're writing everything to.
;; Use unev to store expression not evaluated so far.
;; Use argl to store the first part of the argument.

