#lang sicp

;; * the register-machine simulator from 5.2
;; * added gcd machine from book
;; * added fact machine from 5.2
;; * added sqrt machine from 5.3
;; * added expt machines from 5.4
;; * added fib machine from book
;; * added check for reused label and machine from Ex. 5.8
;; * prevented operations on labels, added test
;; * functionality to inspect machine via filtering provided instructions (Ex. 5.12)
;; * registers are automatically added (Ex. 5.13)
;; * added instruction to print stack stats (Ex. 5.14).
;; * inspector separated from machine and extended
;; * option to pgoress a fixed number of executions directly or interactively
;; * added tracing to fixed executions
;; * extended tracing to print contents of registers before and after execution.
;; * relative locations stored with instructions.
;; * registers are now created when needed.
;; * inspector is broken

;; from the past

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

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
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
              (let ((new-register (make-register name)))
                (set! register-table
                      (cons
                       (list name new-register)
                       register-table))
                new-register))))
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

(define (instruction-text inst)
  (cdadr inst))

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
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

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

;; built machines

;; gcd-machine

(define gcd-machine
  (make-machine
   ;; '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

 (display "running gcd-machine:")
 (newline)
 (set-register-contents! gcd-machine 'a 206)
 (set-register-contents! gcd-machine 'b 40)
 (start gcd-machine)
 (get-register-contents gcd-machine 'a)
 (display "ran gcd-machine")

;; factorial machine

;; (define fact-machine
;;   (make-machine
;;    ;; '(counter n product)
;;    (list (list '> >) (list '* *) (list '+ +))
;;    '(
;;      (assign counter (const 1))
;;      (assign product (const 1))
;;      test-n
;;      (test (op >) (reg counter) (reg n))
;;      (branch (label fact-done))
;;      (assign product (op *) (reg counter) (reg product))
;;      (assign counter (op +) (reg counter) (const 1))
;;      (goto (label test-n))
;;      fact-done
;;      )))

;; (display "running fact-machine:")
;; (newline)
;; (set-register-contents! fact-machine 'n 6)
;; (start fact-machine)
;; (get-register-contents fact-machine 'product)
;; (display "ran fact-machine")


;; square-root machine

;; (define sqrt-machine
;;   (make-machine
;;    ;; '(x guess div)
;;    (list (list '< <) (list '* *) (list '- -)
;;          (list '/ /) (list '+ +)
;;          (list 'abs abs))
;;    '(
;;      (assign guess (const 1.0))
;;      good-enough?
;;      (assign div (op *) (reg guess) (reg guess))
;;      (assign div (op -) (reg div) (reg x))
;;      (assign div (op abs) (reg div))
;;      (test (op <) (reg div) (const 0.001))
;;      (branch (label sqrt-done))
;;      (assign div (op /) (reg x) (reg guess))
;;      (assign guess (op +) (reg div) (reg guess))
;;      (assign guess (op /) (reg guess) (const 2.0))
;;      (goto (label good-enough?))
;;      sqrt-done
;;      )))

;; (display "running sqrt-machine:")
;; (newline)
;; (set-register-contents! sqrt-machine 'x 17)
;; (start sqrt-machine)
;; (get-register-contents sqrt-machine 'guess)
;; (display "ran sqrt-machine")

;; expt machine (recursive)

;; (define expt-machine-recursive
;;   (make-machine
;;    ;; '(n continue val b)
;;    (list (list '= =) (list '* *) (list '- -))
;;    '(
;;      (assign continue (label expt-done))
;;      expt-loop
;;      (test (op =) (reg n) (const 0))
;;      (branch (label base-case))
;;      (save continue)
;;      (assign n (op -) (reg n) (const 1))
;;      (assign continue (label after-expt))
;;      (goto (label expt-loop))
;;      after-expt
;;      (restore continue)
;;      (assign val (op *) (reg b) (reg val))
;;      (goto (reg continue))
;;      base-case
;;      (assign val (const 1))
;;      (goto (reg continue))
;;      expt-done
;;      )))

;; (display "running expt-machine-recursive:")
;; (newline)
;; (set-register-contents! expt-machine-recursive 'b 3)
;; (set-register-contents! expt-machine-recursive 'n 5)
;; (start expt-machine-recursive)
;; (get-register-contents expt-machine-recursive 'val)
;; (display "ran expt-machine-recursive")

;; expt machine (iterative)

;; (define expt-machine-iterative
;;   (make-machine
;;    ;; '(n val b)
;;    (list (list '= =) (list '* *) (list '- -))
;;    '(
;;      (assign val (const 1))
;;      expt-loop
;;      (test (op =) (reg n) (const 0))
;;      (branch (label expt-done))
;;      (assign n (op -) (reg n) (const 1))
;;      (assign val (op *) (reg b) (reg val))
;;      (goto (label expt-loop))
;;    expt-done
;;      )))

;; (display "running expt-machine-iterative:")
;; (newline)
;; (set-register-contents! expt-machine-iterative 'b 3)
;; (set-register-contents! expt-machine-iterative 'n 5)
;; (start expt-machine-iterative)
;; (get-register-contents expt-machine-iterative 'val)
;; (display "ran expt-machine-iterative")

;; fib machine

(define fib-machine
  (make-machine
   ;; '(continue n val)
   (list (list '< <) (list '+ +)
         (list '- -))
   '(
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n − 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)           ; save old value of n
     (assign n (op -) (reg n) (const 1)) ; clobber n to n-1
     (goto (label fib-loop)) ; perform recursive call
     afterfib-n-1 ; upon return, val contains Fib(n − 1)
     (restore n)
     ;; set up to compute Fib(n − 2)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label afterfib-n-2))
     (save val)         ; save Fib(n − 1)
     (goto (label fib-loop))
     afterfib-n-2 ; upon return, val contains Fib(n − 2)
     (assign n (reg val)) ; n now contains Fib(n − 2)
     (restore val)      ; val now contains Fib(n − 1)
     (restore continue)
     (assign val (op +) (reg val) (reg n)); Fib(n − 1) + Fib(n − 2)
     (goto              ; return to caller,
      (reg continue))   ; answer is in val
     immediate-answer
     (assign val (reg n))   ; base case: Fib(n) = n
     (goto (reg continue))
     fib-done
     )))

(define (run-fib-machine n)
  (display "running fib-machine:")
  (newline)
  (set-register-contents! fib-machine 'n n)
  (start fib-machine)
  (get-register-contents fib-machine 'val)
  (display "ran fib-machine")
)

;(define fib-machine-inspector (machine-inspector fib-machine))

(run-fib-machine 5)

;; (fib-machine-inspector 'instructions)
;; ((fib-machine-inspector 'filter-by-type) 'goto)
;; (fib-machine-inspector 'entry-regs)
;; (fib-machine-inspector 'save-rest)
;; ((fib-machine-inspector 'sources) 'val)
;; (fib-machine-inspector 'instruction-count)

(display "running fib-machine:")
(newline)
(set-register-contents! fib-machine 'n 7)
;((fib-machine 'execute-n-trace) 120)
(get-register-contents fib-machine 'val)
(display "ran fib-machine")
(newline)
((machine-inspector fib-machine) 'instruction-count)
((fib-machine 'manual-execute-trace) #t)

;; machine from ex 5.8 machine

;; (define 5.8-machine
;;   (make-machine
;;    '(a)
;;    (list )
;;    '(
;;      start
;;      (goto (label here))
;;      here
;;      (assign a (const 3))
;;      (goto (label there))
;;      here
;;      (assign a (const 4))
;;      (goto (label there))
;;      there
;;      )))

;; (display "running 5.8-machine:")
;; (newline)
;; (start 5.8-machine)
;; (get-register-contents 5.8-machine 'a)
;; (display "ran 5.8-machine")

;; machine to test ex 5.9

;; (define Ex.5.9-machine
;;   (make-machine
;;    '(test)
;;    (list (list 'not not))
;;    '(
;;      test-start
;;      (assign test (op not) (label test-start))
;; ;;   (assign test (op not) (op not))
;;      test-done
;;      )))

;; (define Ex.5.9-machine
;;   (make-machine
;;    '(test)
;;    (list (list 'not not))
;;    '(
;;      test-start
;;      (assign test (op not) (op not))
;;      test-done
;;      )))

(define recursive-fact-machine
  (make-machine
   (list (list '- -) (list '= =) (list '* *))
   '(
     (assign continue (label fact-done))   ; set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)                       ; Set up for the recursive call
     (save n)                              ; by saving n and continue.
     (assign n (op -) (reg n) (const 1))   ; Set up continue so that the
     (assign continue (label after-fact))  ; computation will continue
     (goto (label fact-loop))              ; at after-fact when the
     after-fact                              ; subroutine returns.
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val)) ; val now contains n(n - 1)!
     (goto (reg continue))                 ; return to caller
     base-case
     (assign val (const 1))                ; base case: 1! = 1
     (goto (reg continue))                 ; return to caller
     fact-done
     (display-stack-stats)
     )))

;; (define (run-recursive-fact-machine n)
;;   (display "running recursive-fact-machine-machine:")
;;   (newline)
;;   (set-register-contents! recursive-fact-machine 'n n)
;;   (start recursive-fact-machine)
;;   (get-register-contents recursive-fact-machine 'val)
;;   (display "ran recursive-fact-machine-machine")
;; )

;; (run-recursive-fact-machine 1)
;; (run-recursive-fact-machine 2)
;; (run-recursive-fact-machine 3)
;; (run-recursive-fact-machine 4)
;; (run-recursive-fact-machine 8)
