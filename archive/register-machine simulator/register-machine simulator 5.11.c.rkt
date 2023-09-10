#lang sicp

;; * the register-machine simulator from 5.2
;; * added gcd machine from book
;; * added fact machine from 5.2
;; * added sqrt machine from 5.3
;; * added expt machines from 5.4
;; * added fib machine from book
;; * added check for reused label
;; * prevented operations on labels
;; * implemented a table of stacks linked to registers (5.11.c)

;; from the past

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; make machine

(define (make-machine register-names
                      ops
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name)
                (((machine 'stack-table) 'add-stack) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; registers

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (set! contents value)))
            (else
             (error "Unknown request: REGISTER"
                    message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; the stack

(define (make-stack-table)
  (let ((table '()))
    (define (push id val)
      (let ((entry (assoc id table)))
        (if entry
            (set-cdr! entry (cons val (cdr entry)))
            (error "No stack with id:" id))))
    (define (pop id)
      (let ((entry (assoc id table)))
        (cond ((eq? entry #f)
               (error "No stack with id:" id))
              ((null? (cdr entry))
               (error "Empty stack: POP:" id))
              (else
               (let ((val (cadr entry)))
                 (set-cdr! entry (cddr entry))
                 val)))))
    (define (add-stack id)
      (set! table (cons (cons id '()) table)))
    (define (initialize)
      (set! table '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) pop)
            ((eq? message 'add-stack) add-stack)
            ((eq? message 'initialize)
             (initialize))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))

(define (pop stack-table id)
  ((stack-table 'pop) id))

(define (push stack-table id value)
  ((stack-table 'push) id value))

;; the basic machine

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack-table (make-stack-table))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list
            (list 'initialize-stack
                   (lambda ()
                     (stack-table 'initialize)))))
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
              (error "Unknown register:"
                     name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc
                  (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents!
                pc
                the-instruction-sequence)
               (execute))
              ((eq?
                message
                'install-instruction-sequence)
               (lambda (seq)
                 (set!
                  the-instruction-sequence
                  seq)))
              ((eq? message
                    'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message
                    'install-operations)
               (lambda (ops)
                 (set! the-ops
                       (append the-ops ops))))
              ((eq? message 'stack-table) stack-table)
              ((eq? message 'operations)
               the-ops)
              (else (error "Unknown request: MACHINE"
                           message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents!
   (get-register machine register-name)
   value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;; the assembler

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels)
                   (error "Multiply defined label:" next-inst)
                   (receive
                       insts
                       (cons
                        (make-label-entry
                         next-inst
                         insts)
                        labels)))
               (receive
                   (cons (make-instruction
                          next-inst)
                         insts)
                   labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack-table (machine 'stack-table))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels
         machine
         pc
         flag
         stack-table
         ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc!
         inst
         proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

;; generating execution procedures for instructions

(define (make-execution-procedure inst labels machine pc flag stack-table ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign
          inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test
          inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch
          inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack-table pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack-table pc))
        ((eq? (car inst) 'perform)
         (make-perform
          inst machine labels ops pc))
        (else (error "Unknown instruction type: ASSEMBLE"
                     inst))))

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
      (lambda ()   ; execution procedure
                   ; for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; test, branch, and goto instructions

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
        (error "Bad TEST instruction: ASSEMBLE"
               inst))))

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
        (error "Bad BRANCH instruction: ASSEMBLE"
               inst))))

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
                (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; other instructions

(define (make-save inst machine stack-table pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register
               machine
               reg-name)))
    (lambda ()
      (push stack-table reg-name (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack-table pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register
               machine
               reg-name)))
    (lambda ()
      (set-contents! reg (pop stack-table reg-name))
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
        (error "Bad PERFORM instruction: ASSEMBLE"
               inst))))

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
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE"
                     exp))))

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
                 (make-primitive-exp
                 e machine labels)
                 (error "Operation on something other than a register or constant: ASSEMBLE" e)))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))


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
        (error "Unknown operation: ASSEMBLE"
               symbol))))

;; end

;; built machines

;; gcd-machine

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

;; (display "running gcd-machine:")
;; (newline)
;; (set-register-contents! gcd-machine 'a 206)
;; (set-register-contents! gcd-machine 'b 40)
;; (start gcd-machine)
;; (get-register-contents gcd-machine 'a)
;; (display "ran gcd-machine")

;; factorial machine

(define fact-machine
  (make-machine
   '(counter n product)
   (list (list '> >) (list '* *) (list '+ +))
   '(
     (assign counter (const 1))
     (assign product (const 1))
     test-n
     (test (op >) (reg counter) (reg n))
     (branch (label fact-done))
     (assign product (op *) (reg counter) (reg product))
     (assign counter (op +) (reg counter) (const 1))
     (goto (label test-n))
     fact-done
     )))

;; (display "running fact-machine:")
;; (newline)
;; (set-register-contents! fact-machine 'n 6)
;; (start fact-machine)
;; (get-register-contents fact-machine 'product)
;; (display "ran fact-machine")


;; square-root machine

(define sqrt-machine
  (make-machine
   '(counter x guess div)
   (list (list '< <) (list '* *) (list '- -)
         (list '/ /) (list '+ +)
         (list 'abs abs))
   '(
     (assign guess (const 1.0))
     good-enough?
     (assign div (op *) (reg guess) (reg guess))
     (assign div (op -) (reg div) (reg x))
     (assign div (op abs) (reg div))
     (test (op <) (reg div) (const 0.001))
     (branch (label sqrt-done))
     (assign div (op /) (reg x) (reg guess))
     (assign guess (op +) (reg div) (reg guess))
     (assign guess (op /) (reg guess) (const 2.0))
     (goto (label good-enough?))
     sqrt-done
     )))

;; (display "running sqrt-machine:")
;; (newline)
;; (set-register-contents! sqrt-machine 'x 17)
;; (start sqrt-machine)
;; (get-register-contents sqrt-machine 'guess)
;; (display "ran sqrt-machine")

;; expt machine (recursive)

(define expt-machine-recursive
  (make-machine
   '(n continue val b)
   (list (list '= =) (list '* *) (list '- -))
   '(
     (assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done
     )))

;; (display "running expt-machine-recursive:")
;; (newline)
;; (set-register-contents! expt-machine-recursive 'b 3)
;; (set-register-contents! expt-machine-recursive 'n 5)
;; (start expt-machine-recursive)
;; (get-register-contents expt-machine-recursive 'val)
;; (display "ran expt-machine-recursive")

;; expt machine (iterative)

(define expt-machine-iterative
  (make-machine
   '(n val b)
   (list (list '= =) (list '* *) (list '- -))
   '(
     (assign val (const 1))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label expt-done))
     (assign n (op -) (reg n) (const 1))
     (assign val (op *) (reg b) (reg val))
     (goto (label expt-loop))
   expt-done
     )))

;; (display "running expt-machine-iterative:")
;; (newline)
;; (set-register-contents! expt-machine-iterative 'b 3)
;; (set-register-contents! expt-machine-iterative 'n 5)
;; (start expt-machine-iterative)
;; (get-register-contents expt-machine-iterative 'val)
;; (display "ran expt-machine-iterative")

;; fib machine

(define fib-machie
  (make-machine
   '(continue n val)
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
     (assign n
             (op -)
             (reg n)
             (const 1)) ; clobber n to n-1
     (goto
      (label fib-loop)) ; perform recursive call
     afterfib-n-1 ; upon return, val contains Fib(n − 1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n − 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)         ; save Fib(n − 1)
     (goto (label fib-loop))
     afterfib-n-2 ; upon return, val contains Fib(n − 2)
     (assign n
             (reg val)) ; n now contains Fib(n − 2)
     (restore val)      ; val now contains Fib(n − 1)
     (restore continue)
     (assign val        ; Fib(n − 1) + Fib(n − 2)
             (op +)
             (reg val)
             (reg n))
     (goto              ; return to caller,
      (reg continue))   ; answer is in val
     immediate-answer
     (assign val
             (reg n))   ; base case: Fib(n) = n
     (goto (reg continue))
     fib-done
     )
   ))

(display "running fib-machie:")
(newline)
(set-register-contents! fib-machie 'n 5)
(start fib-machie)
(get-register-contents fib-machie 'val)
(display "ran fib-machie")