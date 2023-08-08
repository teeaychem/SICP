#lang sicp


; (define x (cons 1 2 3))
#|
cons as a two place operator seems odd.
Esp. given + takes an arbirary number of places.
Though, I can see this in terms of a linked list.
Where, car and cdr are value and point (or vice-versa).
Still, as far as the textbook goes, starting with the abstraction of lists and then seeing the implementation is what I expected.
|#


(define zero (lambda (f) (lambda (x) x)))
(zero 0)
((zero 0) 0)


(eq? (car '(a b c)) (car (list 'a 'b 'c)))
'()

#|
Okay, so here we quote a list of things.

'(a b c) = (list 'a 'b 'c)
|#


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        (else (element-of-set? x (right-branch set)))
        )
  )

(define (adjoin-set x set)
  (cond
    ((null? set) (make-tree x nil nil))
    ((= x (entry set)) set)
    ((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
    ((> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))
    )
  )


#|
This is v. lazy with append.
But, it works.
|#
(define (display-set set)
  (if (null? (entry set))
      nil
      (let ((leftBranch (if (null? (left-branch set)) nil (display-set (left-branch set))))
            (rightBranch (if (null? (right-branch set)) nil (display-set (right-branch set)))))
        (append leftBranch (cons (entry set) rightBranch))
  )))


(display-set (adjoin-set 1 (adjoin-set 5 (adjoin-set 0 (adjoin-set 2 (adjoin-set 25 nil))))))
(display-set (adjoin-set 25 (adjoin-set 2 (adjoin-set 0 (adjoin-set 5 (adjoin-set 1 nil))))))
