#lang sicp

(inc 42)
(+ 1 (inc 1))
(* 9 3 2)

; Define is a 'special form'.
; Non-special form is recursive evaluation.
(define size 2)
size
(* 5 size)

; Procedure definitions
(define (square x) (* x x))
(square 8)
; (define (oops x) (+ x (oops x)))
; (oops 64)
(define (squareRedundant x y) (* x x))
(squareRedundant 2 3)

(define (repeat x y)
	(cond 
	((= y 0) x)
	((not (= y 0)) (+ (repeat x (- y 1)) x))
	)
)
(repeat (+ 2) 3)
; So, + 2 works. 0 is added as a default argument.
(define (repeat2 x y)
	(cond 
	((= y 0) x)
	(else (+ (repeat2 x (- y 1)) x))
	)
)
; 'else' is not written in parentheses.
(define (between x y z)
	(
		cond
		((and (< x z) (< z y)) #t)
		(else #f)
	)
)
(between 2 4 3)
(between 2 4 4)
(between 2 4 2.01)

(define (outside x y z)
(not (between x y z))
)
(outside 2 4 3)
(outside 2 4 5)

; Newton's method
(define (sqrtIter guess x)
	(new-if (goodEnough? guess x) ; Check is guess is good enough
		guess ; If good enough, return guess
		(sqrtIter (improve guess x) x) ; else, run again on an improved guess
	)
)

(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))

; Predicates are usually given ? to help clarify what they do.
(define (goodEnough? x y) (< (abs (- (square x) y)) 0.001))

(define (sqrt x) (sqrtIter 3.0 x))

(define (new-if predicate then-clause else-clause)
	(cond (predicate then-clause)
				(else else-clause))
)

10
; (new-if (= 0 1) 5 (new-if (= 0 1) 5 6))
; (sqrt 9)

#| 
	Making change.
	Pass in amount.
	Recurisve.
	For each coin type, take an instance of the coin and call on value without that coin.
	If passed in zero, return 1.
	Sum together all the recursive calls.

	Right, and if negative value, return 0.
	For > 0, recurse.
	This will always get 0 or negative.


	Base case is a simple coin.
	
 |#

(define (count-change amount) (cc amount 5))

	(define (first-denomination kinds-of-coins)
		(cond ((= kinds-of-coins 1) 1)
					((= kinds-of-coins 2) 5)
					((= kinds-of-coins 3) 10)
					((= kinds-of-coins 4) 25)
					((= kinds-of-coins 5) 50)
		))


(define (cc amount kinds-of-coins)

	(cond ((= amount 0) 1)
				((or (< amount 0) (= kinds-of-coins 0)) 0)
				(else (+ 
								(cc amount (- kinds-of-coins 1)) ; Effectively a for loop over kinds-of-coins
								(cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)
							)
				)
	)
)

; (count-change 100)