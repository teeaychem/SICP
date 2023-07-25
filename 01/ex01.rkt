#lang sicp

;; Ex 1.1
10 ; 10

(+ 5 3 4) ; 12

(- 9 1) ; 8

(/ 6 2) ; 3

(+ (* 2 4) (- 4 6)) ; 6

(define a 3)

(define b (+ a 1)) ; b = 4

(+ a b (* a b)) ; 19

(= a b) ; #f

(if (and (> b a) (< b (* a b))) b a) ; b, as 4 > 3, 4 < 12

(cond ((= a 4) 6)
			((= b 4) (+ 6 7 a)) ; b = 4, so 16
			(else 25)
)

(+ 2 (if (> b a) b a)) ; 6

(* (cond ((> a b) a)
				 ((< a b) b) ; #t
				 (else -1))
	 (+ a 1)
) ; 16, as 4 * (3 + 1)


;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; Ex 1.3
; Sum of squares function.
; First, compare a and b.
; If a >= b fix a, else b.
; If a, then compare b and c.
; If b, then compare a and c.

; Two basic functions
(define (square x) (* x x))
(define (sumSquare a b) (+ (square a) (square b)))

(define (twoBig a b c) 
(if
	(>= a b) 
		(if (>= b c) (sumSquare a b) (sumSquare a c)) ; Keeping a
		(if (>= a c) (sumSquare b a) (sumSquare b c)) ; Keeping b
))

; Test possible combinations
(twoBig 1 2 3)
(twoBig 2 3 1)
(twoBig 3 1 3)


;; Ex 1.4

(define (a-plus-abs-b a b) ; define procedure with two arguments
	((if (> b 0) + -) a b) ; Operator position is a procedure, so begin by evaluating this.
)												 ; Returns + procedure if b > 0, else the - procedure.

(a-plus-abs-b 4 4) ; b > 0, so 4 + 4
(a-plus-abs-b -4 4); b > 0, so -4 + 4
(a-plus-abs-b 4 -4); b < 0, so (4 - -4) = (4 + 4)
(a-plus-abs-b -4 -4); b < 0, so (-4 - -4) = (-4 + 4) = 0


;; Ex 1.5

(define (p) (p))

(define (test x y)
	(if (= x 0)
	0
	y))

; With applicative-order, start by evaluating test.
; test is an if statement, so the predicate is evaluated.
; The predicate contains x, so x is evaluated.
; 0 = 0, so test returns 0.
; y / (p) is not evaluated.

; With normal-order evluation, start by evaluating test, 0, and (p)
; Here, when working through test (p) is substituted in for y.
; And, as (p) is a process, it's evaluated.
; So, with normal-order, this procedure should fail to terminate.

; Key point here is procedure is evaluated first.
; In turn, whatever procedure is obtained from this determines which arguments to evaluate.

; So, here, first checking whether x or y are 0 would lead to a different result, e.g.
(define (test2 x y)
	(if (or (x = 0) (y = 0)) 0
	(if (= x 0)
	0
	y)))
; Nothing really changes on good values. If 0 is 0, 0 is returned, else, if 0 is not 0 y is returned,
; but y is already known to be 0.
; Still, in contrast to test we've not got to evaluate y on the first test.


;; 1.6

(define (new-if predicate then-clause else-clause)
	(cond (predicate then-clause)
				(else else-clause))
)

; Is the else-clause evaluated?
; For, AOE could be read as saying arguments are only evaluated when applied, and with new-if 
; it's not clear else-clause is ever *applied*, it's only returned.
; And, this should be the case, right.
; For, in some cases you can return a procedure, and this wouldn't be possible if you need to
; evaluate before returning.

; Consider:
; (define (a-plus-abs-b a b)
;	((if (> b 0) + -) a b))
; Here, + and - aren't evaluated after the predicate.

; But, is it the case that any procedure is automatically evaluated?
; I mean, you don't need to call eval explicitly at the top level.

; I think this is it.
; Any procedure is automatically evaluated.
; However, as sqrt-iter is recursive, a new instance of sqrt-iter
; is made before any evaluation takes place.
; Can see this with rTest

; (define (rTest x)
; 	(new-if (> 1 x) 0 (rTest (- x 1)))
; 	)
; (rTest 2)

; This should terminate fast, but it doesn't.

; By contrast, with a call to if, the two options are explicitly
; evaluated. So, in a recursive case, as long as the function calls
; a conditional which triggers a base case, there's the possibility of
; stopping the recursion before it's triggered again.

; If this is right, it seems a little subtle for the sixth question...


;; Ex 1.7

#| 
	good-enough? checks to see whether abs(guess^2 - target) < 0.001

	In the case of small numbers, this not very effective and for large number inadequate.
	As good-enough? compares two squared numbers, for small values there won't be much of a difference, while for large number there will be a significant difference.

	Observe, squares increaes fast with magnitude.

	(- (square 0.5) (square 0.25))
	(- (square 5) (square 2.5))
	(- (square 50) (square 25))

	So, with very small numbers, it's harder to get a difference less than 0.001, while for very large numbers it's fairly easy.
	This means, more precision is needed to get below the threshold in the
	small case, where the resulting difference isn't going to be noticable.
	And, less precision is needed in the large case.
 |#

(define (sqrt2 x) (sqrtItr2 1.0 0.0 x))

(define (sqrtItr2 guess previousGuess x)
	(if (goodEnough2? guess previousGuess) 
	guess
	(sqrtItr2 (improve guess x) guess x)
	)
)

; Mostly the same but for goodEnough2.
(define (goodEnough2? guess previousGuess)
	(< (abs (- guess previousGuess)) 0.001))
	#| 
		This, then, works as a limit on the number of significant digits we care about.
		Alternatively, can apply to (square guess) (square previousGuess), etc.
		Then, it's significant digits of the squared number, rather than root.
	 |#

(define (improve guess x) (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))

#| 
	Things are now reversed.
	For small numbers, things are less accurate, as the sqrt of a small number is going to be small, 
	and we'll make smaller and smaller changes by taking the average.
 |#


;; Ex 1.8

#| 
	Only thing that changes here is the improve function, where formula for better approximation is given.
	goodCubeGuess? could also be adjusted, as with previous exercise.
 |#

(define (cube x) (* x x x))

(define (cubeRt x) 
	(define (cubeInt guess) ; Using block structure and lexical scoping
		(define (goodCubeGuess? guess) (< (abs (- (cube guess) x)) 0.001)) ; Note x from cubeRt.
		(define (cubeImprove guess) (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)) ; And again
	
	(if (goodCubeGuess? guess)
		guess
		(cubeInt (cubeImprove guess))) ; 
	)
	(cubeInt 1.0)
)


(cube 3)
(cubeRt 27)

(cube 2)
(cubeRt 8)


;; Ex 1.9

#| 
	(define (+ a b)
		(if (= a 0)
		b
		(inc (+ (dec a) b)))
	)

	(+ 4 5)
	(inc (+ (dec a) b))
	(inc (+ 3 5))
	(inc (inc (+ 2 5)))
	(inc (inc (inc (+ 1 5))))
	(inc (inc (inc (inc + 0 5))))
	(inc (inc (inc (inc 5))))
	(inc (inc (inc 6)))
	(inc (inc 7))
	(inc 8)
	9

	So, recursive prodecure and recursive process.

	(define (+ a b)
		(if (= a 0)
		b
		(+ (dec a) (inc b)))
	)

	(+ 4 5)
	(+ (dec 4) (inc 5))
	(+ 3 6)
	(+ 2 7)
	(+ 1 8)
	(+ 0 9)
	9

	So, recursve procedure but iterative process
 |#


 ;; Ex 1.10


#| 
	(A 1 10)
	(A 0 (A 1 9))
	(A 0 (A 0 (A 1 8)))
	(A 0 (A 0 (A 0 (A 1 7))))
	(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
	(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
	(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
	(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
	(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
	(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
	(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
	(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
	(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
	(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
	(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
	(A 0 (A 0 (A 0 (A 0 64))))
	(A 0 (A 0 (A 0 128)))
	(A 0 (A 0 256))
	(A 0 512)
	1024
 |#


 #| 
	(A 2 4) 
	(A 1 (A 2 3))
	(A 1 (A 1 (A 2 2)))
	(A 1 (A 1 (A 1 (A 2 1))))
	(A 1 (A 1 (A 1 (A 1 (A 1 1)))))
	(A 1 (A 1 (A 1 (A 1 2))))
	(A 1 (A 1 (A 1 (A 0 (A 1 1)))))
	(A 1 (A 1 (A 1 (A 0 2))))
	(A 1 (A 1 (A 1 4)))
	(A 1 (A 1 (A 0 (A 1 3))))
	(A 1 (A 1 (A 0 (A 0 (A 1 2)))))
	(A 1 (A 1 (A 0 (A 0 (A 0 (A 1 1))))))
	(A 1 (A 1 (A 0 (A 0 (A 0 2)))))
	(A 1 (A 1 (A 0 (A 0 4))))
	(A 1 (A 1 (A 0 8)))
	(A 1 (A 1 16))
	(A 1 (A 0 (A 1 15)))
	|#

#| 
	Okay, this grows in a cool way.
 |#


(define (A x y)
	(cond
	((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))
	)
)


(A 1 10)
(A 2 4)
(A 3 3)
(A 2 3)

#| 
	(A 0 n) is 2n
	(A 1 n) is 2^ns
	(A 2 n) is 2^^(n - 1) i.e. if n = 3, 2^2^2, if n = 4 2^2^2^2
 |#


;; Ex 1.11

(define (fR n)
	(if 
	(< n 3)
	n
	(+ (fR (- n 1)) (* 2 (fR (- n 2))) (* 3 (fR (- n 3))))
	)
)

(define (fI n)
	(if 
	(< n 3)
	n
	(fIHelp n 2 1 0)
	)
)

(define (fIHelp current backOne backTwo backThree)
	(if (= 3 current)
	(+ backOne (* 2 backTwo) (* 3 backThree))
	(fIHelp (- current 1) (+ backOne (* 2 backTwo) (* 3 backThree)) backOne backTwo)
	))

(define (testfRI n) 
	(= (fR n) (fI n))
)

(testfRI 1)
(testfRI 3)
(testfRI 12)
(testfRI 24)


; Ex 1.12


#| 
	Top left is (1,1) then count down and right.
	So, first instance of 2 should be (2 3) and 6 should be (3 5)

	Basically, define anything negative as 0.
	With this anything positive is filled with, (x, y) = ((x - 1, y - 1) + (x, y - 1)).
	I.e. look up left and up above – imagine triangle aligned left.
	And, the triangle is generated by fixing (1, 1) as 1.
	
	Very ineffective.
 |#

(define (pascal x y)
	(cond
		((or (< x 1) (< y 1)) 0)
		((and (= x 1) (= y 1)) 1)
		(else (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1))))
	)
)

(pascal 1 1)
(pascal 2 2)
(pascal 3 2)
(pascal 2 3)
(pascal 3 5)
(pascal 3 1)


; Ex 1.13

#| 
	It's clear the goal is to show 
	(phi^n - psi^n)/sqrt(5) = (phi^(n-1) - psi^(n-1))/sqrt(5) + (phi^(n-2) - psi^(n-2))/sqrt(5)
	And, it's easy to go the case of 0 and 1 by hand for regular induction.
	But, the induction case... no good idea.

	Looking things up, I was going to be lost for a while...
	Still, I should remember golden ratio is only positive solution to x + 1 = x^2.
	And, phi as given is the golden ratio.
	So, phi^(n-2) + phi^(n-1) = phi^(n-2)(phi + 1) = phi^(n-2)phi^2 = phi^n.
	This hint might have been enough, as after noticing this with phi, it would be natural to to see
	if a similar thing holds for psi.
 |#