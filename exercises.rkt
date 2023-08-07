#lang sicp

;; Ex. 1.1
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
   (+ a 1)) ; 16, as 4 * (3 + 1)


;; Ex. 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; Ex. 1.3

#|
Sum of squares function.
First, compare a and b.
If a >= b fix a, else b.
If a, then compare b and c.
If b, then compare a and c.
|#


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


;; Ex. 1.4

(define (a-plus-abs-b a b) ; define procedure with two arguments
  ((if (> b 0) + -) a b) ; Operator position is a procedure, so begin by evaluating this.
  )												 ; Returns + procedure if b > 0, else the - procedure.

(a-plus-abs-b 4 4) ; b > 0, so 4 + 4
(a-plus-abs-b -4 4); b > 0, so -4 + 4
(a-plus-abs-b 4 -4); b < 0, so (4 - -4) = (4 + 4)
(a-plus-abs-b -4 -4); b < 0, so (-4 - -4) = (-4 + 4) = 0


;; Ex. 1.5

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


;; Ex. 1.7

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


;; Ex. 1.8

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
        (cubeInt (cubeImprove guess)))
    )
  (cubeInt 1.0)
  )


(cube 3)
(cubeRt 27)
(cube 2)
(cubeRt 8)


;; Ex. 1.9

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


;; Ex. 1.10


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


;; Ex. 1.11

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


;; Ex. 1.12


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

;; Ex. 1.13

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
If only I'd looked back to see that the equation was highlighted...
|#


;; Ex. 1.14


#|
Steps: Theta(2^n)
Space: Theta(n)

count-change works in a very similar way to tree-recursive fibonacci.
For each call of the alogorithm, at most two calls are made.
One call reduces the amount by at least one.
The other changes the coin of interest.
So, we're asked about a function of the amount, but we can view this as a function of
amount + coins, as coins is constant.
Each call reduces amounts + coins by at least one.
So, after one call we have at most n - 1 calls remaining.
Each of these takes a constant number of steps.
We're doing some tests and then performing summation on the results of the two calls.

For space, I'm following the reasoning on p. 38--9.
We only need to keep track of where to return to.
The algorithm is set to brach n times.
So, the depth of the tree is n.
After each call, the number of leaves doubles.
That is, leaves is given by 2(n-1).
Hence, in total the leaves count to 4(n-1).

To be honest, I think this question is asking the reader to recall p. 39.
The number of steps required by a tree-recursive process will be proportional to the number
of nodes in the tree, while the space required will be proportional to the maximum depth.

For space, important thing to keep in mind is this is max space at any given point in time.
We only need to store a reference to the originial call when starting a new call.
So, in general this will use as much space as calls.
But, at any given time only a certain number of nodes will be in play.
In particular, the height of the tree.
For, there's no need (nor way) to explore multiple nodes at the same time.
|#


;; Ex. 1.15

#|
a.

How many times is p applied?
sine is recursive, and evaluated on every call to sine, but need the result of the recursive call
to be applied.
So, we need to check how many times the recursive call is made before the base case is hit.
12.5/3^5 < 0.1.
So, after 5 additional calls the if condition is true.
p is not applied on the sixth call, but is applied on the 5 other calls.
|#


#|
b.

Theta(n) for both steps and space.

Steps, as we've got a test and then possible single recursive call.
There's no way to bound this call, and the other steps take constant(ish) time.
So, n.

Space, as the recursive call returns, need to keep track of the original call.
This is some constant(ish) space for each maximum call depth, which is roughly n.

Right, this is a linear recursive process.
|#


;; Ex. 1.16

#| Helper function to test for even |#
(define (even? n)
  (= (remainder n 2) 0))

#| Main fuction, following the hint |#
(define (try a b n) ; a is current value, n is number of exponents remaining
  (cond
    ((= n 0) a)
    ((even? n)  (try a (* b b) (/ n 2))) ; So long as n is even, reduce remaing by half by squaring current.
    ; This is b^n = (b^2)^(n/2)
    ; As input, do a^n
    ; As output, a = a^2, still need a^(n/2)
    (else (try (* a b) b (- n 1)))
    )
  )

(define (expItr b n)
  (try 1 b n)) ; Hide a as a helper variable.

(expItr 2 4)
(expItr 6 5) ; Expect 7776
(expItr 7 1) ; Expect 7
(expItr 15 3) ; Expect 3375
(expItr 15 0) ; Expect 1


;; Ex. 1.17

#|
Using (* 2 x) for double x and (/ x 2) for half x.

dhMultH, only works for positive integers.
So, dhMult ensures positive numbers are passed through, and adjusts the result appropriately.
|#
(define (multPositiveTranslate func a b)
  ((if (> 0 b) + -) 0 ((if (> 0 a) + -) 0 (func (abs a) (abs b))))
  )

(define (dhMult a b)
  (multPositiveTranslate dhMultH a b)
  )


(define (dhMultH a b)
  (cond
    ((> 0 b) (- 0 (dhMultH a (- 0 b))))
    ((= b 0) 0) ; 0 base case, return 0.
    ((= b 1) a) ; positive base case, return a.
    ((even? b) (* 2 (dhMultH a (/ b 2)))) ; Double whatever I get from halving multiplication to do.
    (else (+ a (dhMultH a (- b 1)))) ; Add b to whatever I get from reducing multiplication by one.
    )
  )

;(dhMult 1 4)
;(dhMult 5 5)
(dhMult 5 -8)
(dhMult 5 8)
(dhMult 5 -7)
(dhMult 5 7)
(dhMult 5 0)
(dhMult -6 6)
(dhMult -5 -5)

;; Ex. 1.18

#|
Calculate m * n.
Do this iteratively by doubling m and havling n.
When n is odd, copy the value of m to a store variable, added at the end.
Always go to n = 1, so add m to store variable.
|#

(define (dhMultIterHelp a m n)
  (cond
    ((= n 0) a)
    ((even? n) (dhMultIterHelp a (* m 2) (/ n 2))) ;
    (else (dhMultIterHelp (+ a m) m (- n 1)))
    )
  )

(define (dhMultIterPos m n)
  (dhMultIterHelp 0 m n)
  )

(define (dhMultIter m n)
  (multPositiveTranslate dhMultIterPos m n)
  )

; Some tests
(dhMultIter 2 6)
(dhMultIter 3 5)
(dhMultIter 3 0)
(dhMultIter 0 5)
(dhMultIter 3 14)


;; Ex. 1.19


#|
Two applications of T_pq reduce to a single application of
a <- b(2pq + q^2) + (2pq + q^2) + a(p^2 + q^2)
b <- b(p^2) + a(2pq + q^2)

So, applied twice we have something of the same form, where:
new_q = (2pq + q^2)
new_p = (p^2)

With this in mind, finishing the function is simple, to double the values of
p and q, apply this transformation to itself.
|#


(define (fibI n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* p q) (* q q) (* q p))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; Testing
(fibI 0)
(fibI 1)
(fibI 2)
(fibI 3)
(fibI 4)


;; Ex. 1.20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

#|
(206 40)
(40 6)
(6 4)
(4 2)
(2 0)

The main operator of gcd is a conditional, and as described the condition of a conditional is
evaluated before either result is evaluated.
So, the remainder operations actually performed equal the number of times gcd is called, minus one.
For, on the last call (= b 0), which goes to a.

Or I'm missing something, but it seems the conditional must work in this way.
Normal order reduces everything to a primitive form.
But, with a recursive function like gcd a 'synthetic' primative form is needed.
For, otherwise, and additional call to gcd is always possible.

With all this in mind, given the flow is determined by a conditional, normal and evaluative order should be the same here.
|#


;; Ex. 1.21


#|
A little before.
If d is a divisor of n, then so is n/d.
Proof for this seems easy by contradicition.
To show d divides n, just need an int such that d * a = n.
Consider n/d.
As d is a divisor of n, so n/d is an int, and clearly d * n/d = n.
|#

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

10101
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; ? ? ? What is the purpose of this?


;; Ex. 1.22

(define (prime? n)
  (= n (smallest-divisor n))
  ;; (= n (smallest-divisor-next n))
  )


(define (timed-prime-test n)
  ;; (newline)
  ;; (display n)
  (start-prime-test n (runtime)))


(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))


(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (prime-test-range-k-step n m k)
  (cond ((> n m)
      (display " finished "))
        (else
         (timed-prime-test n)
         (prime-test-range-k-step (+ n k) m k)
         )
      )
  )

(define (search-for-primes n m)
  (prime-test-range-k-step (if (even? n) (+ n 1) n) (if (even? m) (- m 1) m) 2)
  )

; (search-for-primes 1 100000)

#|
Well, it takes a little longer, but things happen too fast to really test sqrt(n).
And, any significant deviation from average is probs. a scheduling thing.
|#


;; Ex. 1.23


(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (find-divisor-next n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-next n (next test-divisor)))))

(define (smallest-divisor-next n)
  (find-divisor-next n 2))


; (smallest-divisor-next 2)

;; (search-for-primes 1 100000)

#|
The speed-up going to 100000 is significant.
Around 2~3 times as fast at the end tail.
|#


;; Ex. 1.24


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))
        ))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n ) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
(cond ((= times 0) true)
      ((fermat-test n) (fast-prime? n (- times 1)))
      (else false)))

#|
Again, timing isn't particularly easy here.
I'd expect 1,000,000 to not take too much more time than 1000, as we have Theta(long n) growth.
|#


;; Ex. 1.25


#|
With expmod we have a way of figuring out the exponential of a number modulo another number (with Theta(log n) growth).
So, roughly remainder(base^exp / m).
The way this works is to figure out base^exp and then task the remainder at each call.
Difference is where remainder is called.
As given, we take remainder before squaring resut, in the even case.
So, this should make a differnece, at least in principle.
As, both remainder and square take time, and depending on the way things go, could lead to m long n growth.
But, presumably as the argument is kept small, this growth is 'hidden' behind the growth of the exponent.

As footnote 46 points out, the position of remainder ensures the numbers dealt with are not much larger than m.

By contrast, for the proposed expmod, we for sure need to consider the growth of remainder.
If it's n, then we have n + long n = n.

Though, I can't find much about the cost of remainder in the text.
|#



;; Ex. 1.26


#|
With explicit multiplication, there are now two calls to expmod on each recursive call.
So, we've effectively doubled the amount of work. (log n)^2 = n.

With square, by contrast, the result of expmod is evaluated and then passed to the fuction.
The evaluated value is used twice, but the evaluation only happens once.
|#


;; Ex. 1.27


(define (carmichael-fool n)
  (hidden-fool (- n 1) n)
  )



(define (hidden-fool b n)
  (cond ((= b 0) (display " fooled "))
        ((= (remainder (expItr b n) n) (remainder b n)) (hidden-fool (- b 1) n))
        (else (display " found "))
        )
  )


;; (carmichael-fool 561)
;; (carmichael-fool 1105)
;; (carmichael-fool 1729)
;; (carmichael-fool 2465)
;; (carmichael-fool 2821)
;; (carmichael-fool 6601)
;; (carmichael-fool 6600)
;; (carmichael-fool 6602)


;; Ex. 1.28


#|
Modifying expmod to signal whether it discovers a non-trivial square root of 1.

An important detail was a little buried.
For, the test is only guaranteed for odd numbers.
So, when working through half of the numbers, we need to already be sure the number is odd.

Added some formatting, though ofc I should have a single call for n/prime display.
|#

(define (MR-expmod base exp m) ; base = a, exp = n
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (MR-check-sqaure (MR-expmod base (/ exp 2) m) m)) m))
        (else (remainder (* base (MR-expmod base (- exp 1) m)) m))
        ))


(define (MR-check-sqaure a m)
  (cond
    ((or (= a 1) (= a (- m 1))) a) ; In this case, a is trivial, so continue
    ((= (remainder (square a) m) 1) 0) ; We know a != 1 nor (m - 1), so check a % n = 1. Not prime if satisfied.
    (else a) ; Otherwise, continue
    )
  )


(define (MR-seq-test n a)
  (cond
    ((> a (/ n 2)) (display "Prime!\n"))
    ((= (MR-expmod a (- n 1) n) 0) (display "Not prime…\n"))
    (else (MR-seq-test n (+ a 1)))
    ))


(define (MR-prime? n)
  (display "Checking: ")
  (display n)
  (display "\n")
  (cond
    ((= n 2) (display "Prime!\n"))
    ((even? n) (display "Not prime…\n"))
    (else (MR-seq-test n 1))
  ))

(MR-prime? 2)
(MR-prime? 3)
(MR-prime? 4)
(MR-prime? 7)
(MR-prime? 9)
(MR-prime? 10)
(MR-prime? 11)
(MR-prime? 12)


;; Ex. 1.29


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (simpson f a b n)
  (define (sM k x)
  (cond
    ((or (= k 0) (= k n)) x)
    ((even? k) (* 2 x))
    (else (* 4 x))
    ))
  (define (h) (/ (- b a) n))
  (define (fakh k) (sM k (f (+ a (* k (h))))))
  (* (/ (h) 3) (sum fakh 0 inc n)))


(simpson cube 0 1 1000.0)
(simpson cube 0 1 10000.0)

#|
Well, the results are closer to 1/4…
|#


;; Ex. 1.30


(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result)))
    )
  (iter a 0)
  )

; (sum-iter cube 0 inc 10)


;; Ex. 1.31


;; a.

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))
      ))

#|
This is just substituting * for +…
And factorial works with identiy and inc.
|#

(define (factorial n) (product identity 1 inc n))


(define (piHelpB d)
  (/ (* (- d 1) (+ d 1)) (square d)))

(define (incTwo n) (+ n 2))

(define (piClose n)
  (* 4.0 (product piHelpB 3 incTwo (+ n 3))))

;(piClose 500)

#|
Use the numerator as the index, and work through pairs of denominators.

Either I've done something wrong, on this takes a lot to get close to pi.
|#

;; b.


(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result)))
    )
  (iter a 1)
  )


;; Ex. 1.32


#|
Made both versions, then read part b…
|#

(define (accumulate-first combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate-first combiner null-value term (next a) next b))
      ))


(define (accumulate-first-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result)))
    )
  (iter a null-value)
  )

#|
E.g.…
|#

(define (sum-accumulate-first term a next b)
  (accumulate-first + 0 term a next b)
  )

; To test, adapt previous use of sum/product.
; Replace accumalate with accumalate-iter to vary recu/iter.

;; (sum-iter cube 0 inc 10)
;; (sum-accumulate-first cube 0 inc 10)



;; Ex. 1.33


(define (filtered-accumulate-first filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value)
                (filtered-accumulate-first filter combiner null-value term (next a) next b))
      ))

#|
If the filter is satisfied, then combine (term a), otherwise combine via the null-value.
|#


; a.

(define (sum-square-prime a b)
  (filtered-accumulate-first prime? + 0 square a inc b))

; (sum-square-prime 1 10)

; b.


(define (sum-relatively-prime n)
  (define (predicate? a)
    (= (gcd a n) 1))
  (filtered-accumulate-first predicate? * 0 identity 0 inc b))

; (sum-relatively-prime 11)

#|
No test values for this, but at least here I have a nice example of blocking the predicate.
|#


;; Ex. 1.34

(define (f g) (g 2))

;; (f square)
;; (f (lambda (z) (* z (+ z 1))))

#|
We apply 2 to 2.
With f labelled
(f1 f2) - > (f2 2) -> (2 2)
But, 2 isn't a procedure.
|#


;; Ex. 1.35

#|
From p. 38 we have the golden ratio is (1 + sqrt(5))/2.
Let φ = (1 + sqrt(5))/2.
And, we know φ^2 = φ + 1.

To figure out how to express φ as a fixed point of x, we'll work through x = φ until we have a non-trivial transformation of x on the rhs.
So:

x = φ
x^2 = φ^2 \ Algebra
x^2 = φ + 1 \ Given
x^2 = x + 1 \ Initial equality.
x = (x + 1)/x \ Algebra
x = (x/x + 1/x) \ Algebra
x = (1 + 1/x) \ Algebra
|#


(define (fixed-point f first-guess tolerance)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display "φ is roughly: ")
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0 0.0000001)


;; Ex. 1.36


(define (fixed-point-display f first-guess tolerance)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess guess-number)
    (let ((next (f guess)))
      (display "Guess: ")
      (display guess-number)
      (display " is: ")
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next (+ guess-number 1)))))
  (try first-guess 1))

(fixed-point-display (lambda (x) (/ (log 1000) (log x))) 20 0.0001)



;; Ex. 1.37


; a.

(define (cont-frac n d k)
  (define (cont-frac-i n d k step)
    (if (= step k)
        (/ (n step) (d step))
        (/ (n step) (+ (d step) (cont-frac-i n d k (+ step 1))))
        )
  )
  (cont-frac-i n d k 1)
  )

(cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             11)

#|
Looks as though k = 11 works for 4 decimal places.
1/φ = 0.6180339887498948482045868343656381177203091798057628621354486227…
|#


; b.

#|
cont-frac is recursive, so for an iterative version…
Start the other way.
Do N_k/D_k and then work backwards.
|#


(define (cont-frac-iter n d k)
  (define (cont-frac-i n d k so-far)
    (if (= k 0)
        so-far
        (cont-frac-i n d (- k 1) (/ (n k) (+ (d k) so-far)))
        )
  )
  (cont-frac-i n d k 0)
  )

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)



;; Ex. 1.38

#|
There's nothing here other than figuring out a way to generate the desired sequence for Dk.
So, here's something excessive.
|#

(define (dk i)
  (let ((x (- i (/ i 3)))
        )
    (if (= (- x (floor x)) (/ 1 3)) (ceiling x) 1)
    )
  )

#|
e = 2.7182818284590452353602874713526624977572470936999595749669676277…
|#

(+ 2 (cont-frac (lambda (x) 1.0) dk 200))


;; Ex. 1.39


#|
Modify cont-frac.
n is constant x^2.
d is number variable, given as argument.
And, offset things a little.

At start, calculate x^2 to save some resources.
Then, work through offset cont-frac k times.
|#

(define (tan-cf x k)

  (define (cont-frac-i x2 d k step)
    (if (= step k)
        (/ x2 d)
        (/ x2 (- d (cont-frac-i x2 (+ d 2) k (+ step 1))))
        )
    )
  (let (
        (x2 (square x))
        )
    (/ x (- 1 (cont-frac-i x2 3 k 1)))
    )
  )


(tan-cf 1.0 10.0)


;; Ex. 1.40

(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x)
           (/ (- (g (+ x dx)) (g x))
              dx)))
  )


(define (newton-transform g)
  (lambda (x)
         (- x (/ (g x) ((deriv g) x))))
  )


(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess 0.0000001)
  )


(define (cubic-all x a b c)
  (+ (cube x) (* a (square x)) (* b x) c)
  )


(define (cubic a b c)
  (lambda (x) (cubic-all x a b c)))
  ;; (lambda (x)
  ;;   (+ (cube x) (* a (square x)) (* b x) c)
  ;;   ))


#|
Here, just constructing a function.
There's two options.
First (as taken), define the general function, and then obtain a particular instance.
Second, directly define a particular instance (as commented).
|#


;; Ex. 1.41


(define (double f)
  (lambda (x) (f (f x)))
  )

((double inc) 2)
(((double (double double)) inc) 5)


;; Ex. 1.42

(define (compose f g)
  (lambda (x) (f (g x)))
  )

((compose square inc) 6)


;; Ex. 1.43


(define (repeated f times)
    (if (= times 1)
        f
        (compose f (repeated f (- times 1)))
        )
  )

((repeated square 2) 5)


#|
This is a little surprising.
For, the most natural base case is f x.
With this, completing recursive calls gets f^n x.
And, as a last step take λ x f^n x, or just leave the procedure as it is.

But, this isn't okay, as x dones't evaluate to anything.

So, instead, the base case is f.
The argument is not made explicit, so there's no issue of evaluting it.
We just return the procedure.

So, mabye it's not *that* strange.
With f x we are working with some arbitrary value, and as this is arbitrary, what we're doing is transformed into a procedure.
And, with f we are working with a procedure directly.

Hm.
|#


;; Ex. 1.44


(define (smoothed f dx)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)
    )
  )


(define (smooth-fold f dx n)
  ((repeated (lambda (x) (smoothed x dx)) n) f)
  )



#|
Nothing too exciting here.
We repeat the smoothing, and then apply this to the function.
(Repeating smoothing applied to the function would involve repeating the function.)
lambda lets us do this easily, though I guess it would also be easy if dx was fixed as a constant somewhere.
|#


;; Ex. 1.45

#|
Average-damp(f(x)) = (x + f(x))/2

e.g. Average-damp(10^2) = 55 = (10 + 100)/2

In general, fixed point fails without dampening as y -> x/y -> x/(x/y) -> y
Then, y -> (x + x/y)/2.

In any case, this exercise seems rough.
We're asked to experiment to find out how many average damps are required to compute nth roots as a fixed-point search.
The problem is failure to converge, though.
Of course, things are a little better.
For, the general form of the problem is re-obtaining the initial value, or at least a prior value.
So, it's in principle possible to store every value computed and then check to see if any of these are repeated.
Still, it's not very interesting.

The final function isn't too interesting either.
Once the required number of times to average damp is figured out, the next task is to repeatedly average damp the function, and then apply the fixed point solver to this.
|#


;; Ex. 1.46


#|
In outline, a conditional:

if (good? guess) guess (improve guess)

So, here, the only issue is obtaining the guess.
But, this is two lambda terms.
lambda f lambda x (f x)
|#

(define (iterative-improve good? improve)
  (lambda (guess) (if (good? guess)
      guess
      ((iterative-improve good? improve) (improve guess))))
  )


#|
To help make things clear, a couple of let statements to define the relevant procedures.
Then, call iterative-improve.
Here, 1 could be anything – it's just a first guess.
|#


(define (iiSqrt x)
  (let (
        (good? (lambda (guess) (< (abs (- (square guess) x)) 0.001)))
        (improve (lambda (guess) (average guess (/ x guess))))
        )
    ((iterative-improve good? improve) 1)
    )
  )

; Some tests
;; (iiSqrt 4.0)
;; (iiSqrt 16.0)
;; (iiSqrt 125.0)


(define (iiFixed-Point f)
  (let (
        (good? (lambda (guess) (< (abs (- (f guess) guess)) 0.00001)))
        (improve (lambda (guess) (f guess)))
        )
    ((iterative-improve good? improve) 1)
    )
  )


; A couple of funcs from 1.3.3 (p. 69) for testing
;; (iiFixed-Point cos)
;; (iiFixed-Point (lambda (y) (+ (sin y) (cos y))))



;; Ex 2.1

#|

|#

(define (numer x) (car x))
(define (demon x) (cdr x))


#|
Not particularly elegant.
Though, cheeky λ to avoid calculating (* n d) twice.
Note, doing (define (mult) (* n d)) wouldn't be any help, as this would just call the multiplcation.
|#

(define (make-rat-basic n d)
  (let ((sign  ((lambda (x) (/ x (abs x))) (* n d))))
    (cons (* sign (abs n)) (abs d))
    )
  )

#|
It's not possible to always reference a let constant from another let constant.
This kind of makes sense to me.
At some point, need to make the reference happen.
This way, reference isn't assumed to be sequential, nor does one need to track dependencies.
|#

#|
(define (make-rat-basic-x n d)
  (let ((mult (* n d))
        (sign (/ mult (abs mult)))
        )
    (cons (* sign (abs n)) (abs d))
    )
  )
|#


#|
make-rat as defined in the book already does this.
But, it's due to a bug in gcd.
For, gcd(2, -3) = 1.
Yet, (gcd 2 -3) = -1.
And, in general, given (gcd a b), if a is positive and b is negative, then the result is negative.
Otherwise, the result has the sign of a.
|#

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))
    )
  )

#|
In the book, newline is first, but newline last fits with previous displays.
|#

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (demon x))
  (newline)
  )

;; (print-rat (make-rat-basic -2 -3))
;; (print-rat (make-rat-basic -2 3))
;; (print-rat (make-rat-basic 2 3))
;; (print-rat (make-rat-basic -2 3))
;; (print-rat (make-rat -2 -3))
;; (print-rat (make-rat 2 -3))
;; (print-rat (make-rat 2 3))
;; (print-rat (make-rat -2 3))


;; Ex. 2.2


(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;; (x-point (make-point 1 2))
;; (y-point (make-point 1 2))


(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-segment
   (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
   (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)
   )
  )

;; (midpoint-segment (make-segment (make-point 2 2) (make-point 4 4)))
;; (midpoint-segment (make-segment (make-point -4 -8) (make-point 4 4)))

#|
Though, I think this should be a little more general.
With a coordinate space, there's no limit on the dimension.
So, rather than having distinct x and y selectors, there should be a general selection which takes the dimension as an argument.
Then, it's easy to expand everything, given some way to test wether the dimension is stored.
Though, at this point in the book the problem is lists.
We only have pairs.
|#

;; Ex. 2.3

#|
For a rectangle, there's a few ways to do this.
Though, point for origin and rationals for legnth and width seems most straightforward.

A rectange is stored as ((origin-x origin-y) (width height))
|#


(define (make-rectangle origin width height)
  (cons origin (cons width height)))

(define (rectangle-origin rectangle)
  (car rectangle))

(define (rectangle-width rectangle)
  (car (cdr rectangle)))

(define (rectangle-height rectangle)
  (cdr (cdr rectangle)))

#|
Not sure what is meant by perimeter here.
Length of the perimeter is… not very interesting.
Points to construct the perimeter is a little better.
So, that's what the following procedure does.
Points are enumerated clockwise starting with 1 as top-left going to 3 as bottom-left.
|#

(define (rectangle-perimeter rectangle point)
  (if (= point 0)
      (rectangle-origin rectangle)
      (let ((xPoint (x-point (rectangle-origin rectangle)))
            (yPoint (y-point (rectangle-origin rectangle))))
        (cond
          ((= point 1) (make-point (+ xPoint (rectangle-width rectangle)) yPoint))
          ((= point 2) (make-point (+ xPoint (rectangle-width rectangle)) (+ yPoint (rectangle-height rectangle))))
          ((= point 3) (make-point xPoint (+ yPoint (rectangle-height rectangle))))
          )
        )
      )
  )

(define (rectangle-area rectangle)
  (* (rectangle-width rectangle) (rectangle-height rectangle)))


(define test-rect (make-rectangle (make-point 5 10) 15 10))
(rectangle-origin test-rect)
(rectangle-width test-rect)
(rectangle-height test-rect)
(rectangle-perimeter test-rect 2)

#|
The representation of the rectangle doesn't matter, so long as there's selectors for origin, width, and height.

And, I can't think of an interesting alterantive representation.
Could take the center point.
The only thing here is origin is given by center.x - width/2, etc.

Could also take two points.
Say, top left and bottom right.
Then, origin is top left, and width is obtained from top right.x - bottom left.x in the positive case.
|#


;; Ex. 2.4

#|
(car (cons x y)) = λm (m x y) (λ (p q) p)
                 = (λ (p q) p) x y
                 = x

for cdr

(define (cdr z)
(z (lambda (p q) q)))

So:

(cdr (cons x y)) = λm (m x y) (λ (p q) p)
                 = (λ (p q) q) x y
                 = y
|#



;; Ex. 2.5

#|
To pair the numbers use any exponentiation procedure which takes base and exponent arguments.

To break apart the number, keep dividing by either 2 or 3 until no further integer division is possible.

I'd guess there's some trick with log here, but I don't see it quickly.
|#

(define (break-down pair n)
  (define (break-down-i pair n m)
    (if (not (= 0 (remainder pair n)))
        m
        (break-down-i (/ pair n) n (+ m 1))
        )
    )
  (break-down-i pair n 0)
  )

(define (pair-numbers a b)
  (* (expItr 2 a) (expItr 3 b)))

(define (pair-number-a pair)
  (break-down pair 2))

(define (pair-number-b pair)
  (break-down pair 3))

; Basic test
(pair-number-a (pair-numbers 32 94))
(pair-number-b (pair-numbers 32 94))

; Some edge cases.
(pair-number-a (pair-numbers 1 1))
(pair-number-b (pair-numbers 1 0))

#|
Note, only asked to do with for non-negative integers.
|#


;; Ex. 26

#|
So…

0 = λf λx x
1 = λf λx f x
2 = λf λx ff x
|#

#|
(define one (lambda (f) (lambda (x) (f (x)))))
(define two (lambda (f) (lambda (x) (f (f (x))))))
|#

#|
Okay, not allowed to apply repeat to add-1.

Still, this is just a variation on add-1.

(define (add n m)
(lambda (f) (lambda (x) (f (n f) ((m f) x)))))

Instead of x, we have ((m f) x).
m is of the form λf λx f^m x.
So, ((m f) x) is of the form f^m x.
n is of the form λf λx f^n x
So, (n f) is of the form λx f^m x.
Hence, (f (n f) ((m f) x)) reduces to f^n (f^m x).
And, this is what we want. f applied n + m times.
|#



;; 2.1.4 Extended exercise


#|
Why not do intervals by a precise quantity tolerance pair?
At least in the case of resistors, where things are specified this way?

Though, I can see in general specifying lower and upper bounds is easier.
Unless, upper + lower / 2.
This gets 'precise quantity'.
Then, upper - mid.
This gets half width.
So, either way seems fine.
|#


;; 2.7

(define (make-interval a b) (cons a b))
#|
So, this is as specified, but why rely on the user to fix the correct upper and lower bounds?
|#
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

;; 2.8

#|
So, add-interval reduces to addition on the upper and lower bounds.
In this way, sub-interval will do the same.
Though, add a check to ensure upper is upper and lower is lower.
|#

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))
                 ))

(define (sub-interval x y)
  (let (
        (lIS (- (lower-bound x) (lower-bound y)))
        (hIS (- (upper-bound x) (upper-bound y)))
        )
    (if (> hIS lIS)
        (make-interval hIS lIS)
        (make-interval lIS hIS)
        )
    ))

(add-interval (make-interval 0 1) (make-interval 9 10))
(sub-interval (make-interval 0 2) (make-interval 9 10))

#|
In the same way adding increases margin, subtancting decreases margin.
I feel this isn't quite right.
Instead, go with proposal above and take an procedure for combining two tolerances.
|#


;; 2.9


(define (interval-width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

#|
In the case of addition.
x.w + y.w = (x.u - x.l)/2 + (y.u - y.l)/2
          = ((x.u - x.l) + (y.u - y.l))/2
          = ((x.u + y.u) + (x.l - y.l))/2
          = (x + y).w

The reasoning is the same for subtraction.
|#


#|
For multiplication, consider some intervals using 0 and 1.
(1 0) * (1 0) = (0 0)
And, we have (1 0).w = 0.5, while (0 0).w = 0.
In contrast:
(1 1) * (1 1) = (1 1)
And, we have (1 1).w = 0.5

Division is… defined in terms of multiplication as the main operator.
This isn't a proof, but suggests similar problems.
|#


;; Ex. 2.10


#|
'spans 0' means 'has width 0'?
I mean, and interval crossing over 0 should be no problem.
Anyway, mul-interval written so variant of div-interval doesn't prevent anything else running.
|#

(define (mul-interval x y)
  (let (
        (p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y)))
        )
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4)))
  )

(define (div-interval x y)
  (if (= 0 (interval-width y))
      (error)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))
                                   )
                    )
      )
  )


;; Ex. 2.11

#|
I'm not quite seeing the point of this exercise.
In any case, we can write out a table of all the possible positive/negative combinations.
Then, we can work out whether there's a guaranteed value for the upper and lower bounds.
With perhaps some errors, I have this:

 u_x | u_y | l_x | l_y | ub | lb
  +  |  +  |  +  |  +  | P4 | P1
  +  |  +  |  +  |  -  | P4 | P3
  +  |  +  |  -  |  +  | P4 | P2
  +  |  +  |  -  |  -  |
  +  |  -  |  +  |  +  | P3 | P4
  +  |  -  |  +  |  -  | P2 | P3
  +  |  -  |  -  |  +  |
  +  |  -  |  -  |  -  | P1 | P3
  -  |  +  |  +  |  +  | P2 | P3
  -  |  +  |  +  |  -  |
  -  |  +  |  -  |  +  | P1 | P4
  -  |  +  |  -  |  -  | P1 | P2
  -  |  -  |  +  |  +  |
  -  |  -  |  +  |  -  | P3 | P1
  -  |  -  |  -  |  +  | P4 | P1
  -  |  -  |  -  |  -  | P1 | P4

Where:

P1 l_x l_y
P2 l_x u_y
P3 u_x l_y
P4 u_x u_y

Here, then, there's the 'else' case, which covers the four instances where we need to calculate more than two combinations.
In all the other cases, we need to calculate the listed two cases.

But, I guess I've got this a little wrong.
For, we can break this down into sets of calculations.
I.e. { P1, P4 } covers both the last and the first case.
Then, for the upper and lower bound we only need to figure out which is larger than the other.
This is still only two instances of multiplications.

But, on my count there are six distinct sets.
So, this means we only need to consider seven cases.

I guess I'm missing something here…
|#



#|
Ah, I guess I'm this potential user, huh.
|#


;; Ex. 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w))
  )

(define (make-center-percent c p)
  (let ((nudge (* (abs c) p)))
    (make-interval (- c nudge) (+ c nudge))
    )
  )

(define (i-center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)
  )

#|
This isn't super precise with small percentages.
But, there's no way around this without changing the bound constructor.
As, imprecision has already entered by multiplying the center by the percentage to get the bounds.
|#
(define (i-percent i)
  (- (/ (upper-bound i) (i-center i)) 1)
  )


(i-center (make-center-percent 10 0.001))
(i-percent (make-center-percent 10 1.01))


;; Ex 2.13


#|
Assuming small percentage tolerances and all numbers are positive.

Formula for the approximate percentage tolerance of the product of two intervals
in terms of the tolerances of the factors.
|#


#|
So, for the product we take min/max of different upper/lower x/y combinations.
In general, then, product is of the form:

(x ± t_x) * (y ± t_y) = (xy ± yt_x ± xt_y ± t_xt_y)
|#


#|
Hm, well, to find the tolerance pertentage, divide upper by center then subtract 1.
Ignoring the part where we substract one, we have:
u / ((u + l) / 2) = 2u/(u * l)

If assuming everything is positive, then upper and lower bounds are direct.

So, we have

2(ux + tx)(uy + ty) / ((ux + tx)(uy + ty) + (lx + tx)(ly + ty))

Where tx is really uxtx, etc.
That is, ux + tx = ux(1 + tx).


Different way of looking at things.
If we start by keeping tolerance in play, then the upper bound for x, with tolerance is u_x(1 * tx).
So, then, the upper bound for product, with tolerance is
u_x * u_y * (1 * tx) * (1 * ty)
So, then, (1 * tx) * (1 * ty) = 1 + tx + ty + txty.
In this case, then, why not take the tolerance as tx + ty + txty?
This seems sufficiently simple, but gives the exactl tolerance…
Of course, tx + ty should get quite close when tx and ty are very small, as txty is going to be very very small.
|#


(define (quick-mul-tolerance i1 i2)
  (+ (i-percent i1) (i-percent i2))
  )

#|
This looks good to me…
|#

(quick-mul-tolerance (make-center-percent 10 0.001) (make-center-percent 10 0.006))
(i-percent (mul-interval (make-center-percent 10 0.001) (make-center-percent 10 0.006)))


;; Ex. 2.14


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define ti1 (make-center-width 2 0.01))
(define ti2 (make-center-width 2 0.001))
(define ti3 (make-center-width 4 0.001))

#|
Two intervals are equal just in case they have the same upper and lower bounds.
|#
(define (i-equal i1 i2)
  (and (= (upper-bound i1) (upper-bound i2)) (= (lower-bound i1) (lower-bound i2)))
  )

(par1 ti1 ti2)
(par2 ti1 ti2)
(i-equal (par1 ti1 ti2) (par2 ti1 ti2))

(i-percent (par1 ti1 ti2))
(i-percent (par2 ti1 ti2))


;; Ex. 2.15


#|
So, as intervals are just upper and lower bounds, the tolerance percentage is implicit.

When adding, everything should be preserved.
For, we sum the lower and upper bounds.
So, the center point of the new point is just the sum of the center points of the initial points.

Similar for subtraction.

In this sense, Eva Lu Ator isn't quite right.
The operations performed are important, not just the way the interval is written.
|#


#|
Things are different in the case of product and division, though.
Here we multiply bounds, and hence multiply tolerance.
As seen earlier, we get (1 * tx) * (1 * ty) = 1 + tx + ty + txty.

par2 has one less instance of multiplication than par1.
And, as x * y > x + y, at least when everything is positive, at least some of the additional tolerance from product is going to be preserved.

Going to need to define "better", tho.
It's not clear tolerance as given really reflects anything under these transformations.
|#


;; Ex. 2.16


#|
Well, par1 and par2 are the same when i1 and i2 are rational numbers, at least.
So, when addition, multiplication, etc. satisfy certain properties.
It's not clear these same properties are satisfied when working with intervals.

The answer here really depends on what is allowed with the package.
If mul and div are fixed, there's no way out.
Equivalent expressions using rationals won't translate to intervals.
Assuming, that is, something is different.

But really, the issue is this.
Resistor values are only know up to some tolerance.
These formulas are defined with respect to resistor values without accounting for tolerance.
It's a mistake to think operations on fixed values apply equally to intervals or whatever.
|#



;; Ex. 2.17

(define (last-pair l)
  (define (last-pear l e)
    (if (null? l)
        e
        (last-pear (cdr l) (car l))
        )
    )
      (last-pear l nil)
  )

(last-pair (list 23 72 149 34))
(last-pair (list 23 72 149))
(last-pair (list ))

;; Ex. 2.18


#|
See reverse as a special case of appending in reverse.
Then, reverse is just this with an empty list.
|#

(define (reverse l)
  (define (mirror-onto l1 l2)
    (if (null? l1)
        l2
        (mirror-onto (cdr l1) (cons (car l1) l2))
        )
    )
  (mirror-onto l nil)
  )

(reverse (list 23 72 149 34))


#|
With lists like this, there's a tradeoff between easy read and easy write.
And, as set up, it's easy read.
For, it's easy to go from the right element to the last.
It's not easy write, though, as you need to traverse through every element until you get to the end.

Though, you'd expect this to be the case in general.
We need to keep track of things, and the easy-write probably changes more than the easy to read thing.
|#


;; Ex. 2.19


(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc-list amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc-list amount
                     (except-first-denomination coin-values))
            (cc-list (- amount
                        (first-denomination coin-values)) coin-values)
            )
         )
        )
  )


(define (first-denomination coin-list) (car coin-list))
(define (except-first-denomination coin-list) (cdr coin-list))
(define (no-more? coin-list) (null? coin-list))

(cc-list 100 us-coins)
(cc-list 100 (reverse us-coins))

#|
The order of the coin list doesn't matter.
For, we always try to make the value using with and without the current 'first' coin.
So, if, say, 50 is up first, the first call will split into using 50 and ignoring 50.
While, if 50 is up somewhere in the middle, this split will happen multiple times, according to the splits that have already happened.
|#


;; Ex. 2.20


(define (same-parity e . l)
  (define (parity-list ie il)
    (if (null? il)
        nil
        (let ((eParity (remainder ie 2))
              (newElem (car il)))
          (if (= (remainder newElem 2) eParity)
            (cons newElem (parity-list ie (cdr il)))
            (parity-list ie (cdr il))
            )
          )
        )
    )
  (cons e (parity-list e l))
  )

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity -2 3 4 5 -6 7)

#|
I understand this way of writing procedures is useful.
Still, the application here seems strange.
same-parity feels like a regular procedure, which  takes some int and a list as an argument and returns a list.
Though, maybe I could think about this from a difference perspective.
At issue is the list is not explicit, but the same is true of +.
And in some sense the list is explicit, it's just that the first element is a procedure to apply.
And, kind of everything is a list in this way.
So, there's no need to make a list which contains a procedure and a list when we can just extend the list we'd be adding with the procedure.
Ok.
|#


;; Ex. 2.21

(define (my-map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (my-map proc (cdr items)))
      )
  )


(define (square-list-full items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list-full (cdr items)))
      )
  )


(define (square-list-my-map items)
  (my-map (lambda (x) (* x x)) items)
  )

(define testSqList (list 1 2 3 4))
(square-list-full testSqList)
(square-list-my-map testSqList)

#|
Yes, could have used (square x) in both, and yes in full this makes a difference as (car items) is only done once.
|#


;; Ex. 2.22


#|
So, we've got an inner iterative function.
Takes as argument a in-list and out-list
Idea is to transfer in-list to out-list.
But, lists  work in a specific order.
We have (element rest of list)
So, we read from a list top to bottom.
But, built a list bottom to top.
In this respect, Louis' 'top' element from in-list is added as the 'bottom' element of out-list.
|#


#|
In the second case we're not building a list.
|#



;; Ex. 2.23


(define (for-each proc l)
  (cond ((null? (cdr l)) (proc (car l)))
        (else
         (proc (car l)) (for-each proc (cdr l)))
      )
  )

#|
Ideally I'd use a pass procedure on the null? test here.
Then, we could check if the argument is nil, rather than looking a step ahead.
|#

(for-each (lambda (x) (display x) (newline))(list 57 321 88))
; Switched (newline) to second, to keep display style.


#|
As an aside, search seems difficult with lists.
Efficient search, that is.
For, it's easy to work through a list and check for equality.
But, it's hard to do a standard recurse onto sub-lists, as there's no quick way to make a sub-list.
That is, with this abstraction.
Though, moving to pointers things are very easy, especially if the length of the list is stored.
|#

;; Ex. 2.24


(list 1 (list 2 (list 3 4)))


#|
This is basically a tree which immediately terminates when branching to the left.
|#



;; Ex. 2.25


(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

#|
This last one is interesting.
(1 (2 …))
If do cdr then get (2 …).
Now, this is not simply a list with 2 as the first element.
For, then the initial argument would be (1 2 …)
So, we have a list with (2 …) as the first element.
|#


;; Ex. 2.26


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))
      )
  )


(define lx (list 1 2 3))
(define ly (list 4 5 6))

(append lx ly) ; A list of 1 2 3 4 5 6
(cons lx ly) ; A list of (list of 1 2 3) 4 5 6
(list lx ly) ; A list of (list of 1 2 3) (list of 4 5 6)


;; Ex. 2.27


;; (define (reverse l)
;;   (define (mirror-onto l1 l2)
;;     (if (null? l1)
;;         l2
;;         (mirror-onto (cdr l1) (cons (car l1) l2))
;;         )
;;     )
;;   (mirror-onto l nil)
;;   )

;; (reverse (list 23 72 149 34))


(define (deep-reverse l)
  (define (mirror-onto l1 l2)
    (if (null? l1)
        l2
        (mirror-onto (cdr l1) (cons (deep-reverse (car l1)) l2))
        )
    )
  (if (pair? l)
      (mirror-onto l nil)
      l
   ))

(define lx2 (list (list 1 2) (list 3 4)))
(define lx22 (list 2 (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4 5))))
lx2

(deep-reverse 3)
(deep-reverse (list 1 2))
(deep-reverse lx2)
(deep-reverse lx22)



;; Ex 2.28


(define (fringe tree)
  (cond ((and (not (pair? tree)) (not (null? tree))) (list tree))
        ((not (pair? tree)) nil)
        (else
         (append (fringe (car tree)) (fringe (cdr tree)))
         )
        )
  )

;; (define (fringe tree)
;;   (cond ((null? tree) nil)
;;         ((not (pair? tree)) (list tree))
;;         (else
;;          (append (fringe (car tree)) (fringe (cdr tree)))
;;          )
;;         )
;;   )

(define tx (list (list 1 2) (list 3 4)))
(define tx2 (list (list (list 1 2) (list 4)) (list (list 1 2) (list 3 4))))
(define tx3 (list (list 3 (list 1 2) (list 4)) (list (list 1 2) (list 3 4))))
(fringe tx3)
(fringe (list tx tx))

#|
It's simply to test to see if we have a leaf.
Though, there's an issue of nil, which always ends a list, so is always an implicit leaf, so to speak.
So, we traverse left and then right.
There are two base cases.
First, not pair and a non-nil.
In this case, a leaf.
Second, as the first condition failed, it's either an branch or nil.
If not branch, then for sure nil.

Building up, we append lists.
|#

#|
As an aside, append here is key to ensure we flatter everything, rather than repeating the strcture of the tree.
Setting aside scaling, this is the only difference between scale-tree and fringe, along with making sure the base case returns a list so append always works.

Commented version is a rewrite with hindsignt.
Base case conditions are in line with scale-tree
|#


;; Ex 2.29


(define (make-mobile left right) (list left right))

(define (make-branch length structure) (list length structure))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (car (cdr branch)))

; I guess there's a better way of doing this.
; As, sum-up here is just +, but applied to a list.
; (cons + list) doesn't work, though.
(define (sum-up list)
  (if (null? list)
      0
      (+ (car list) (sum-up (cdr list)))
      ))


#|
To test whether we're going to a mobile from another mobile,
we look at the right element.
If this is a pair, then we've got another mobile.
Else, it's an int representing some weight.
|#
(define (to-mobile? branch)
  (pair? (branch-structure branch)))

(define (get-weight branch)
  (branch-structure branch))

(define (total-weight mobile)
  (let ((twL (if (to-mobile? (left-branch mobile)) (total-weight (branch-structure (left-branch mobile))) (get-weight (left-branch mobile))))
        (twR (if (to-mobile? (right-branch mobile)) (total-weight (branch-structure (right-branch mobile))) (get-weight (right-branch mobile))))
        )
    (+ twL twR)
    )
  )


(define lb1 (make-branch 1 10))
(define rb1 (make-branch 1 10))
(define m1 (make-mobile lb1 rb1))
(define rb2 (make-branch 1 m1))
(define m2 (make-mobile lb1 rb2))
(define lb2 (make-branch 1 m1))
(define m3 (make-mobile lb2 rb2))
(total-weight m1)
(total-weight m2)

#|
A 'relaxed' version of total-weight, which may be applied to mobiles or weights.
|#
(define (total-weight-relaxed weight-or-mobile)
  (if (pair? weight-or-mobile)
      (total-weight weight-or-mobile)
      weight-or-mobile)
  )

(total-weight-relaxed 10)
(total-weight-relaxed m2)

(define (hang-weight branch)
  (* (branch-length branch) (total-weight-relaxed (branch-structure branch)))
  )


(define (isBalanced? mobile)
  (display mobile)
  (newline)
  (and
  (= (hang-weight (left-branch mobile)) (hang-weight (right-branch mobile)))
  (if (to-mobile? (left-branch mobile)) (isBalanced? (branch-structure (left-branch mobile))) #t)
  (if (to-mobile? (right-branch mobile)) (isBalanced? (branch-structure (right-branch mobile))) #t)
  )
  )

(isBalanced? m3)

#|
In short, we need a conjunction of balanced applied to main and all sub-mobiles.
So, we check weight * length for each branch of the mobile.
Then, we get to work on sub-mobiles.
There's no guarantee of sub-mobules, so we only check when sure.
And, otherwise return #t does the value of the conjunction is determined by all the other components.
The only thing to note with the recusrive case is that we need to move to the structure of the branch, rather than the branch itself.
|#


#|
I'd need to change right-branch and brach-structure procedures.
For, these assume we're working with a list.
With a list, the right element is always either a list or nil.
So, some extra work is needed to get any value.
In particular, we're always working with a pair.
So, to get the right hand value, we need to cdr then car.
But, if cons is used, then we're just working with two values.
Hence, we'd car and be done.
|#


;; Ex. 2.30


(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))
        )
  )

(scale-tree (list 1 (list 2 (list 3 4 5))) 4)

#|
Cons works fine here, as we're breaking a list down into it's basic elements, then building back up.
Things would be different if flattening the list, etc.
|#

(define (square-tree-basic tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-basic (car tree))
                    (square-tree-basic (cdr tree))))
        )
  )

(square-tree-basic (list 1 (list 2 (list 3 4) 5) (list 6 7)))


(define (square-tree-map tree)
  (my-map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree-map sub-tree)
                (* sub-tree sub-tree))
       )
  tree
  ))

(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))

#|
Right, map here just goes through each element in the list and applies the procedure.
So, what this does is abstracts from the way the list works, as emphasised in the book.
It's important to keep in mind this is the limit of what's happening.
The code should look mostly the same, given that little is being done to reconstruct the tree via lists.
|#


;; Ex. 2.31


(define (tree-map proc tree)
  (my-map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map proc sub-tree)
                (proc sub-tree))
       )
  tree
  ))

(define (square-tree-again tree) (tree-map square tree))


(square-tree-again (list 1 (list 2 (list 3 4) 5) (list 6 7)))


;; Ex. 2.32


(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (my-map (lambda (x) (cons (car s) x)) rest))))
  )

(subsets (list 1 2 3))

#|
So, we're splitting on the first element of the list.
Then, applying subsets to all other elements of the list.
We then keep a copy of every subset from the other elements of the list.
So, the only thing to do is ensure we also have a copy of the those subsets with the element we excluded.
This is what the lambda expresion does.

So, for example, (a b)
rest is just b.
subsets applied to b goes once more, to the value b and nil.
Now, on the way back, we have nil turned into an empty list, and b joined with the empty list.
So, we have (() (b)).
Now, we store a copy of this, and also consider a included.
So, this is ((a) (a b)).
Combined, we have (() (b) (a) (a b)).
|#



;; Ex. 2.33


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))
      )
  )


(define (map-again p sequence)
  (accumulate (lambda (x y) (cons (p x) y))  nil sequence))

(map-again square (list 1 2 3 4 5))

#|
Okay.
The point here is op is a two place argument.
The first argument to op is the current element of the sequence, and accumulate works through these one-by-one.
Then second argument to op is the result of accumulating.
So, for example, rather than cons, we could have summed.
|#

#|
E.g., map-sum maps the procedure and then sums the list.
|#


(define (map-sum p sequence)
  (accumulate (lambda (x y) (+ (p x) y)) 0 sequence))

(map-sum square (list 1 2 3 4 5))



(define (append-again seq1 seq2)
  (accumulate cons seq2 seq1)
  )

(append-again (list 1 2) (list 3 4))

#|
We're going through every element in list1 and cons to list2.
Here, though, you need some idea of the way lists and accumulate work.
For, if we worked through the list on call rather than close, this would reverse list1.
I like abstraction, but here I'm not sure what the point is, given we need the details to understand why the abstraction works.
|#

(define (length sequence)
  (accumulate (lambda (x y) (+ (if (null? x) 0 1) y)) 0 sequence))

(length (list 1 2 3))
(length (list ))


;; Ex. 2.34


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence)
  )


(horner-eval 2 (list 1 3 0 5 0 1))

#|
Right, I figured out the goal was to modify addition.
But, the choice of a and b was guess work.
I originially had (+ (* a x) b).
Thinking that a would be a_n at first.
But, it's not really higher-terms, it's higher-terms already done, right?

It's, what's already been done with the higher-terms, or something like this.

Right, this is clear when looking at the definition of accumulate.
We have op applied to the current first thing in the sequence and the rest.
So, by higher-terms this is the op-defined as applied to all the higher-terms from the current term.
|#



;; (define (horner-test x coefficient-sequence)
;;   (accumulate (lambda (this-coeff higher-terms) (display higher-terms) this-coeff higher-terms)
;;               0
;;               coefficient-sequence)
;;   )


;; (horner-test 2 (list 1 3 0 5 0 1))

#|
At this point in the book we're not only learning about the way abstractions are useful.
But, we're also learning about the way remembering what the abstractions really do is also useful.
|#



;; Ex. 2.35


(define (count-leaves-acc tree)
  (accumulate + 0 (map (lambda (x) 1 ) (fringe tree)))
  )

(count-leaves-acc m2)

#|
As I missing something here?
With an accumulator we need a list.
So, somehow we need to collapse the tree into a list.
fringe does this.
Then, somehow accumulate the elements of the list to get the number of leaves.
Well, here then just set every leaf value to 1.
|#


;; Ex. 2.36


#|
Here, a way to make a list of the first elements of the lists.
Then, the rest of the lists.
|#


(list (list 1 2 3) (list 4 5 6) (list 7 8 9 ) (list 10 1 1 12))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (my-map car seqs))
                        (accumulate-n op init (my-map cdr seqs)))))


(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9 ) (list 10 11 12)))

#|
Alright, this exercise was really cool!
|#



;; Ex. 2.37

(define test-matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))


(define (dot-product v w)
  (accumulate + 0 (map * v w))
  )


#|
Hold on, we defined map to take a proc and a list.
Yet, here, map takes a proc and two lists.
So, here we're using the base map mentioned in Footnote 12.
This takes a procedure of n arguments and n lists.
Then, applies the procedure to the ith element in each of the lists.

Oh, right, *this* is in footnote 17.
|#

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))


(define (transpose m)
  (accumulate-n cons nil m))

(transpose (list (list 1 2) (list 3 4) (list 5 6)))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)
    )
  )

(matrix-*-matrix (list (list 2 3 4) (list 1 0 0)) (list (list 0 1000) (list 1 100) (list 0 10)))


;; Ex. 2.38


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence)
  )

(accumulate / 1 (list 1 2 3)) ;  1 / (2 / 3)
(fold-left / 1 (list 1 2 3))  ; (1 / 2) / 3
(accumulate list nil (list 1 2 3)) ; With cons, the same list.
                                   ; With list, (list … (list nil))
                                   ; So, (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; With cons, (((nil 1) 2) 3)
                                  ; With list, (list (list nil 1) 2)

#|
I mean, op should be such that (op x y) = (op y x)
Right, commutative.

Hence, +, *, etc should work fine
|#

(= (accumulate * 1 (list 1 2 3)) (fold-left * 1 (list 1 2 3)))
(= (accumulate + 0 (list 2 3 5 9)) (fold-left + 0 (list 2 3 5 9)))

#|
Things like list fail as (list 2 3) = (2 3) != (3 2) = (list 3 2).
Same for /, ^, etc.
|#


;; Ex. 2.39

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
        (cons (car sequence)
              (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


#|
TODO

reverse-fr isn't it.
But, I'm drawing blanks.

The issue is, append-elem is a 'worse' version of append.
In that, with appened-elem we make all the same recursive calls, and all but the last are kind of pointless.
While, in the case of append these can be used for other elements.

I'm really drwawing blanks on a way to do this without refering to the procedure by name.
|#

(define (append-elem e list)
  (if (null? list)
      (cons e nil)
      (cons (car list) (append-elem e (cdr list)))
      )
  )


(define (reverse-fr sequence)
  (accumulate (lambda (x y) (append-elem x y)) nil sequence))

(reverse-fr (list 1 2 3))

(define (reverse-fl sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse-fl (list 1 2 3))





(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n))))
  )


(prime-sum-pairs 10)
(enumerate-interval 1 10)
(flatmap
 (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
 (enumerate-interval 1 10))
(flatmap (lambda (x) (list x x)) (enumerate-interval 1 10))

#|
flatmap

We take a procedure and a list.
The list can be of plain elements.
However, the procedure must return pairs.
So, in this case, after applying the procedure with map, we have a list of lists.
Or, at least, each elemnt mapped over is replaced by a list.
Flatmap then flattens this, so the original list is expanded with additional elements, but these aren't themselves inside a list.
|#


;; Ex. 2.40

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))
  )

(unique-pairs 10)


(define (prime-sum-pairs-up n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n)))
  )

(prime-sum-pairs-up 10)


#|
But, this was just cutting apart the definition of prime-sum-pairs…

Or was this the point? To make sure the reader understands what's going on…
|#


;; Ex. 2.41


(define (unique-triples n)
  (flatmap (lambda (i) (map (lambda (j) (cons i j)) (unique-pairs (- i 1)))) (enumerate-interval 1 n))
  )


(define (unique-triples-to-n-sum-to-s n sum)
  (filter (lambda (x) (= sum (accumulate + 0 x))) (unique-triples n))
  )

(unique-triples-to-n-sum-to-s 10 10)


;; Ex. 2.42

#|
Function which takes an index and a list.
Returns a ist where the first element is what was at the index, and the second elemnt is the list without the element at the index.

That is, the list with the ith element now the 0th element.

Originally this was thought of as the ith element removed and the rest of the list, but shifting to front is a more natural perspective.

Index starts at 0.
|#

(define (shift-to-front i list)
  (define (shift-list-i old new i n)
    (if (= i n)
        (cons (car old) (cons (cdr old) new))
        (shift-list-i (cons (car new) old) (cdr new) i (+ n 1)))
    )
  (define (recombine old new)
    (if (null? old)
        new
        (recombine (cdr old) (cons (car old) new)))
    )
  (let ((split (shift-list-i nil list i -1)))
    (cons (car split) (recombine (car (cdr split)) (cdr (cdr split))))
    )
  )

(cdr (shift-to-front 0 (list 1 2 3 4 5 6)))


(define tempGrid
  (list
   (list 1 2 3 4)
   (list 5 6 7 8)
   (list 9 10 11 12)
   ))


#|
A variant of map.
Instead of applying each item to proc, we apply a number corresponding to the instance of map and the item.
So, for example, (map-i proc (a b)) does (proc 0 a) and then (proc 1 b).
|#

(define (map-i-to-n proc items n)
  (define (map-apply-index proc i items n)
    (if (or (null? items) (= i n))
        nil
        (cons (proc i (car items))
              (map-apply-index proc (+ i 1) (cdr items) n))
        )
    )
  (map-apply-index proc 0 items n)
  )

#|
So, now it's easy to apply shift-to-front diagonally

E.g. (map-i shift-to-front grid) or to be explicit (map-i (lambda (i x) (shift-to-front i x)) grid)
The latter form is kind of helpful, as it's easy to discard i and only apply x.
|#


#|
Getting all the diagonals of a grid is a little difficult.
We use map-i-to-n to dynamically shift and limit the mapping.

The problem is whenever we do this, the grid is sure to become a rectangle.
And, while it's easy to make a function which works with a grid or wide rectange, extending this to a tall rectange isn't easy.

(Basically, (map-i-to-n shift-to-front tallGrid (length tallGrid)) only considers a grid.)

So, we avoid this issue by tranpsoing the grid if needed, to ensure it's always a grid or a wide rectangle.
It's easy to see diagonals are preserved under this.

The only issue is that this procedure is that it includes the empty list.
So, filter this out.
|#

(define (get-diags-lr grid)
  (define (collect-diag-to diagList grid)
    (if (null? grid)
        diagList
        (let ((tallGrid (cond ((null? grid) grid)
                              ((< (length (car grid)) (length grid)) (transpose grid))
                              (else grid))))
          (let ((shifted-list (map-i-to-n shift-to-front tallGrid (length tallGrid))))
            (collect-diag-to (append diagList (cons (map car shifted-list) nil)) (map cdr shifted-list))
            )
          )
        )
    )
  (filter (lambda (x) (not (null? x))) (collect-diag-to nil grid))
  )

(get-diags-lr (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

#|
get-diags-lr only gets diags going left to right.
But, given the representation of a grid here, there's an easy fix.
Apply reverse.
This gives us a mirror.
Hence, lr becomes rl.
|#

(define (get-diags-rl grid)
  (get-diags-lr (reverse grid)))

(map (lambda (x) (if (= 2 x) 0 x)) (enumerate-interval 1 10))


#|
We'll represent queens as 0s.
To test whether a board is safe, we check to see there are no two queens in a row column or diagonial.
get-diags gets us diagonials, grid is built with columsn, and transpose gets us rows.

So, all that's needed is a  couple of functions to ensure each list in a list of lists has at most one instance of a nubmer.
That's all these two functions do.
|#
(define (count-ns-in-list n l)
  (length (filter (lambda (x) (= x n)) l))
  )

(define (unique-n? n listOfLists)
  (if (null? listOfLists)
      #t
      (and (< (count-ns-in-list n (car listOfLists)) 2) (unique-n? n (cdr listOfLists))))
  )


(define (queens board-size)
  (define empty-board nil)  ; empty-board is nil.
                            ; adjoin-position will add a row.
  (define (safe? k positions) (and (unique-n? 0 (transpose positions)) (unique-n? 0 (get-diags-lr positions)) (unique-n? 0 (get-diags-rl positions))))
  (define (adjoin-position new-row k rest-of-queens)
    (cons (map (lambda (x) (if (= new-row x) 0 x)) (enumerate-interval 1 board-size)) rest-of-queens)
    ; Place the queen at the new-row position.
    )
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queens-cols (- k 1))))))
  (queens-cols board-size)
  )

(queens 4)


;; Ex. 2.43


#|
So, there's (queens-cols (- k 1)) and the number of times it's evaluated.
All this does is generate everything for the k - 1 case.
All here is a big all.
In the original proc, this is only evaluated once.
So, we make a lot of recursive calls, for sure, but these all stem from evaluating a single instance of queens-cols and going from there.

When things are flipped around, the proc is evluated for every instance in new-row.

Hence, if a single run takes T, then this is going to be even more than T * board-size.
Does it get close to T! ?

In any case, this is mostly about the way scheme evaluates.
So, moving on…
|#


;; Ex. 2.44

#|
Going by the suggestion, this should paint two copies side-by-side below the arg.
|#

;; (define (up-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (up-split painter (- n 1))))
;;         (below painter (beside smaller smaller)))))


;; Ex. 2.45

#|
For reference, here's right-split
|#

;; (define (right-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (right-split painter (- n 1))))
;;         (beside painter (below smaller small))
;;         ))
;;   )

#|
Goal is 'split' such that:

(define right-split (split beside below))
(define up-split (split below beside))

So, proc which takes painter and n as argument.
lambda makes this easy.
|#

;; (define (split firstProc secondProc)
;;   (lambda (painter n)
;;     (if (= n 0)
;;         painter
;;         (let ((smaller ((split firstProc secondProc) painter (- n 1))))
;;           (firstProc painter (secondProc smaller small))
;;         ))
;;         )
;;     )


;; Ex. 2.46

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v1)
  (make-vect (* s (xcor-vect v1)) (* s (ycor-vect v1))))


;; Ex. 2.47

(define (make-frameL origin edge1 edge2)
  (list origin edge1 edge2))

(define (frameL-origin frame)
  (car frame))

(define (frameL-edge1 frame)
  (cadr frame))

(define (frameL-edge2 frame)
  (caddr frame))


(define (make-frameC origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frameC-origin frame)
  (car frame))

(define (frameC-edge1 frame)
  (cadr frame))

(define (frameC-edge2 frame)
  (cddr frame))


;; Ex. 2.48


(define (make-segment origin start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

#|
There's nothing else here, right?
vec running from ogirin to the start-point is just start-point coordinates.
Same for end.

Unless the suggestion is to add start to origin, but I don't see good motivation for this.

On the other hand, I'm not sure why so many excercises are just cons car cdr…
|#


;; Ex. 2.49

#|
So, fix the relevant coordinates, and then pass a list of segments to segmemts->painter.

I'm not interested enough to do the rest.

wave seems… a lot of work for basically nothing.
|#


;; (define (paintFrame frame)
;;   (let ((bottomLeft (frame-origin frame))
;;         (bottomRight (add-vec (frame-origin frame) (frame-edge1 frame)))
;;         (topRight (add-vec bottomRight (frame-edge2 frame)))
;;         (topLeft (add-vec (frame-origin frame) (frame-edge2 frame)))
;;         )
;;     (segmemts->painter
;;    (list
;;     (make-segment bottomLeft bottomRight)
;;     (make-segment bottomRight topRight)
;;     (make-segment topRight topLeft)
;;     (make-segment topLeft bottomLeft)
;;     )
;;    ) frame
;;   )
;;   )


;; Ex. 2.50

;; (define (flip-hori painter)
;;   (transform-painter painter
;;                      (make-vect 0.0 1.0)
;;                      (make-vect 1.0 1.0)
;;                      (make-vect 0.0 0.0))
;;   )

#|
So, origin is now (0, 1).
New x points to (1, 1)
And, y points to (0, 0).
Basically, everything is now upside down.
|#

;; (define (rotate180 painter)
;;   (rotate90 (rotate90 painter))
;;   )

#|
I mean, really.
270 works the same way.

The poing here is to minimize where things can go wrong.
Applying a rotation twice is straightforward.
So, then, leave 90 as the only complex operation.

Or, create a general function that works with any degree.
|#
