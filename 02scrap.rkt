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


(+ (car '(a b c)) (car (list 'a 'b 'c)))
'()

#|
Okay, so here we quote a list of things.

'(a b c) = (list 'a 'b 'c)
|#
