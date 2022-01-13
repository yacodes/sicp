;; --------------------------------
;; Exercise 1.1
;; Below is a sequence of expressions.
;; What is the result printed by the interpreter in response to each expression?
;; Assume that the sequence is to be evaluated in the order in which it is presented. 
10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3) ;; 
(define b (+ a 1)) ;;
(+ a b (* a b)) ;; 19
(= a b) ;; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ;; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; 16

;; --------------------------------
;; Exercise 1.2
;; Translate the following expression into prefix form: 
;;                5 + 4 + (2 - (3 - (6 + 4/5)))
;;                -----------------------------
;;                       3(6 - 2)(2 - 7)
(/
 (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
 (* 3 (- 6 2) (- 2 7)))
;; = -37/150

;; --------------------------------
;; Exercise 1.3
;; Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers. 
(define (largest-squares x y z)
  (cond
   ((and (>= x z) (>= y z)) (+ (* x x) (* y y)))
   ((and (>= y x) (>= z x)) (+ (* y y) (* z z)))
   ((and (>= x y) (>= z y)) (+ (* x x) (* z z)))))

(largest-squares 10 20 30)
;; = 13

;; --------------------------------
;; Exercise 1.4
;; Observe that our model of evaluation allows for combinations whose operators are compound expressions.
;; Use this observation to describe the behavior of the following procedure: 
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; if b > 0 -> (+ a b)
;; if b <= 0 -> (- a b)

;; --------------------------------
;; Exercise 1.5
;; Ben Bitdiddle has invented a test to determine whether the interpreter
;; he is faced with is using applicative-order evaluation or normal-order evaluation.
;; He defines the following two procedures: 
(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))

;; Then he evaluates the expression:
(test 0 (p))

;; What behavior will Ben observe with an interpreter that uses applicative-order evaluation?
;; What behavior will he observe with an interpreter that uses normal-order evaluation?
;; Explain your answer. 

;; Normal-order evaluation will return 0, when applicative-order evaluation will never terminate,
;; cause the value of (p) will be expanded to the infite loop.
(define (square x)
  (* x x))

(define (average x y) 
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(sqrt 9)
(sqrt-iter 2.0 9)

;; -------------------------------- 
;; Exercise 1.6
;; Alyssa P. Hacker doesn’t see why if needs to be provided as a special form.
;; “Why can’t I just define it as an ordinary procedure in terms of cond?” she asks.
;; Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if: 
(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Eva demonstrates the program for Alyssa: 
(new-if (= 2 3) 0 5)
;; = 5

(new-if (= 1 1) 0 5)
;; = 0

;; Delighted, Alyssa uses new-if to rewrite the square-root program: 
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(sqrt-iter 2.0 9)

;; sqrt-iter recursively expands to itself, cause of the applicative-order evaluation.
;; This behaviour causing the procedure to hang forever.

;; -------------------------------- 
;; Exercise 1.7
;; The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision.
;; This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers? 
