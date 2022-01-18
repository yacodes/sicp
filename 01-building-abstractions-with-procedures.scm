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
;; The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers? 
(define (sqrt-iter guess last-guess x)
  (let ([good-enough? (< (abs (- guess last-guess)) 0.001)]
        [next-guess (average guess (/ x guess))])
    (if good-enough? guess
        (sqrt-iter next-guess guess x))))

(sqrt-iter 2.0 2.0 9)

;; -------------------------------- 
;; Exercise 1.8: Newton’s method for cube roots is based on the fact that if y is an approximation to the cube root of x , then a better approximation is given by the value
;; x / y^2 + 2y
;; ------------
;;      3
;; Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In 1.3.4 we will see how to implement Newton’s method in general as an abstraction of these square-root and cube-root procedures.) 
(define (sqrt-iter guess last-guess x)
  (let ([good-enough? (< (abs (- guess last-guess)) 0.001)]
        [next-guess (/ (+ (/ x (square guess))
                       (* 2 guess))
                    3)])
    (if good-enough? guess
        (sqrt-iter next-guess guess x))))

;; -------------------------------- 
;; Exercise 1.9: Each of the following two procedures defines a method for adding two positive integers in terms of the procedures inc, which increments its argument by 1, and dec, which decrements its argument by 1. 
(define (+ a b)
  (if (= a 0) 
      b 
      (1+ (+ (1- a) b))))

(define (+ a b)
  (if (= a 0) 
      b 
      (+ (1- a) (1+ b))))

;; Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4 5). Are these processes iterative or recursive? 

(define (+ a b)
  (if (= a 0) 
      b 
      (1+ (+ (1- a) b))))

,trace (+ 4 5)
;; trace: |  (+ 4 5)
;; trace: |  |  (+ 3 5)
;; trace: |  |  |  (+ 2 5)
;; trace: |  |  |  |  (+ 1 5)
;; trace: |  |  |  |  |  (+ 0 5)
;; trace: |  |  |  |  |  5
;; trace: |  |  |  |  6
;; trace: |  |  |  7
;; trace: |  |  8
;; trace: |  9

(define (+ a b)
  (if (= a 0) 
      b 
      (+ (1- a) (1+ b))))

,trace
(+ 4 5)
;; trace: |  (+ 4 5)
;; trace: |  (+ 3 6)
;; trace: |  (+ 2 7)
;; trace: |  (+ 1 8)
;; trace: |  (+ 0 9)
;; trace: |  9


;; -------------------------------- 
;; Exercise 1.10: The following procedure computes a mathematical function called Ackermann’s function. 
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; What are the values of the following expressions? 
(A 1 10) ;; 1024
(A 2 4) ;; 65536
(A 3 3) ;; 65536

;; Consider the following procedures, where A is the procedure defined above: 
(define (f n) (A 0 n))   ;; 2n
(define (g n) (A 1 n))   ;; n^2
(define (h n) (A 2 n))   ;; 2!n
(define (k n) (* 5 n n)) ;; 5n^2

;; Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive integer values of n. For example, (k n) computes 5n^2 . 

,trace (A 2 3)
;; trace: |  (A 2 3)
;; trace: |  |  (A 2 2)
;; trace: |  |  |  (A 2 1)
;; trace: |  |  |  2
;; trace: |  |  (A 1 2)
;; trace: |  |  |  (A 1 1)
;; trace: |  |  |  2
;; trace: |  |  (A 0 2)
;; trace: |  |  4
;; trace: |  (A 1 4)
;; trace: |  |  (A 1 3)
;; trace: |  |  |  (A 1 2)
;; trace: |  |  |  |  (A 1 1)
;; trace: |  |  |  |  2
;; trace: |  |  |  (A 0 2)
;; trace: |  |  |  4
;; trace: |  |  (A 0 4)
;; trace: |  |  8
;; trace: |  (A 0 8)
;; trace: |  16

;; -------------------------------- 
;; Exercise 1.11: A function f is defined by the rule that f ( n ) = n if n < 3 and f ( n ) = f ( n − 1 ) + 2 f ( n − 2 ) + 3 f ( n − 3 ) if n ≥ 3 . Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

;; Recursive process
(define (f n)
  (if (< n 3) n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; Iterative process
(define (f n)
  (define (iter a b c count)
    (if (= count n) c
        (iter (+ a (* 2 b) (* 3 c))
              a 
              b
              (1+ count))))
  (iter 2 1 0 0))

;; --------------------------------


;; --------------------------------
;; Exercise 1.12: The following pattern of numbers is called Pascal’s triangle.
;;
;;          1
;;        1   1
;;      1   2   1
;;    1   3   3   1
;;  1   4   6   4   1
;;        . . .
;;
;; The numbers at the edge of the triangle are all 1, and each number inside the triangle
;; is the sum of the two numbers above it. Write a procedure that computes elements of Pascal’s triangle by means of a recursive process. 

;; @todo what ,` and @ means?
;; https://exercism.org/tracks/scheme/exercises/pascals-triangle/solutions/jitwit
(define (pascals-triangle n)
  (let iter ((n n) (row '(1)))
    (if (zero? n) '()
        `(,row ,@(iter (1- n) (map + `(0 ,@row) `(,@row 0)))))))

(pascals-triangle 5)
;; --------------------------------


;; --------------------------------
;; Exercise 1.13: Prove that Fib ( n ) is the closest integer to φ n / 5 , where φ = ( 1 + 5 ) / 2 . Hint: Let ψ = ( 1 − 5 ) / 2 . Use induction and the definition of the Fibonacci numbers (see 1.2.2) to prove that Fib ( n ) = ( φ n − ψ n ) / 5 . 
(display "wtf\n")
;; --------------------------------


;; --------------------------------
;; Exercise 1.14: Draw the tree illustrating the process generated by the count-change procedure of 1.2.2 in making change for 11 cents. What are the orders of growth of the space and number of steps used by this process as the amount to be changed increases? 
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination 
                           kinds-of-coins))
                kinds-of-coins)))))

(define (count-change amount)
  (cc amount 5))

;; ,trace (count-change 11)
;; trace: |  (count-change 11)
;; trace: |  (cc 11 5)
;; trace: |  |  (cc 11 4)
;; trace: |  |  |  (cc 11 3)
;; trace: |  |  |  |  (cc 11 2)
;; trace: |  |  |  |  |  (cc 11 1)
;; trace: |  |  |  |  |  |  (cc 11 0)
;; trace: |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  (cc 10 1)
;; trace: |  |  |  |  |  |  |  (cc 10 0)
;; trace: |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  (cc 9 1)
;; trace: |  |  |  |  |  |  |  |  (cc 9 0)
;; trace: |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  (cc 8 1)
;; trace: |  |  |  |  |  |  |  |  |  (cc 8 0)
;; trace: |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  (cc 7 1)
;; trace: |  |  |  |  |  |  |  |  |  |  (cc 7 0)
;; trace: |  |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  (cc 6 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  (cc 6 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  (cc 5 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  (cc 5 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  (cc 4 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 4 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 3 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 3 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 2 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 2 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 1 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 1 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 0 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  1
;; trace: |  |  |  |  |  1
;; trace: |  |  |  |  |  (first-denomination 2)
;; trace: |  |  |  |  |  5
;; trace: |  |  |  |  |  (cc 6 2)
;; trace: |  |  |  |  |  |  (cc 6 1)
;; trace: |  |  |  |  |  |  |  (cc 6 0)
;; trace: |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  (cc 5 1)
;; trace: |  |  |  |  |  |  |  |  (cc 5 0)
;; trace: |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  (cc 4 1)
;; trace: |  |  |  |  |  |  |  |  |  (cc 4 0)
;; trace: |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  (cc 3 1)
;; trace: |  |  |  |  |  |  |  |  |  |  (cc 3 0)
;; trace: |  |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  (cc 2 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  (cc 2 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  (cc 1 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  (cc 1 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  (cc 0 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  (first-denomination 2)
;; trace: |  |  |  |  |  |  5
;; trace: |  |  |  |  |  |  (cc 1 2)
;; trace: |  |  |  |  |  |  |  (cc 1 1)
;; trace: |  |  |  |  |  |  |  |  (cc 1 0)
;; trace: |  |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  |  (cc 0 1)
;; trace: |  |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  (first-denomination 2)
;; trace: |  |  |  |  |  |  |  5
;; trace: |  |  |  |  |  |  |  (cc -4 2)
;; trace: |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  1
;; trace: |  |  |  |  |  2
;; trace: |  |  |  |  3
;; trace: |  |  |  |  (first-denomination 3)
;; trace: |  |  |  |  10
;; trace: |  |  |  |  (cc 1 3)
;; trace: |  |  |  |  |  (cc 1 2)
;; trace: |  |  |  |  |  |  (cc 1 1)
;; trace: |  |  |  |  |  |  |  (cc 1 0)
;; trace: |  |  |  |  |  |  |  0
;; trace: |  |  |  |  |  |  |  (first-denomination 1)
;; trace: |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  |  (cc 0 1)
;; trace: |  |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  1
;; trace: |  |  |  |  |  |  (first-denomination 2)
;; trace: |  |  |  |  |  |  5
;; trace: |  |  |  |  |  |  (cc -4 2)
;; trace: |  |  |  |  |  |  0
;; trace: |  |  |  |  |  1
;; trace: |  |  |  |  |  (first-denomination 3)
;; trace: |  |  |  |  |  10
;; trace: |  |  |  |  |  (cc -9 3)
;; trace: |  |  |  |  |  0
;; trace: |  |  |  |  1
;; trace: |  |  |  4
;; trace: |  |  |  (first-denomination 4)
;; trace: |  |  |  25
;; trace: |  |  |  (cc -14 4)
;; trace: |  |  |  0
;; trace: |  |  4
;; trace: |  |  (first-denomination 5)
;; trace: |  |  50
;; trace: |  |  (cc -39 5)
;; trace: |  |  0
;; trace: |  4
;;
;; https://codology.net/post/sicp-solution-exercise-1-14/
;; Order of growth of space: Θ(n)
;; Order of growth of number of steps: Θ(n^5)

;; --------------------------------


;; --------------------------------
;; Exercise 1.15: The sine of an angle (specified in radians) can be computed by making use of the approximation sin ⁡ x ≈ x if x is sufficiently small, and the trigonometric identity 
;; sin(x) = 3sin(x/3) − 4sin^3(x/3)
;; to reduce the size of the argument of sin. (For purposes of this exercise an angle is considered “sufficiently small” if its magnitude is not greater than 0.1 radians.) These ideas are incorporated in the following procedures: 

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; 1. How many times is the procedure p applied when (sine 12.15) is evaluated? 
(sine 12.15)
,trace (sine 12.15)
;; The `p` procedure is evaluated 5 times.

;; 2. What is the order of growth in space and number of steps (as a function of a) used by the process generated by the sine procedure when (sine a) is evaluated? 
;; https://codology.net/post/sicp-solution-exercise-1-15/
;; The order of growth in number of steps is Θ(log(a)).
;; --------------------------------
