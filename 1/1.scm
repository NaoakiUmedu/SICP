;; R1.1 略
;; R1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
;; R1.3
(define (f a b c)
		(cond ((= a (min a b c)) (* b c))
			  ((= b (min a b c)) (* a c))
			  (else (* a b))))
;; R1.4 略
;; R1.5 (p)を評価しようとして無限ループに陥る

;; 1.1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (square x) (* x x))
(define (my-sqrt x)
  (sqrt-iter 1.0 x))

;; R1.6 new-ifを展開しようとして無限に呼び出しが続く ifは特殊形式なので↑の展開順序に従わない

;; R1.7
;; 小さい数では誤差0.001では大きすぎ、大きい数では小さすぎるため
(my-sqrt 9000000000000000000222222222222222);; おそい...
(my-sqrt 0.00000001)
0.03125010656242753
;; こうすると十分早く、正確である
(define (good-enough? guess x)
  (= (improve guess x) guess))

;; R1.8
(define (my-cube-root x)
  (my-cube-root-itr 1.0 x))
(define (my-cube-root-itr guess x)
  (if (good-enough? guess x)
	  guess
	  (my-cube-root-itr (improve guess x) x)))
(define (good-enough? guess x)
  (= (improve guess x) guess))
(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

;; R1.9
;; 前者は再帰、後者は反復

;; R1.10
;; (f n) は 2n
;; (g n) は 2^n
;; (h n) は 2^2^n

;; R1.30
(define (sum term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a)
			  (+ (term a) result))))
  (iter a 0))

;; R1.31 a
(define (product term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a)
			  (* (term a) result))))
  (iter a 1))

(define (inc n) (+ n 1))
(define (identity n) n)

(define (factorial n)
  (product identity 1 inc n))

(define (pi-term n)
  (/ (* (- n 1) (+ n 1))
	 (* n n)))

(define (pi-next n)
  (+ n 2))

(define (pi-approximate)
  (* (product pi-term 3.0 pi-next 1000) 4))

;; R1.31 b
(define (product term a next b)
  (if (> a b)
	  1
	  (* (term a)
		 (product term (next a) next b))))

;; R1.32 a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
	  null-value
	  (combiner (term a)
				(accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))

;; R1.32 b
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a)
			  (combiner (term a) result))))
  (iter a null-value))

;; R1.33
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
		(cond ((> (square test-divisor) n) n)
			  ((divides? test-divisor n) test-divisor)
			  (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
		(= n (smallest-divisor n)))

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a)
			  (if (filter a)
				  (combiner (term a) result)
				  result))))
  (iter a null-value))
;; a
(define (prime-square-sum a b)
  (filtered-accumulate + 0 square a inc b prime?))
;; b
(define (prime-prod n)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (define (gcd a b)
	(if (> b a)
		(gcd b a)
		(if (= (mod a b) 0)
  			b
			(gcd b (mod a b)))))
  (define (pp-filter x)
	(= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc n pp-filter))

;; 1.35
(define (fixed-point f first-guess)
  (define (average a b) (/ (+ a b) 2))
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 10.0)

;; 1.36
(define (fixed-point f first-guess)
  (define (average a b) (/ (+ a b) 2))
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (display guess)
	  (newline)
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))
;; ふつう
(fixed-point (lambda (x) (/ (log 1000) (log x))) 10)
;; 平均緩和法(xと(f x)のaverageを使う
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2.0)) 2.0)

;; 1.37
(define (cont-frac n d k)
  (define (cf i)
	(if (= i k)
		(/ (n i) (d i))
		(/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 1))
(define (cont-frac-iter n d k)
  (define (cf res i)
    (if (= i 0)
        res
        (cf (/ (n i) (+ (d i) res)) (- i 1))))
  (cf (/ (n k) (d k)) (- k 1)))
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)

;; 1.38
(+ 2.0
   (cont-frac (lambda (i) 1.0)
			  (lambda (i) (if (= (remainder i 3) 2)
							  (/ (+ i i 2.0) 3.0)
							  1.0))
			  20))

;; 1.39
(define (cont-frac n d k)
  (define (cf res i)
	(if (= i 0)
		res
		(cf (/ (n i) (+ (d i) res)) (- i 1))))
  (cf (/ (n k) (d k) (- k 1))))
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
			 (lambda (i) (+ i i -1.0)) k))

;; 1.3.4
(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (square n) (* n n))
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
	(- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

;; R1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a (* x x)) (* b x) c)))

;; R1.41
(define (double f)
  (lambda (x) (f (f x))))

;; R1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; R1.43
(define (repeated f n)
  (define (repeated-itr cnt res)
	(if (<= n cnt)
		res
		(repeated-itr
		 (+ cnt 1)
		 (compose f res))))
  (repeated-itr 1 f))

;; (解答例にのっていた方)
(define (repeated f n)
  (if (= n 0)
	  (lambda (x) x)
	  (compose f (repeated f (- n 1)))))

;; R1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x 1)) (f x) (f (+ x 1))) 3)))
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;; R1.45
;; y = x/y^n-1 をk回average-dampした式のfixed-pointを求める
(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (square n) (* n n))
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))
(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 0)
	  (lambda (x) x)
	  (compose f (repeated f (- n 1)))))

;; n<=3:k=1 n<=7:k=2 n<=15:k=3 これは(log n) / (log 2)では?
(define (n-th-root n x)
  (let ((k (floor (/ (log n) (log 2)))))
	(fixed-point ((repeated average-damp k)
				  (lambda (y) (/ x (expt y (- n 1))))) 1.0)))

(define (repeated f n)
  (define (repeated-itr cnt res)
	(if (<= n cnt)
		res
		(repeated-itr
		 (+ cnt 1)
		 (compose f res))))
  (repeated-itr 1 f))

(do ((n 1 (+ n 1))) ((= n 32))
  (display (n-th-root n 2)) (newline))

;; R1.46
(define (iterative-improve good? improve)
  (define (improving guess)
	(if (good? guess)
		guess
		(improving (improve guess))))
  (lambda (first-guess)
	(improving first-guess)))

(define (sqrt n)
  (define (good-sqrt guess)
	(define tolerance 0.00001)
	(define (square x) (* x x))
;;	(display "guess is ") (display guess) (newline)
	(< (abs (- n (square guess))) tolerance))
  (define (improve-sqrt guess)
	(define (average a b) (/ (+ a b) 2))
	(average guess (/ n guess)))
  ((iterative-improve good-sqrt improve-sqrt) 1.0))

(define (fixed-point f first-guess)
  (define (good-fixed-point guess)
	(define tolerance 0.000001)
	(display "guess is ") (display guess) (newline)
	(< (abs (- guess (f guess))) tolerance))
  (define (improve-fixed-point guess)
	(f guess))
  ((iterative-improve good-fixed-point improve-fixed-point) first-guess))
