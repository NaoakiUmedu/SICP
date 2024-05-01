;; 記号微分
(define (deriv exp var)
  (cond ((number? exp) 0)
		((variable? exp)
		 (if (same-variable? exp var) 1 0))
		((sum? exp)
		 (make-sum (deriv (addend exp) var)
				   (deriv (augend exp) var)))
		((product? exp)
		 (make-sum
		  (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
		  (make-product (deriv (multiplier exp) var)
						(multiplicand exp))))
		(else
		 (error "unknown expression type --DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; a1, a2は数値ではなく変数の可能性がある
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list '* m1 m2))))

;; R2.56
(define (make-exponentiation u n)
  (cond ((=number? u 1) 1)
		((=number? n 0) 1)
		((=number? n 1) u)
		((and (number? u) (number? n)) (expt u n))
		(else (list '** u n))))
(define (base expo)
  (cadr expo))
(define (exponent expo)
  (caddr expo))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (deriv exp var)
  (cond ((number? exp) 0)
		((variable? exp)
		 (if (same-variable? exp var) 1 0))
		((sum? exp)
		 (make-sum (deriv (addend exp) var)
				   (deriv (augend exp) var)))
		((product? exp)
		 (make-sum
		  (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
		  (make-product (deriv (multiplier exp) var)
						(multiplicand exp))))
		((exponentiation? exp)
		 (make-product (exponent exp)
					   (make-exponentiation
						(base exp)
						(make-sum (exponent exp) -1))))
		(else
		 (error "unknown expression type --DERIV" exp))))

;; R2.57
;; (define (sum? x)
;;   (and (pair? x) (eq? (cadr x) '+)))
;; (define (addend s)
;;   (car s))
;; (define (product? x)
;;   (and (pair? x) (eq? (cadr x) '*)))
;; (define (multiplier m)
;;   (car m))

;; R2.58
(define (compile exp)
  (cond
   ;; シンボル・数字だけならそのままでよい
   ((symbol? exp) exp)
   ((number? exp) exp)
   ;; 前置演算子なら、コンパイル完了
   ((eq? (car exp) '+) exp)
   ((eq? (car exp) '*) exp)
   ;; 要素が3つ(前置演算子と子expが2つにできる)なら、成形
   ((= (length exp) 3)
	(list (cadr exp) (compile (car exp)) (compile (caddr exp))))
   ((> (length exp) 4)
	(cond
	 ((eq? (cadr exp) (cadddr exp))
	  (compile (cons (list (cadr exp) (compile (car exp)) (compile (caddr exp)))
					 (cdddr exp))))
	 ;;  v + x * y z..の場合は、掛け算を先にする
	 ((and (eq? (cadr exp) '+) (eq? (cadddr exp) '*))
	  (let ((a (car exp))
			(b (cadr exp))
			(c (caddr exp))
			(d (cadddr exp))
			(e (list-ref exp 4))
			(f (list-tail exp 5)))
		(compile (cons a (cons b (cons (list d (compile c) (compile e))
										f))))))))))
