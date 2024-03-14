;; 2.17
(define (last-pair ls)
  (if (null? ls)
	  '()
	  (if (null? (cdr ls))
		  (car ls)
		  (last-pair (cdr ls)))))
;(print (last-pair (list 23 72 149 34)))
;; 2.18
(define (reverse ls)
  (define (rvs lst prd)
	(if (null? lst)
		prd
		(if (pair? lst)
			(rvs (cdr lst) (cons (car lst) prd))
			(cons lst prd))))
  (rvs ls '()))
;(display '(1 2 3 4))
;(display (reverse 1))

;; 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else
		 (+ (cc amount
				(except-first-denomination
				 coin-values))
			(cc (- amount
				   (first-denomination
					coin-values))
				coin-values)))))
(define (no-more? coin-values)
  (null? coin-values))
(define (except-first-denomination ls)
  (cdr ls))
(define (first-denomination ls)
  (car ls))
;; (print (cc 100 us-coins))
;; (print (cc 100 uk-coins))
;; コインの順番は関係ない 各コインを0枚だけ使う(=使わない)場合を含め全探索しているから

;; R2.20
(define (same-parity ls)
  (define head (car ls))
  (define (sp lst)
	(if (null? lst)
		'()
		(if (= (remainder head 2) (remainder (car lst) 2))
			(cons  (car lst) (sp (cdr lst)))
			(sp (cdr lst)))))
  (sp ls))
;; (print (same-parity '(1 2 3 4 5 6)))
;; (print (same-parity '(2 3 4 5 6)))

;; R2.21
(define (square-list items)
  (if (null? items)
	  '()
	  (cons (* (car items) (car items)) (square-list (cdr items)))))


(define (square-list items)
  (map (lambda (x) (* x x)) items))


;; R2.22
;; 前者について、最後のconsで今回のsquareをcar,前回までのsquareのリストをcdrに付けている
;; thingsを前から後ろへたどっているため、入力と逆順のリストが出来上がる

;; 後者について、いちばん前が'()になっているため、answerがリストにならない

;; R2.23
(define (for-each factor ls)
  (if (null? ls)
	  '()
	  (begin
		(factor (car ls))
		(for-each factor (cdr ls)))))
;(for-each (lambda (x) (newline) (display x)) '(57 321 88))


(define (count-leaves tree)
		(if (null? tree)
			0
			(if (pair? tree)
				(+ (count-leaves (car tree))
				   (count-leaves (cdr tree)))
				1)))

;; R2.25
;; (define a (list 1 3 (list 5 7) 9))
;; (car (cdr (car (cdr (cdr a)))))

;; (define b (list (list 7)))
;; (car (car b))

;; (define c (list 1 (list 2 (list 3 (list 5 (list 6 7))))))
;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))

;; R2.26
;; (define x (list 1 2 3))
;; (define y (list 4 5 6))
;; (append x y)							; (1 2 3 4 5 6)
;; (cons x y)								; ((1 2 3) 4 5 6) yのおしりの'()がlistのおしりになる
;; (list x y)								; ((1 2 3) (4 5 6))

;; R2.27
(define (reverse ls)
  (define (reverse-iter lst prd)
	(if (null? lst)
		prd
		(reverse-iter (cdr lst) (cons (car lst) prd))))
  (reverse-iter ls '()))
(define (deep-reverse ls)
  (if (null? ls)
	  '()
	  (if (pair? (car ls))
		  ;; deep-reverseはlistを返すためappend
		  (append (deep-reverse (cdr ls)) (list (deep-reverse (car ls))))
		  (append (deep-reverse (cdr ls)) (list (car ls))))))

;; (define x (list (list 1 2) (list 3 4)))
;; (display x)
;; (newline)
;; (display (deep-reverse x))

;; R2.28
(define (fringe tree)
  (if (null? tree)
	  '()
	  (if (pair? (car tree))
		  ; fringeはlistを返すためappend
		  (append (fringe (car tree)) (fringe (cdr tree)))
		  (append (list (car tree)) (fringe (cdr tree))))))

;; R2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define x
  (make-mobile
   (make-branch 1 2)
   (make-branch 3 (make-mobile
				   (make-branch 4 5)
				   (make-branch 6 7)))))
;; a.
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;; b.
(define (is-leaf? branch)
  (if (list? (branch-structure branch))
	  #f
	  #t))
(define (total-weight mobile)
		(+ (if (is-leaf? (left-branch mobile))
			   (branch-structure (left-branch mobile))
			   (total-weight (branch-structure (left-branch mobile))))
		   (if (is-leaf? (right-branch mobile))
			   (branch-structure (right-branch mobile))
			   (total-weight (branch-structure (right-branch mobile))))))

;(total-weight x)

;; c.
(define (balanced? mobile)
  (= (total-weight (branch-structure (left-branch mobile)))
	 (total-weight (branch-structure (right-branch mobile)))))

(define a (make-mobile
		   (make-branch 3 (make-mobile (make-branch 1 2)
									   (make-branch 3 6)))
		   (make-branch 4 (make-mobile (make-branch 1 5)
									   (make-branch 7 4)))))
(define b (make-mobile
		   (make-branch 3 (make-mobile (make-branch 1 2)
									   (make-branch 3 6)))
		   (make-branch 4 (make-mobile (make-branch 1 4)
									   (make-branch 7 4)))))

;; (balanced? a)
;; (balanced? b)

;; d.
(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (left-branch m)
  (car m))
(define (right-branch m)
  (cdr m))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cdr b))
(define (is-leaf? branch)
  (not (pair? (branch-structure branch))))

;; R2.30
(define (square-tree tree)
  (if (null? tree)
	  '()
	  (if (pair? (car tree))
		  (cons (square-tree (car tree)) (square-tree (cdr tree)))
		  (cons (* (car tree) (car tree)) (square-tree (cdr tree))))))
(define (square-tree tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
			 (square-tree sub-tree)
			 (* sub-tree sub-tree)))
	   tree))

;; R2.31
(define (tree-map process tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
			 (tree-map process sub-tree)
			 (process sub-tree)))
	   tree))
(define (square-tree tree) (tree-map square tree))

;; R2.32
(define (subsets s)
  (if (null? s)
	  (list '())
	  (let ((rest (subsets (cdr s))))
		(display rest) (newline)
		(append rest (map (lambda (x) (cons (car s) x)) rest)))))
;; 実行例
;; (())
;; (() (3))
;; (() (3) (2) (2 3))
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;; restは必ずlistのlistになっている
;; 最後の行は、restに、restの各要素に(car s)を追加したものを追加している

;; subsetsが引数sのすべての部分集合のリストを返すとすると、restはsの先頭要素をのぞいた集合のすべての部分集合のリスト
;; sのすべての部分集合のリストは、restと、restの各要素にsの先頭要素を追加したものとの合成となる

;; 2.2.3
(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence)) (cons (car sequence)
										  (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
	  '()
	  (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
					  (enumerate-tree (cdr tree))))))

;; R2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; R2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
			  0
			  coefficient-sequence))

;; R2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

;; R2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	  '()
	  (cons (accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs)))))

;; R2.37
(define (dot-product v m)
  (accumulate + 0 (map * v m)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
	(map (lambda (x) (matrix-*-vector cols x)) m)))

;; R2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
	(if (null? rest)
		result
		(iter (op result (car rest))
			  (cdr rest))))
  (iter initial sequence))
; (二項演算opが可換であること)

;; R2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))

;; 写像の入れ子
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (prime? n)
  (define (prime-iter x)
	(if (= 0 (remainder n x))
		x
		(prime-iter (- x 1))))
  (cond ((= n 1) #f)
		((= n 2) #t)
		(else (= 1 (prime-iter (ceiling (sqrt n)))))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (flatmap (lambda (i)
						  (map (lambda (j) (list i j))
							   (enumerate-interval 1 (- i 1))))
						(enumerate-interval 1 n)))))

(define (remove val seq)
  (filter (lambda (x) (not (= val x))) seq))
(define (permutations s)
  (if (null? s)
	  (list '())
	  (flatmap (lambda (x)
				 (map (lambda (p) (cons x p))
					  (permutations (remove x s))))
			   s)))

;; R2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
			 (map (lambda (j) (list i j))
				  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (unique-pairs n))))

;; R2.41
;; flatmapは空集合を除いている(append '() '()) => '()
(define (unique-trios n)
  (flatmap (lambda (i)
			 (flatmap (lambda (j)
						(map (lambda (k) (list i j k))
							 (enumerate-interval 1 (- j 1))))
					  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))
(define (s-sum-pairs s)
  (filter (lambda (x)
				  (= s (+ (car x) (cadr x) (caddr x))))
		  (unique-trios s)))

;; R2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
			(display (number-list->string rest-of-queens)) (newline)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define empty-board '())

(define (safe? k positions)
  (define (safe-iter x n)
	(or (= n k)
		(let ((y (list-ref positions n)))
		  (and (not (= x y))       ;; 同じ行ならアウト
			   (not (= (- x y) n)) ;; 斜めの線はy=x+nになる
			   (not (= (- y x) n)) ;; ↑の逆
			   (safe-iter x (+ n 1))))))
  (display (string-append "safe? " (number->string k) " (" (number-list->string positions) ")")) (newline)
  (safe-iter (car positions) 1))

;; R2.43
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
		  (adjoin-position new-row k rest-of-queens))
		(queen-cols (- k 1)))
   (enumerate-interval 1 board-size)))

;; もとのqueensではqueen-colsはboard-size回呼ばれている

;; Louisのqueensでは(new-row)につき1回よばれている
;; たとえば(queen-cols 8)では(queen-cols 7)がboard-size回よばれる
;; 上記よりよってboard-size=8の場合、1 + 1*8 + 1*8*8 + 1*8*8*8 ....
;;                                8   7     6       5
;; => 1 + 8^1 + 8^2 + 8^3 + 8^4 + 8^5 + 8^6 + 8^7 = (8^8 - 1)/7
;; 2.42ではqueens-colは8回なので、Louisは2.42に対し(8^8 - 1)/7/8倍遅い
