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
