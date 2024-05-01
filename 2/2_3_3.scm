;; 集合の表現
(define (element-of-set? x set)
  (cond ((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
	  set
	  (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
		 (cons (car set1)
			   (intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))
;; R2.59
(define (union-set set1 set2)
  (define (iter s1 prd)
	(cond ((null? s1) (append prd set2))
		  ((element-of-set? (car s1) set2) (iter (cdr s1) prd))
		  (else (iter (cdr s1) (cons (car s1) prd)))))
  (iter set1 '()))

;; R2.60
;; (define (element-of-set? x set)
;;   (cond ((null? set) #f)
;; 		((= x (car set)) #t)
;; 		(else (element-of-set? x (cdr set)))))
;; (define (adjoin-set x set)
;;   (cons x set))
;; (define (union-set set1 set2)
;;   (append set1 set2))
;; (define (intersection-set set1 set2)
;;   (define (iter s1 s2 prd)
;; 	(cond ((or (null? s1) (null? s2)) prd)
;; 		  ((and (element-of-set? (car s1) s2)
;; 				(not (element-of-set? (car s1) prd)))
;; 		   (iter (cdr s1) s2 (append prd (list (car s1)))))
;; 		  (else (iter (cdr s1) s2 prd))))
;;   (iter set1 set2 '()))

;; 順序づけられたリストとしての集合
(define (element-of-set? x set)
  (cond ((null? set) #f)
		((= x (car set)) #t)
		((< x (car set)) #f)
		(else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) '()
	  (let((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2)
			   (cons x1
					 (intersection-set (cdr set1) (cdr set2))))
			  ((< x1 x2)
			   (intersection-set (cdr set1) set2))
			  ((< x2 x1)
			   (intersection-set set1 (cdr set2)))))))
;; R2.61
(define (adjoin-set x set)
  (cond
   ((null? set) (cons x set))
   ((= x (car set)) set)
   ;; xが残りの要素より小さければこれ以上の探索は不要
   ((< x (car set)) (cons x set))
   ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

;; R2.62
(define (union-set set1 set2)
  (define (iter s1 s2 prd)
	(cond ((null? s1) (append prd s2))
		  ((null? s2) (append prd s1))
		  ((= (car s1) (car s2)) (iter (cdr s1) (cdr s2) (append prd (list (car s1)))))
		  ((< (car s1) (car s2)) (iter (cdr s1) s2 (append prd (list (car s1)))))
		  (else (iter s1 (cdr s2) (append prd (list (car s2)))))))
  (iter set1 set2 '()))

;; 二進木としての集合
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define a
  (make-tree 7
			 (make-tree 3
						(make-tree 1 '() '())
						(make-tree 5 '() '()))
			 (make-tree 9
						'()
						(make-tree 11 '() '()))))
(define (element-of-set? x set)
  (cond ((null? set) #f)
		((= x (entry set)) #t)
		((< x (entry set))
		 (element-of-set? x (left-branch set)))
		((> x (entry set))
		 (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
		((= x (entry set)) set)
		((< x (entry set))
		 (make-tree (entry set)
					(adjoin-set x (left-branch set))
					(right-branch set)))
		((> x (entry set))
		 (make-tree (entry set)
					(left-branch set)
					(adjoin-set x (right-branch set))))))

;; R2.63
(define (tree->list-1 tree)
  (if (null? tree)
	  '()
	  (append (tree->list-1 (left-branch tree))
			  (cons (entry tree)
					(tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
	(if (null? tree)
		result-list
		(copy-to-list (left-branch tree)
					  (cons (entry tree)
							(copy-to-list (right-branch tree)
										  result-list)))))
  (copy-to-list tree '()))

;; a 同じリストを返す(どちらもleft-branchのあとにentryとright-branchをくっつけているだけ
;; b どちらもほぼ変わらない(1回の呼び出しにつき、2回の再帰呼び出しが走る)
;;   強いて言えばaの方は線形再帰やappendのオーバーヘッドがある

;; R2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
	  (cons '() elts)
	  (let ((left-size (quotient (- n 1) 2)))
		(let ((left-result (partial-tree elts left-size)))
		  (let ((left-tree (car left-result))
				(non-left-elts (cdr left-result))
				(right-size (- n (+ left-size 1))))
			(let ((this-entry (car non-left-elts))
				  (right-result (partial-tree (cdr non-left-elts)
											  right-size)))
			  (let ((right-tree (car right-result))
					(remaining-elts (cdr right-result)))
				(cons (make-tree this-entry left-tree right-tree)
					  remaining-elts))))))))
;; a
;; partial-treeは、リストの最初の(- n 1)/2要素を木の左の要素、((- n 1)/2) + 1こめの要素を木の見出し、次の(- n (木の左のサイズ + 1)を右の要素とした木と、その残りの要素を返す
;; (1 3 5 7 9 11) => (7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))
;; b 1回のpartial-treeごとに2回partial-treeがよばれるため、O(n * 2)

;; R2.65
(define tree->list tree->list-1)
(define (union-set-tree s1 s2)
  (list->tree
   (union-set (tree->list s1)
			  (tree->list s2))))
(define b  (list->tree '(1 2 3 4 5)))

(define (intersection-set-tree s1 s2)
  (list->tree
   (intersection-set (tree->list s1)
					 (tree->list s2))))

;; 集合と情報検索
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
		((equal? given-key (key (car set-of-records)))
		 (car set-of-records))
		(else (lookup given-key (cdr set-of-records)))))
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
		((samller given-key (key (car set-of-records)))
		 (lookup given-key (left-branch set-of-records)))
		((bigger given-key (key (car set-of-records)))
		 (lookup given-key (right-branch set-of-records)))
		((eqaul-key given-key (key (car set-of-records))) #t)))
