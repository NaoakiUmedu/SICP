;; Huffman木の表現
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
		right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
	  (list (symbol-leaf tree))
	  (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
	  (weight-leaf tree)
	  (cadddr tree)))
;; 復号化手続き
(define (decode bits tree)
  (define (decode-1 bits current-branch)
	(if (null? bits)
		'()
		(let ((next-branch
			   (choose-branch (car bits) current-branch)))
		  (if (leaf? next-branch)
			  (cons (symbol-leaf next-branch)
					(decode-1 (cdr bits) tree))
			  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
		((= bit 1) (right-branch branch))
		(else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; 重み付き要素の集合
(define (adjoin-set x set)
  (cond ((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else (cons (car set)
					(adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
	  '()
	  (let ((pair (car pairs)))
		(adjoin-set (make-leaf (car pair)   ; 記号
							   (cadr pair)) ; 頻度
					(make-leaf-set (cdr pairs))))))

;; R2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
				  (make-code-tree
				   (make-leaf 'B 2)
				   (make-code-tree (make-leaf 'D 1)
								   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;; (A D A B B C A)

;; R2.68
(define (encode message tree)
  (if (null? message)
	  '()
	  (append (encode-symbol (car message) tree)
			  (encode (cdr message ) tree))))
;; element-of-set?を定義する
;; 自分にあれば止める
;; 左の木にあれば0、右の木にあれば1を返す
(define (encode-symbol sym tree)
  (cond ((and (leaf? tree)
			  (equal? sym (symbol-leaf tree)))
		 '())
		((element-of-tree? sym (left-branch tree))
		 (cons 0 (encode-symbol sym (left-branch tree))))
		((element-of-tree? sym (right-branch tree))
		 (cons 1 (encode-symbol sym (right-branch tree))))
		(else (error "symbol is not exist in tree" sym))))
(define (element-of-tree? sym tree)
  (define (element-of-list? ls)
	(cond ((null? ls) #f)
		  ((eq? (car ls) sym) #t)
		  (else (element-of-list? (cdr ls)))))
  (element-of-list? (symbols tree)))

;; R2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge pairs)
		(if (null? (cdr pairs))
			(car pairs)
			(successive-merge
			 ;; adjoin-setで重みを考慮する
			 (adjoin-set
			  (make-code-tree (car pairs) (cadr pairs))
			  (cddr pairs)))))

(define sample-pairs '((A 4) (B 2) (C 1)  (D 1)))
(define sample-leaves (make-leaf-set sample-pairs))
(define sample-tree2 (generate-huffman-tree sample-pairs))
;; ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))

;; R2.70
(define rock-encoder-raw '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define rock-tree (generate-huffman-tree
				   (sort
					rock-encoder-raw
					(lambda (x y) (> (cadr x) (cadr y))))))
(define rock-message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
;; 84bit
;; 8記号アルファベット文字列では36 * 3bit使う

;; R2.71
(define n5-pairs '((A 1) (B 2) (C 4) (D 8) (E 16)))
(define n5-tree (generate-huffman-tree n5-pairs))
;; gosh> (length (encode '(A) n5-tree))
;; 4 <- n-1bit
;; gosh> (length (encode '(E) n5-tree))
;; 1 <- 1bit

(define n10-pairs '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))
(define n10-tree (generate-huffman-tree n10-pairs))
;; gosh> (length (encode '(A) n10-tree))
;; 9 <- n-1bit
;; gosh> (length (encode '(J) n10-tree))
;; 1 <- 1bit

;; R2.72
;; 最低頻度の場合、
;; encode-symbolがn-1回、element-of-***が2^(n-1) - 1回
;; 最高頻度の場合、
;; encode-symbolが1回、element-of-***が1回よばれる
