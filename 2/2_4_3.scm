;; 2.4.3 データ主導プログラミングと加法性
(define (install-rectangle-package)
  ;; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
	(sqrt (+ (square (real-part z))
			 (square (imag-part z)))))
  (define (angle z)
	(atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
	(cons (* r (cos a)) (* r (sin a))))
  ;; システムの他の部分とのインタフェース
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
	   (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
	   (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; 内部手続き
  (define (real-part z)
	(* (magnitude z) (cos (angle z))))
  (define (imag-part z)
	(* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-real-imag x y)
	(cons (sqrt (+ (square x) (square y)))
		  (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))
  ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; 一般「演算」手続き
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (apply proc (map contents args))
		  (error
		   ("No method for these types -- APPLY-GENERIC"
			(list op type-tags)))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (mamgnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
;; 実部と虚部があるときには直行座標数、絶対値と偏角があるときには極座標数、みたいなこともできる
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; put/getを実現するための手続き
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; R 2.73
;; before (略)
;; after
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;; a
;; number? variable?以外のケースをoperator/operandsの組であると一般化した。
;; operator/operandsには演算子や被演算子が入る
;; number?やvariable?の場合、operatorに当たる演算子が存在しないため
;; データ主導の振り分けに吸収できない

;; b c
;; 足し算掛け算べき乗の微分を定義したパッケージ
(define (deriv-package)
  ;; 本体
  (define (deriv-sum exp var)
	(make-sum (deriv (addend exp) var)
			  (deriv (augend exp) var)))
  (define (deriv-prod exp var)
	(make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  (define (deriv-exp exp var)
	(make-product (exponent exp)
				  (make-exponentiation
				   (base exp)
				   (make-sum (exponent exp) -1))))
  ;; 本体を実現するための手続き
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  ;; 演算子のシンボルをリストに入れる
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product m1 m2) (list '* m1 m2))
  (define (make-exponentiation u n)
	(cond ((=number? u 1) 1)
		  ((=number? n 0) 1)
		  ((=number? n 1) u)
		  ((and (number? u) (number? n)) (expt u n))
		  (else (list '** u n))))
  (define (base expo)
	(car expo))
  (define (exponent expo)
	(cadr expo))
  ;; システムの他の部分とのインターフェース
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-prod)
  (put 'deriv '** deriv-exp))
;; その他手続き
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; d
;; 上記deriv-packageのputしてるところの演算子と'derivの順番を入れ替えるだけ

;; R2.74
;; a
;; それぞれの事業所のファイルは名前をキーにaddressやsalaryを取得できなければならない
;; ファイルの定義例
(define tanuki-file
  (list
   'tanuki
   (list (list 'ponta
			   (list 'address "Kinshicho")
			   (list 'salary 298000))
		 (list 'ponkichi
			   (list 'address "Isezakichojamachi")
			   (list 'salary 279000)))))
;; get-recordに必要な定義
(define (type-tag file)
  (car file))
(define (get-record file name)
  (define (make-record name address salary)
	(list name address salary))
  (if (null? ((get (type-tag file) 'get-name) file name))
	  '()
	  (make-record
	   name
	   ((get (type-tag file) 'get-address) file name)
	   ((get (type-tag file) 'get-salary) file name))))
;; tanuki-file用のレコード取得処理
(define (tanuki-file-package)
  ;; public
  (define (get-name file name)
	(let ((record (find-record (get-records file) name)))
	  (if (null? record) '()
		  (car record))))
  (define (get-address file name)
	(let ((record (find-record (get-records file) name)))
	  (if (null? record) '()
		  (cadr (cadr record)))))
  (define (get-salary file name)
	(let ((record (find-record (get-records file) name)))
	  (if (null? record) '()
		  (cadr (caddr record)))))
  ;; private
  (define (get-records file)
	(cadr file))
  (define (find-record records name)
	(cond ((null? records) '())
		  ((eq? (caar records) name) (car records))
		  (else (find-record (cdr records) name))))
  ;; インターフェース
  (put 'tanuki 'get-address get-address)
  (put 'tanuki 'get-salary get-salary)
  (put 'tanuki 'get-name get-name)
  'done)

;; b
(define (get-name-from-record record)
  (car record))
(define (get-address-from-record record)
  (cadr record))
(define (get-salary-from-record record)
  (caddr record))
(define (get-salary file name)
  (get-salary-from-record
   (get-record file name)))

;; c
;; 他の事業所のファイルの定義例
(define kitsune-file
  (list 'kitsune
		(list
		 (list 'konta
			   (list 'salary 2000)
			   (list 'address "Umi"))
		 (list 'kitsuneta
			   (list 'salary 3000)
			   (list 'address "Yama")))))

(define (kitsune-file-package)
  ;; public
  (define (get-name file name)
	(let ((record (find-record (get-records file) name)))
	  (if (null? record) '()
		  (car record))))
  (define (get-address file name)
	(let ((record (find-record (get-records file) name)))
	  (if (null? record) '()
		  (cadr (caddr record)))))
  (define (get-salary file name)
	(let ((record (find-record (get-records file) name)))
	  (if (null? record) '()
  		  (cadr (cadr record)))))
  ;; private
  (define (get-records file)
	(cadr file))
  (define (find-record records name)
	(cond ((null? records) '())
		  ((eq? (caar records) name) (car records))
		  (else (find-record (cdr records) name))))
  ;; インターフェース
  (put 'kitsune 'get-address get-address)
  (put 'kitsune 'get-salary get-salary)
  (put 'kitsune 'get-name get-name)
  'done)

;; 複数のファイルから従業員を探す
(define (get-employee-record file-list name)
  (define (iter file)
	(if (null? file)
		'()
		(let ((result (get-record (car file) name)))
		  (if (null? result)
			  (iter (cdr file))
			  result))))
  (iter file-list))

;; d
;; 上記のget-name, get-address, get-salaryインターフェースを実装する
