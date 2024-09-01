;;; 2.5.1
;; 一般「演算」手続き
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (apply proc (map contents args))
		  (error
		   ("No method for these types -- APPLY-GENERIC"
			(list op type-tags)))))))
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
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
	  (car datum)
	  (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
	  (cdr datum)
	  (error "Bad tagged datum -- TYPE-TAG" datum)))

;; 2.5.1
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? l r) (apply-generic 'equ? l r))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))

(define (install-scheme-number-package)
  (define (tag x)
	(attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
	   (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
	   (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
	   (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
	   (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
	   (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
	   (lambda (l r) (= l r)))
  (put '=zero? '(scheme-number)
	   (lambda (x) (= x 0)))
  (put 'raise '(scheme-number)
	   (lambda (x) (make-rational x 1)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; 有理数演算パッケージ
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
	(let ((g (gcd n d)))
	  (cons (/ n g) (/ d g))))
  (define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
				 (* (numer y) (denom x)))
			  (* (denom x) (denom y))))
  (define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
				 (* (numer y) (denom x)))
			  (* (denom x) (denom y))))
  (define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
			  (* (denom x) (denom y))))
  (define (div-rat x y)
	(make-rat (* (numer x) (denom y))
			  (* (denom x) (numer y))))
  (define (equ?-rat x y)
	;; make-ratにて約分ずみ
	(and (= (numer x) (numer y))
		 (= (denom x) (denom y))))
  (define (=zero?-rat x)
	(= (numer x) 0))
  ;; システムの他の部分へのインターフェース
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
	   (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
	   (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
	   (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
	   (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
	   (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
	   (lambda (l r) (equ?-rat l r)))
  (put '=zero? '(rational)
	   (lambda (x) (=zero?-rat x)))
  (put 'raise '(rational)
	   (lambda (x) (make-complex-from-real-imag (/ (numer x) (denom x))
												0)))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; 複素数パッケージ
(define (install-complex-package)
  ;; makeは直交座標と極座標系から取り入れた手続き
  (define (make-from-real-imag x y)
	((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
	((get 'make-from-mag-ang 'polar) r a))

  ;; 内部手続き
  (define (add-complex z1 z2)
	(make-from-real-imag (+ (real-part z1) (real-part z2))
						 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
	(make-from-real-imag (- (real-part z1) (real-part z2))
						 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
	(make-from-mag-ang (* (magnitude z1) (magnitude z2))
					   (+ (angle z1) (angle z2))))
  (define (sub-complex z1 z2)
	(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
					   (- (angle z1) (angle z2))))
  (define (equ?-complex r l)
	(and (= (real-part l) (real-part r))
		 (= (imag-part l) (imag-part r))))
  (define (=zero?-complex x)
	(and (= (real-part x) 0) (= (imag-part x) 0)))
  ;; システムの他の部分へのインターフェース
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
	   (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
	   (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
	   (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
	   (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
	   (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
	   (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
	   (lambda (l r) (equ?-complex l r)))
  (put '=zero? '(complex)
	   (lambda (x) (=zero?-complex x)))
  'done)

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


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; R2.77
;; complexパッケージの下に具体的な複素数パッケージがぶら下がっているが、
;; real-part等がcomplexパッケージに存在しないため、その下の複素数パッケージに定義されているreal-partなどを引き当てられない

;; R2.78
;; attach-tagでscheme-numberのときは、contentsをそのまま返すようにする
(define (attach-tag tag-type contents)
  (if (eq? tag-type 'scheme-number)
	  contents
	  (cons tag-type contents)))
;;
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number) ;; scheme-numberのタグはここでつける(make-scheme-numberしなくてもよいようにするため)
		((pair? datum) (car datum))
		(else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond
   ((number? datum) datum)
   ((pair? datum) (cdr datum))
   (else (error "Bad tagged datum -- TYPE-TAG" datum))))

;; R2.79
;; これでよくない？？？
;; (define (equ? lhs rhs)
;;   (if (eq? (type-tag lhs) (type-tag rhs))
;; 	  ;; tagが同じならequal?(tagが同じだと意味が違うかもしれないため)
;; 	  (equal? lhs rhs)
;; 	  ;; ちがうなら違う
;; 	  #f))

;; 模範解答は上に書いた

;; R2.80
;; 上に書いた

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-rectangle-package)
(install-polar-package)

;;2.5.2

;; 複素数+整数をパッケージに組み込む...がどっちのほうに入れる???
;; (define (add-complex-to-schemenum z x)
;;   (make-from-real-imag (+ (real-part z) x)
;; 					   (imag-part z)))
;; (put 'add '(complex-scheme-number)
;; 	 (lambda (z x) (tag (add-complex-to-schemenum z x))))

;; 強制型変換テーブルの定義
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

;;強制型変換
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  ;; 該当する組み合わせのprocがあればそれをする
		  (apply proc (map contents args))
		  (if (= (length args) 2)
			  ;; 強制型変換を試みる
			  (let ((type1 (car type-tags))
					(type2 (cadr type-tags))
					(a1 (car args))
					(a2 (cadr args)))
				(let ((t1->t2 (get-coercion type1 type2))
					  (t2->t1 (get-coercion type2 type1)))
				  (cond (t1->t2
						 (apply-generic op (t1->t2 a1) a2))
						(t2->t1
						 (apply-generic op a1 (t2->t1 a2)))
						(else
						 (error "No method for these types"
								(list op type-tags))))))
			  ;; 一般化は2.82でする
			  (error "No method for these types"
					 (list op type-tags)))))))

;; R2.81
;; a 無限ループする(第一引数を第二引数の型へ、第二引数を第一引数の型へ変換し、変換後の型でapply-genericをよぶところ)
;; b このままで動く
;; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  ;; 該当する組み合わせのprocがあればそれをする
		  (apply proc (map contents args))
		  (if (and (= (length args) 2)
				   ;; タグが同じなら強制型変換しない
				   (not (equal? (car type-tags) (cadr type-tags))))
			  ;; 強制型変換を試みる
			  (let ((type1 (car type-tags))
					(type2 (cadr type-tags))
					(a1 (car args))
					(a2 (cadr args)))
				(let ((t1->t2 (get-coercion type1 type2))
					  (t2->t1 (get-coercion type2 type1)))
				  (cond (t1->t2
						 (apply-generic op (t1->t2 a1) a2))
						(t2->t1
						 (apply-generic op a1 (t2->t1 a2)))
						(else
						 (error "No method for these types"
								(list op type-tags))))))
			  ;; 一般化は2.82でする
			  (error "No method for these types"
					 (list op type-tags)))))))

;; R2.82
(define (apply-generic op . args)
  ;; 強制型変換を試みる
  (define (try-coercion types)
	(define (convert type args)
	  (if (null? type) '()
		  (cons ((get-coercion (car type) (car types)) (car args))
				(convert (cdr type) (cdr args)))))
	(if (null? types)
		(error "No method for these types"
			   (list op type-tags))
		(eval (cons apply-generic (cons op (convert type-tags args))))));; 変換後の型でapply-genericをもっかい試す
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (apply proc (map contents args));; 処理があればそれをする
		  (try-coercion type-tags)))));; なければ強制型変換

;; R2.83
;; 上に書いた

;; R2.84
(define (raise-to-top-or-other self other)
  (let ((self-type (type-tag self))
		(other-type (type-tag other)))
	(if (eq? self-type other-type)
		self
		(if (get raise self-type)
			(try-coercion (raise self) other)
			self))))
(define (is-heigher l r)
  (let ((raised-l (raise-to-top-or-other l r)))
	(if (= (type-tag raised-l) (type-tag r))
		#f;; rに負ける雑魚
		#t)))

(define (apply-generic op . args)
  ;; 強制型変換を試みる
  (define (try-coercion types)
	(define (convert type args)
	  (if (null? type) '()
		  (cons ((get-coercion (car type) (car types)) (car args))
				(convert (cdr type) (cdr args)))))
	(if (null? types)
		(error "No method for these types"
			   (list op type-tags))
		(eval (cons apply-generic (cons op (convert type-tags args))))));; 変換後の型でapply-genericをもっかい試す
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (apply proc (map contents args));; 処理があればそれをする
		  (try-coercion type-tags)))));; なければ強制型変換
