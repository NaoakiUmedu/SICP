;; 2.1.1
;; 有理数の演算の定義
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
; a/b == c/d <=> ad == bd
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
;; 有理数の定義
;(define (make-rat n d) (cons n d))
(define (make-rat n d)
  (define (gcd a b)
    (if (< a b)
        (gcd b a)
        (let ((r (remainder a b)))
              (if (= r 0)
              b
              (gcd b r)))))
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))
;; ログ
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; 動作確認
;;(define one-half (make-rat 1 2))
;; (print-rat one-half)
;;(define one-third (make-rat 1 3))
;; (print-rat (add-rat one-half one-third))
;; (print-rat (sub-rat one-half one-third))
;; (print-rat (mul-rat one-half one-third))
;; (print-rat (div-rat one-half one-third))
;; (display (equal-rat? one-half one-third))
;; (display (equal-rat? one-half one-half))
;(print-rat (add-rat one-third one-third))

;; R 2.1
(define (make-rat n d)
  (define (gcd a b)
    (if (<= (abs a) (abs b))
        (gcd b a)
        (let ((r (remainder a b)))
              (if (= r 0)
              b
              (gcd b r)))))
  ;; 既約分数にする
  (let ((g (gcd n d)))
    (let ((n-g (/ n g))
          (d-g (/ d g)))
          (if (> d-g 0)
              (cons n-g d-g)
              (cons (* -1 n-g) (* -1 d-g))))))

(define (make-rat n d)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (let ((g (abs (gcd n d))))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

;; 2.1.2
(define (make-rat n d) (cons n d))
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;;(print-rat (make-rat  1 -2))
;;(print-rat (make-rat -1  2))
;;(print-rat (make-rat  1  2))
;;(print-rat (make-rat -1 -2))

;; R2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
(define (midpoint-segment s)
  (define (average a b) (/ (+ a b) 2))
  (let ((mid-x (average (x-point (start-segment s)) (x-point (end-segment s))))
        (mid-y (average (y-point (start-segment s)) (y-point (end-segment s)))))
        (make-point mid-x mid-y)))
(define (print-segment s)
  (newline)
  (display "---print-segment start---")
  (display (print-point (start-segment s)))
  (display (print-point (end-segment s)))
  (newline)
  (display "---print-segment end---"))

;;(define a (make-point 12 -3))
;;(define b (make-point -27 6))
;;(define sg (make-segment a b))
;;(print-segment sg)
;;(print-point (midpoint-segment sg))

;; R2.3
;; 必用なもろもろ
(define (square x) (* x x))
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
  (< (abs (- n (square guess))) tolerance))
  (define (improve-sqrt guess)
  (define (average a b) (/ (+ a b) 2))
  (average guess (/ n guess)))
  ((iterative-improve good-sqrt improve-sqrt) 1.0))
(define (get-length sg)
  (sqrt (+  (square (- (x-point (end-segment sg)) (x-point (start-segment sg))))
            (square (- (y-point (end-segment sg)) (y-point (start-segment sg)))))))
;; 90度に交わる線分を2つとるアプローチ
;(define (make-rectangle sga sgb)
;  (cons (get-length sga) (get-length sgb)))
(define (get-virtical sg) (car sg))
(define (get-longitudinal sg) (cdr sg))
;; 4点をとるアプローチ
;;(define (make-rectangle a b c d)
;;  (cons (get-length (make-segment a b)) (get-length (make-segment c d))))
;; x軸,y軸に平行な長方形を対角で相対する頂点 corner0 cornet1で表現する
(define (make-rectangle corner0 corner1)
  (let ((longitudinal (abs (- (x-point corner0) (x-point corner1))))
        (virtical     (abs (- (y-point corner0) (y-point corner1)))))
        (cons longitudinal virtical)))

;; 外周
(define (peremeter rect)
  (+  (* 2 (get-virtical rect))
      (* 2 (get-longitudinal rect))))
;; 面積
(define (area rect)
  (* (get-virtical rect) (get-longitudinal rect)))

;;(define origin (make-point 0 0))
;;(define a-point (make-point 0.71 0.71))
;;(define b-point (make-point 0.71 -0.71))
;;(define c-point (make-point 1.4 0))
;;(define sga (make-segment origin a-point))
;;(define sgb (make-segment origin b-point))
;;;;(define rct (make-rectangle sga sgb))
;;;;(define rct (make-rectangle origin a-point b-point c-point))
;;(define rct (make-rectangle (make-point 1 1) (make-point 2 3)))
;;(display (peremeter rct))
;;(newline)
;;(display (area rct))
;;(newline)

;; R2.4
;; consはmの処理にx yを渡す
(define (cons x y)
  (lambda (m) (m x y)))
;; carは(p q)を受け取ってpを返す
(define (car z)
  (z (lambda (p q) p)))
;;(car (cons x y))
;;↓
;;(car (lambda (m) (m x y)))
;;↓
;;((lambda (m) (m x y)) (lambda (p q) p))
;;↓
;;((lambda (p q) p) x y)
;;↓
;;(x)

(define (cdr z)
  (z (lambda (p q) q)))

;; R2.5
(define (2-5-cons  a b) (cons (expt 2 a) (expt 3 b)))
(define (2-5-car p) (log (car p) 2))
(define (2-5-cdr p) (log (cdr p) 3))
(define x (2-5-cons 3 9))
;;(display (2-5-car x))
;;(newline)
;;(display (2-5-cdr x))
;;(newline)

;; R2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
;; 引数が渡されてるlambdaを評価していき、評価できなくなったらおわり
((lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) zero)
(lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) (lambda (f) (lambda (x) x))
(lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) 
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f x)))
(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
;↓add-1の定義を代入
((lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) one)
;↓oneの定義を代入
((lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) (lambda (f) (lambda (x) (f x))))
;↓lambda(f)をxで評価し、いらない()を外す
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;↓lambda(f)をxで評価し、いらない()を外す
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;↓lambda(x)をxで評価し、いらない()を外す
(lambda (f) (lambda (x) (f (f x))))
; もう評価できるものはない
(define two (lambda (f) (lambda (x) (f (f x)))))

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
;; ↑xに対しfを何回実行しますかという定義になっている
(define church+ (lambda m b) (lambda (f) (lambda (x) ((m f) ((n f) x)))))
;; ↑fをm回実行したものと、fをn回実行したものをxに対して実行している
