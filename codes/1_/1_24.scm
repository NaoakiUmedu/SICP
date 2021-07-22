;; 線形再帰 ステップはO(n)で、スペースもO(n)
;; (define (expt b n)
;;   (if (= n 0)
;;       1
;;       (* b (expt b (- n 1)))))
;; 反復 ステップはO(n)だが、スペースはO(1)
;; (覚えておくのはb, n, 今のcounter, 今のproductの4つだけでよい。
;; ただし、Cとかでは反復的でも手続き呼び出しの数と共に記憶量の消費が増加するような実装になっている
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* b product))))

;; 逐次平方 O(logn)
;; b^2nを作るのに、b^nを作る回数+1ステップしか消費しない
;; そのため、プロセスはnに対数的に増加する(作れる数b^nはステップに対して指数的に増加する)
(define (fast-expt b n)
  (cond ((= n 0 1))
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))
