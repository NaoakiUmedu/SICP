;; java -jar schuluessel -- now.scmx
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2)))) 
	(else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;(print (fast-expt 3 9))

;; 1.16
(define (fast-expt-rep b n)
  (fast-expt-rep-iter b n 1))

(define (fast-expt-rep-iter b counter product)
  (cond ((= counter 0) product)
	((even? counter) (fast-expt-rep-iter b (/ counter 2) (square product)))
	(else (fast-expt-rep-iter b (- counter 1) (* b product)))))

;(print (fast-expt-rep 3 9))

