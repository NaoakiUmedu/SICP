;; R2.54
(define (my-equal? l r)
  (cond ((and (null? l) (null? r)) #t)
		((and (pair? (car l)) (pair? (car r))) (and (my-equal? (car l) (car r))
												   (my-equal? (cdr l) (cdr r))))
		(else (and (eq? (car l) (car r))
				   (my-equal? (cdr l) (cdr r))))))

;; R2.55
;; ''abracadabra => (quote 'abracadabra) => 'abracadabra なので quoteが返ってきた
