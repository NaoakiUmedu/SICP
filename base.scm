(define (square n) (* n n))

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

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (string-list->string ls)
  (accumulate (lambda (x y) (string-append x " " y)) "" ls))

(define (number-list->string ls)
  (string-list->string (map number->string ls)))
