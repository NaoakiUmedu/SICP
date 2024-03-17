(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
	(below painter2 painter2)))
(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
	  painter
	  (let ((smaller (right-split painter (- n 1))))
		(beside painter (below smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
	  painter
	  (let ((up (up-split painter (- n 1)))
			(right (right-split painter (- n 1))))
		(let ((top-left (beside up up))
			  (bottom-right (below right right))
			  (corner (corner-split painter (- n 1))))
		  (beside (below painter top-left)
				  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
	(let ((half (beside (flip-horiz quarter) quarter)))
	  (below (flip-vert half) half))))

;; R2.44
(define (up-split painter n)
  (if (= n 0)
	  painter
	  (let ((smaller (up-split painter (- n 1))))
		(below painter (below smaller smaller)))))

;; 高階演算
(define (square-of-four tl tr bl br)
  (lambda (painter)
	(let ((top (beside (tl painter) (tr painter)))
		  (bottom (beside (bl painter) (br painter))))
	  (below bottom top))))
(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
								  identity flip-vert)))
	(combine4 painter)))
(define (identity n) n)
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
								  rotete180 flip-vert)))
	(combine4 (corner-split painter n))))

;; R2.45


(define (split make-smaller positioner)
  (lambda (painter n)
	(if (= n 0)
		painter
		(let ((smaller (make-smaller painter)))
		  (let ((right (positioner smaller smaller)))
		  (beside painter right))))))
(define right-split (split beside below))
(define up-split (split below beside))
(define (split make-smaller positioner)
  (lambda (painter n)
	(if (= n 0)
		painter
		(let ((splitted ((split make-smaller positioner) painter n)))
		  (let ((smaller (make-smaller splitted splitted)))
			(let ((right (positioner smaller smaller)))
			  (beside painter right)))))))
