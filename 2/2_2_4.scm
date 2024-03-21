;; m-x racket-repl
;;(load "**/codes/SICP/SICP/2/2_2_4.scm")
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
	(below painter2 painter2)))

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
(define (split positioner make-smaller)
  (lambda (painter n)
	(if (= n 0)
		painter
		(let ((smaller ((split positioner make-smaller) painter (- n 1))))
		  (positioner painter (make-smaller smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))

;; フレーム
(define (frame-coord-map frame)
  (lambda (v)
	(add-vect
	 (origin-frame frame)
	 (add-vect (scale-vect (xcor-vect v)
						   (edge1-frame frame))
			   (scale-vect (ycor-vect v)
						   (edge2-frame frame))))))
;; ;; R2.46
;; (define (make-vect x y)
;;   (list x y))
;; (define (xcor-vect v)
;;   (car v))
;; (define (ycor-vect v)
;;   (cadr v))
;; (define (add-vect v1 v2)
;;   (make-vect (+ (xcor-vect v1) (xcor-vect v2))
;; 			 (+ (ycor-vect v1) (ycor-vect v2))))
;; (define (sub-vect v1 v2)
;;   (make-vect (- (xcor-vect v1) (xcor-vect v2))
;; 			 (- (ycor-vect v1) (ycor-vect v2))))
;; (define (scale-vect v s)
;;   (make-vect (* (xcor-vect v) s)
;; 			 (* (ycor-vect v) s)))

;; ;; R2.47
;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))
;; (define (origin-frame f)
;;   (car f))
;; (define (edge1-frame f)
;;   (cadr f))
;; (define (edge2-frame f)
;;   (caddr f))

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))
;; (define (origin-frame f)
;;   (car f))
;; (define (edge1-frame f)
;;   (cadr f))
;; (define (edge2-frame f)
;;   (cddr f))

;; ペインタ
;; segments->painterは組み込みのものを使う
;; (define (segments->painter segment-list)
;;   (lambda (frame)
;; 	(for-each
;; 	 (lambda (segment)
;; 	   (draw-line
;; 		((frame-coord-map frame) (start-segment segment))
;; 		((frame-coord-map frame) (end-segment segment))))))
;;   segment-list)
;; R2.49
;; (define (make-segment start end)
;;   (list start end))
;; (define (start-segment s)
;;   (car s))
;; (define (end-segment s)
;;   (cadr s))
;; R2.50
(define outline-frame
  (segments->painter
   (list
	(make-segment (make-vect 0 0) (make-vect 0 1))
	(make-segment (make-vect 0 1) (make-vect 1 1))
	(make-segment (make-vect 1 1) (make-vect 1 0))
	(make-segment (make-vect 1 0) (make-vect 0 0)))))
;; 1だと点滅してしまう...
(define outline-frame
  (segments->painter
   (list
	(make-segment (make-vect 0 0) (make-vect 0 0.9))
	(make-segment (make-vect 0 0.9) (make-vect 0.9 0.9))
	(make-segment (make-vect 0.9 0.9) (make-vect 0.9 0))
	(make-segment (make-vect 0.9 0) (make-vect 0 0)))))

(define x-frame
  (segments->painter
   (list
	(make-segment (make-vect 0 0) (make-vect 1 1))
	(make-segment (make-vect 0 1) (make-vect 1 0)))))

(define center 0.5)
(define rhombus-frame
  (segments->painter
   (list
	(make-segment (make-vect center 0) (make-vect 1 center))
	(make-segment (make-vect 1 center) (make-vect center 1))
	(make-segment (make-vect center 1) (make-vect 0 center))
	(make-segment (make-vect 0 center) (make-vect center 0)))))


(define w1 (make-vect 0.00 0.85))
(define w2 (make-vect 0.15 0.62))
(define w3 (make-vect 0.30 0.70))
(define w4 (make-vect 0.42 0.70))
(define w5 (make-vect 0.38 0.88))
(define w6 (make-vect 0.40 1.00))
(define w7 (make-vect 0.62 1.00))
(define w8 (make-vect 0.65 0.88))
(define w9 (make-vect 0.60 0.70))
(define w10 (make-vect 0.75 0.70))
(define w11 (make-vect 1.00 0.38))
(define w12 (make-vect 1.00 0.15))
(define w13 (make-vect 0.64 0.48))
(define w14 (make-vect 0.78 0.00))
(define w15 (make-vect 0.62 0.00))
(define w16 (make-vect 0.52 0.30))
(define w17 (make-vect 0.40 0.00))
(define w18 (make-vect 0.25 0.00))
(define w19 (make-vect 0.36 0.52))
(define w20 (make-vect 0.30 0.64))
(define w21 (make-vect 0.15 0.43))
(define w22 (make-vect 0.00 0.67))
(define wave
  (segments->painter
   (list (make-segment w1 w2)
         (make-segment w2 w3)
         (make-segment w3 w4)
         (make-segment w4 w5)
         (make-segment w5 w6)
         (make-segment w7 w8)
         (make-segment w8 w9)
         (make-segment w9 w10)
         (make-segment w10 w11)
         (make-segment w12 w13)
         (make-segment w13 w14)
         (make-segment w15 w16)
         (make-segment w16 w17)
         (make-segment w18 w19)
         (make-segment w19 w20)
         (make-segment w20 w21)
         (make-segment w21 w22))))
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))
(define wave4 (flipped-pairs wave))
