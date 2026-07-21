;; title:   game title
;; author:  game developer, email, etc.
;; desc:    short description
;; site:    website link
;; license: MIT License (change this to your license of choice)
;; version: 0.1
;; script:  scheme

(define-macro (inc! x dx) `(set! ,x (+ ,x ,dx)))

(define WIDTH 240)
(define HEIGHT 136)
(define NUMPOINTS 20000)
(define t 0)
(define points '())

(define (pal c0 c1)
  "Palette swap helper."
  (if (and (not c0) (not c1))
      (do ((i 0 (+ i 1)))
	  ((> i 15))
	(t80::poke4 (+ i (* #x3FF0 2)) i))
      (t80::poke4 (+ c0 (* #x3FF0 2)) c1)))

(define (generate-points count)
  "Iteratively generate points for the Sierpinski triangle."
  (let ((v0 (cons 0.0 0.0))
	(v1 (cons (/ WIDTH 2.0) (exact->inexact HEIGHT)))
	(v2 (cons (exact->inexact WIDTH) 0.0)))
    (let loop ((i 0)
	       (curr-x 50.0)
	       (curr-y 50.0)
	       (acc '()))
      (if (<= count i)
	  acc
	  (let* ((r (random 3))
		 (v (cond ((= r 0) v0)
			  ((= r 1) v1)
			  (else v2)))
		 (next-x (/ (+ curr-x (car v)) 2.0))
		 (next-y (/ (+ curr-y (cdr v)) 2.0)))
	    (loop (+ i 1)
		  next-x
		  next-y
		  (cons (cons next-x next-y) acc)))))))

(define (deg->rad degrees)
  (* degrees (/ pi 180)))

(define (BDR y)
  "Raster interrupt for rotating palette, skip black."
  (let* ((scroll-speed 0.5)
	 (line-index (+ y (* t scroll-speed)))
	 (total-lines 64.0)
	 (phase (/ (modulo line-index total-lines) total-lines))
	 (num (floor (+ 2 (* 13 phase)))))
    (pal 1 num)))

(define (BOOT)
  (set! points (generate-points NUMPOINTS)))

(define (TIC)
  (t80::cls 0)
  (let* ((cx (/ WIDTH 2.0))
	 (cy (/ HEIGHT 2.0))
	 (angle (deg->rad 0.5))
	 (cos-a (cos angle))
	 (sin-a (sin angle)))
    (set! points
	  (map (lambda (p)
		 (let* ((tx (- (car p) cx))
			(ty (- (cdr p) cy))
			(rx (+ (- (* tx cos-a) (* ty sin-a)) cx))
			(ry (+ (+ (* tx sin-a) (* ty cos-a)) cy))
			(sx (round (+ (- rx) WIDTH)))
			(sy (round (+ (- ry) HEIGHT))))
		   (t80::pix sx sy 1)
		   (cons rx ry)))
	       points)))
  (inc! t 1))

;; <TILES>
;; 001:eccccccccc888888caaaaaaaca888888cacccccccacc0ccccacc0ccccacc0ccc
;; 002:ccccceee8888cceeaaaa0cee888a0ceeccca0ccc0cca0c0c0cca0c0c0cca0c0c
;; 003:eccccccccc888888caaaaaaaca888888cacccccccacccccccacc0ccccacc0ccc
;; 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
;; 017:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
;; 018:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
;; 019:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
;; 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
;; </TILES>

;; <WAVES>
;; 000:00000000ffffffff00000000ffffffff
;; 001:0123456789abcdeffedcba9876543210
;; 002:0123456789abcdef0123456789abcdef
;; </WAVES>

;; <SFX>
;; 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
;; </SFX>

;; <TRACKS>
;; 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </TRACKS>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

