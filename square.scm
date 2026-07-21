;; title:   game title
;; author:  game developer, email, etc.
;; desc:    short description
;; site:    website link
;; license: MIT License (change this to your license of choice)
;; version: 0.1
;; script:  scheme

(define WIDTH 240)
(define HEIGHT 136)
(define t 0)

(define (deg->rad degrees)
  (* degrees (/ pi 180)))

(define (TIC)
  (t80::cls 0)
  (let* ((cx (/ WIDTH 2))
	 (cy (/ HEIGHT 2))
	 (size 30)
	 (angle (* t (deg->rad 1.5)))
	 (cos-a (cos angle))
	 (sin-a (sin angle))
	 (pts '((-30 . -30)
		(30 . -30)
		(30 . 30)
		(-30 . 30))))
    (let* ((rotated
	    (map (lambda (p)
		   (let ((x (car p))
			 (y (cdr p)))
		     (cons (round (+ (- (* x cos-a) (* y sin-a)) cx))
			   (round (+ (+ (* x sin-a) (* y cos-a)) cy)))))
		 pts))
	   (p1 (list-ref rotated 0))
	   (p2 (list-ref rotated 1))
	   (p3 (list-ref rotated 2))
	   (p4 (list-ref rotated 3)))

      (t80::line (car p1) (cdr p1) (car p2) (cdr p2) 12)
      (t80::line (car p2) (cdr p2) (car p3) (cdr p3) 12)
      (t80::line (car p3) (cdr p3) (car p4) (cdr p4) 12)
      (t80::line (car p4) (cdr p4) (car p1) (cdr p1) 12)))
  (set! t (+ t 1)))

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

