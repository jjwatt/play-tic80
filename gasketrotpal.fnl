;; title:   gasketrotfennel
;; author:  game developer, email, etc.
;; desc:    short description
;; site:    website link
;; license: MIT License (change this to your license of choice)
;; version: 0.1
;; script:  fennel
;; strict:  true
(global WIDTH 240)
(global HEIGHT 136)
(global NUMPOINTS 20000)
(var t 0)
(var points {})
(fn pal [c0 c1]
  (if (and (= nil c0)
           (= nil c1))
      (for [i 0 15 1]
        (poke4 (+ i
                  (* 0x3FF0 2)) i))
      (poke4 (+ c0
                (* 0x3FF0 2)) c1)))
(fn generate-point [lastx lasty vertices]
  "Generate a new point halfway between current point and random vertex."
  (let [j (math.random 1 3)
        x (/ (+ lastx (. vertices j 1)) 2)
        y (/ (+ lasty (. vertices j 2)) 2)]
    (values x y)))
(fn generate-points [x y vertices count points]
  "Recursively generate points for Sierpinski triangle."
  ;; stopping condition
  (if (= 0 count)
      points
      ;; else
      (let [(new-x new-y) (generate-point x y vertices)]
        (tset points count [new-x new-y])
        (generate-points new-x new-y vertices (- count 1) points))))
;; (fn _G.BDR [y]
;;   (var rem (% t 10))
;;   (if (= 5 rem)
;;       (pal 1 (math.random 1 15)))
;;   (set t (+ t 1)))
;; (fn _G.BDR [y]
;;   (pal 1 (+ 1 (+ 128 (* 120 (math.sin (/ (+ y (/ t 32)) 16)))))))
;; (fn _G.BDR [y]
;;   (let [speed 10
;;         y-offset (/ y HEIGHT)  ; normalize y position
;;         wave-phase (/ (+ (* y-offset math.pi) (/ t speed)) 8)]
;;     (pal 1 (+ 1 (math.floor 
;;                   (+ 8 
;;                      (* 7 (math.sin wave-phase))))))))
(fn _G.BDR [y]
  (let [speed 16        ; higher = slower (try 64, 128, 256)
        wave-height 16   ; higher = more spread out wave (try 16, 32, 48)
        color-range 15    ; how many colors to cycle through
        base-color 8    ; center color to oscillate around
        num (+ 1 (math.floor
                  (+ base-color
                     (* color-range
                        (math.sin (/ (+ y (/ t speed))
                                     wave-height))))))]
    (if (= 0 num)
        (pal)
        (pal 1 num))))
(fn _G.BOOT []
  (let [vertices [[0 0]
	          [(/ WIDTH 2) HEIGHT]
		  [WIDTH 0]]
	firstx 50.0
        firsty 50.0]
    (set points (generate-points firstx firsty vertices NUMPOINTS points))))
(fn transform-point [x y]
  "Transform point coordinates to screen space."
  (values (+ (- x) WIDTH) 
          (+ (- y) HEIGHT)))
(fn render-points [points]
  "Recursively render all points."
  (fn render-loop [points count]
    (when (> count 0)
      (let [point (. points count)
            (x y) (transform-point (. point 1) (. point 2))]
        (pix x y 1)
        (render-loop points (- count 1)))))
  (let [count (length points)]
    (render-loop points count)))
(fn rotate-point [x y cx cy angle]
  "Rotate point by angle in radians."
  ;; Translate point to origin.
  (let [translated-x (- x cx)
        translated-y (- y cy)
        ;; Rotate.
        rotated-x (- (* translated-x (math.cos angle))
                     (* translated-y (math.sin angle)))
        rotated-y (+ (* translated-x (math.sin angle))
                     (* translated-y (math.cos angle)))
        ;; Translate point back.
        new-x (+ rotated-x cx)
        new-y (+ rotated-y cy)]
    (values new-x new-y)))
(fn rotate-points [points cx cy angle]
  "Rotate a table of points."
  (fn rotate-helper [points count]
    (if (= count 0)
        points
        (let [point (. points count)]
          (if point
              (let [(new-x new-y) (rotate-point (. point 1) (. point 2) cx cy angle)]
                (tset points count [new-x new-y])
                (rotate-helper points (- count 1)))
              points))))
  (let [count (length points)]
    (rotate-helper points count)))
(fn _G.TIC []
  (cls 0)
  (let [cx (/ WIDTH 2.0)
        cy (/ HEIGHT 2.0)
        speed (math.rad 0.5)]
    (set points (rotate-points points cx cy speed))
    (render-points points))
  (set t (+ t 1)))

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

