;; title:   gasketrotfennel
;; author:  jwatt@broken.watch
;; desc:    Rotating sierpinksi gasket
;; site:    website link
;; license: MIT License (change this to your license of choice)
;; version: 0.1
;; script:  fennel
;; strict:  true
(global WIDTH 240)
(global HEIGHT 136)
(global NUMPOINTS 20000)
(var t 0)
(var points [])

(fn pal [c0 c1]
  "Palette swap helper."
  (if (and (= nil c0)
           (= nil c1))
      (for [i 0 15]
        (poke4 (+ i (* 0x3FF0 2)) i))
      (poke4 (+ c0 (* 0x3FF0 2)) c1)))

(fn generate-points [count]
  "Iteratively generates points for the Sierpinski triangle."
  (let [vertices [{:x 0             :y 0}
                  {:x (/ WIDTH 2)   :y HEIGHT}
                  {:x WIDTH         :y 0}]
        pts []]
    (var current-x 50)
    (var current-y 50)
    (for [_ 1 count]
      (let [v (. vertices (math.random 1 3))
            next-x (/ (+ current-x v.x) 2)
            next-y (/ (+ current-y v.y) 2)]
        (table.insert pts {:x next-x :y next-y})
        (set current-x next-x)
        (set current-y next-y)))
    pts))

(fn _G.BOOT []
  (set points (generate-points NUMPOINTS)))

(fn _G.BDR [y]
  "Raster interrupt for rotating palette, skip black."
  (let [speed 16
        wave-height 8
        input-val (/ (+ y (/ t speed)) wave-height)
        phase (- input-val (math.floor input-val))
        tri-val (if (< phase 0.5)
                    (* phase 2)          ; Rises smoothly from 0 to 1
                    (- 2 (* phase 2)))   ; Falls smoothly from 1 to 0
        num (math.floor (+ 2 (* 13 tri-val)))]
    (pal 1 num)))

(fn _G.TIC []
  (cls 0)
  (let [cx (/ WIDTH 2)
        cy (/ HEIGHT 2)
        angle (math.rad 0.5)
        cos-a (math.cos angle)
        sin-a (math.sin angle)]
    ;; Rotate and render points in a single pass.
    (each [_ p (ipairs points)]
      ;; Translate to origin.
      (let [tx (- p.x cx)
            ty (- p.y cy)
            ;; Rotate and translate back.
            rx (+ (- (* tx cos-a) (* ty sin-a)) cx)
            ry (+ (+ (* tx sin-a) (* ty cos-a)) cy)
            ;; Transform to screen space.
            sx (+ (- rx) WIDTH)
            sy (+ (- ry) HEIGHT)]
        (set p.x rx)
        (set p.y ry)
        (pix sx sy 1))))
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

