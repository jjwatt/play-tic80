;; title:   Granular Pixel Shatter
;; author:  jwatt@broken.watch
;; site:    jjwatt/play-tic80
;; license: GPL3
;; version: 0.1
;; script:  fennel
;; strict:  true

(local WIDTH 240)
(local HEIGHT 136)

(local text "GREETS TPOT")
(local start-x (- WIDTH 180))
(local start-y (/ HEIGHT 2))

(var particles [])
(var shattered? false)

(fn init-particles []
  "Draws text offscreen once, scans it and caches active pixel coordinates."
  (cls 0)
  (print text start-x start-y 14 false 2)

  (set particles [])

  ;; Scan the text bounding box
  (for [y start-y (+ start-y 16)]
    (for [x start-x (+ start-x 120)]
      (let [color (pix x y)]
        (when (< 0 color)
          ;; Center of blast calculations
          (let [cx (+ start-x 45)
                cy (+ start-y 8)
                ;; Direction vector away from center
                dx (- x cx)
                dy (- y cy)
                dist (math.max 1 (math.sqrt (+ (* dx dx) (* dy dy))))]
            (table.insert particles
                          {: x : y
                           :orig-color color
                           :vx (+ (/ dx dist) (* (- (math.random) 0.5) 1.5))
                           :vy (+ (/ dy dist) (* (- (math.random) 0.5) 1.5))})))))))

(fn _G.BOOT []
  (init-particles))

(fn _G.TIC []
  (cls 0)

  (if (btn 4) (set shattered? true))
  (if (not shattered?)
      (print text start-x start-y 14 false 2)
      (each [_ p (ipairs particles)]
        ;; Apply physics
        (set p.x (+ p.x p.vx))
        (set p.y (+ p.y p.vy))
        ;; Light downward gravity drift
        (set p.vy (+ p.vy 0.05))
        ;; Air resistance
        (set p.vx (* p.vx 0.98))
        ;; Render pixel particle back to screen memory if onscreen
        (if (and (<= 0 p.x) (< p.x WIDTH) (<= 0 p.y) (< p.y HEIGHT))
            (pix p.x p.y p.orig-color))))
  (when (btn 5)
    (set shattered? false)
    (init-particles)))

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

