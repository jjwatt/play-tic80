;; title:   sketch
;; author:  jwatt@broken.watch
;; desc:    trash
;; site:    website link
;; license: GPL3
;; version: 0.1
;; script:  fennel
;; strict:  true
(var myt 0)
(local center-x 120)
(local center-y 75)
(local WIDTH 240)
(local HEIGHT 136)
(local SCREEN_SIZE (* WIDTH HEIGHT))

(fn custom-random []
  (- 1 (math.pow (math.random) 5)))

;; normalize
(fn norm [value low high]
  "Normalize value to be between 0.0 and 1.0."
  (/ (- value low) (- high low)))

;; linear interpolation
(fn lerp [low high amt]
  "Linear interpolation. `amt` should be between 0.0 and 1.0."
  (+ low (* amt (- high low))))

(fn mapvalue [value low1 high1 low2 high2]
  "Map from one range of values to another."
  (let [normalized (norm value low1 high1)
        remapped (lerp low2 high2 normalized)]
    remapped))

(fn my-eight-ten [width height]
  "Draw circles along an arc."
  (for [x 1 width 5]
    (let [n (norm x 0 width)
          y (^ n 2)
          yscale (lerp 20 HEIGHT y)]
      (circb x yscale (/ width 10) 5))))
(fn my-eight-eleven [width ?pow ?color]
  "Draw lines down to a curve."
  (for [x 1 width 5]
    (let [n (mapvalue x 1 width -1 1)
          p (^ n (or ?pow 2))
          y (lerp 20 HEIGHT p)]
      (line x 0 x y (or ?color 2)))))
(fn my-eight-twelve [width height]
  "Draw gradiants."
  (for [x 0 width 1]
    (let [n (norm x 0.0 width)
          val (* n 15)
          valsquare (* (^ n 2) 15)]
      (line x 0 x (/ height 2) val)
      (line x (/ height 2) x HEIGHT valsquare))))

(fn _G.TIC []
  (cls 8)
  ;; (my-eight-eleven WIDTH 2 6)
  ;; (my-eight-twelve WIDTH HEIGHT)
  (my-eight-ten WIDTH HEIGHT)
  )

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

