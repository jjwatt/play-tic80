;; title:   spn3fen
;; author:  jwatt@broken.watch
;; desc:    Spiral noise 3 with fennel
;; site:    jjwatt/play-tic80
;; license: MIT
;; version: 0.1
;; script:  fennel
;; strict:  true
(var myt 0)
(local radius 20)
(local center-x 120)
(local center-y 75)
(local num-points 150)
(local WIDTH 240)
(local HEIGHT 136)
;; WIDTH * HEIGHT / 2 bytes (4-bpp)
(local SCREEN_SIZE (/ (* WIDTH HEIGHT) 2))
(local VRAM_ADDR 0x0000)
(local OFFSCREEN_ADDR 0x8000) ;; Start of free RAM

(fn norm [value low high]
  "Normalize value to between 0.0 and 1.0"
  (/ (- value low) (- high low)))
(fn lerp [low high amt]
  "Linear interpolation of amt (normalized) to low-high"
  (+ low (* amt (- high low))))
(fn mapvalue [value low1 high1 low2 high2]
  "Map from one set of values to the other"
  (let [n (norm value low1 high1)]
    (lerp low2 high2 n)))

;; For double-buffering
(fn save-buffer []
  "Save offscreen buffer to vram."
  (memcpy OFFSCREEN_ADDR VRAM_ADDR SCREEN_SIZE))

(fn restore-buffer []
  "Restore vram to offscreen buffer."
  (memcpy VRAM_ADDR OFFSCREEN_ADDR SCREEN_SIZE))

(fn custom-random []
  (- 1 (math.pow (math.random) 5)))

(var last-time (time))
(var current-fps 60)
(fn draw-fps []
  (let [now (time)
        delta (- now last-time)]
    (when (> delta 0)
      (let [raw-fps (/ 1000 delta)]
        (set current-fps (lerp current-fps raw-fps 0.05))))
    (set last-time now)
    (print (string.format "FPS: %.1f" current-fps) 0 0 14 true)))

(fn my-noise-spiral [centerx centery radius color]
  (let [startradius (/ radius 10)
        radius-noise (math.random startradius)
        first-radius (+ startradius (* radius-noise (- 1 (custom-random))))
        spiral {:startradius (+ startradius 0.2 (- 1 (custom-random)))
                :lastx (+ centerx first-radius) ;; cos(0) = 1
                :lasty centery}]                ;; sin(0) = 0
    (var current-noise radius-noise)
    (for [angle 10 (* 360 4) 10]
      (set current-noise (+ current-noise 0.08))
      (let [thisradius (+ spiral.startradius
                          (* current-noise (- 1 (custom-random))))
            radians (math.rad angle)
            x (+ centerx (* thisradius (math.cos radians)))
            y (+ centery (* thisradius (math.sin radians)))]
        (line x y spiral.lastx spiral.lasty color)
        (set spiral.startradius (+ spiral.startradius 0.2 (- 1 (custom-random))))
        (set spiral.lastx x)
        (set spiral.lasty y)))))

(fn _G.TIC []
  (restore-buffer)
  (when (= 0 (% myt 4))
    (cls 1)
    (my-noise-spiral center-x center-y radius 4))
  (save-buffer)
  (draw-fps)
  (set myt (+ myt 1)))


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

