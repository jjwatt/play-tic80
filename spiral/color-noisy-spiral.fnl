;; title:   spn3fen
;; author:  jwatt@broken.watch
;; desc:    Spiral noise 3 with fennel
;; site:    jjwatt/play-tic80
;; license: GPL3
;; version: 0.7
;; script:  fennel
;; strict:  true
(var myt 0)
(local radius 20)
(local center-x 120)
(local center-y 75)
(local num-points 150)
(local WIDTH 240)
(local HEIGHT 136)

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

;;;; Perlin Noise
(local p {151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99
    37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 150 252 175 211 193 66 54
    194 148 153 141 66 128 143 219 84 188 205 116 68 14 142 217 3 240 69 251 88 14
    221 141 126 155 90 135 142 238 251 202 69 114 83 75 37 175 190 148 154 166 133
    182 73 158 162 190 40 165 162 81 37 95 145 51 28 40 211 191 126 163 254 103 240
    155 200 103 80 63 94 251 189 47 99 139 111 165 225 29 141 75 123 178 160 209
    215 152 148 228 73 34 166 220 103 28 77 208 187 204 181 190 208 135 153 151 154
    224 195 160 94 258 150 261 91 241 180 188 107 176 146 84 204 115 227 159 166 211
    254 196 177 117 175 212 31 90 75 237 171 232 232 111 183 115 200 81 179 152 165
    26 183 161 247 40 216 163 222 46 141 75 215 92 203 143 117 104 136 173 30 95
    124 116 134 153 60 119 252 65 79 156 227 169 150})

(fn fade [t]
  (* t t t (+ (* t (- (* t 6) 15)) 10)))

(fn perlin-noise [x y]
  (let [X (% (math.abs (math.floor x)) 256)
        Y (% (math.abs (math.floor y)) 256)
        xf (- x (math.floor x))
        yf (- y (math.floor y))
        u (fade xf)
        v (fade yf)
        idx-X1 (+ 1 X)
        idx-X2 (+ 1 (% (+ X 1) 256))
        val-X1 (or (. p idx-X1) 0)
        val-X2 (or (. p idx-X2) 0)
        A (% (+ val-X1 Y) 256)
        B (% (+ val-X2 Y) 256)
        aa (or (. p (+ 1 A)) 0)
        ab (or (. p (+ 1 (% (+ A 1) 256))) 0)
        ba (or (. p (+ 1 B)) 0)
        bb (or (. p (+ 1 (% (+ B 1) 256))) 0)]
    (- (/ (lerp (lerp aa ab u)
                (lerp ba bb u)
                v)
          128)
       1)))

(fn perlin-noise-3d [x y z]
  (let [X (% (math.abs (math.floor x)) 256)
        Y (% (math.abs (math.floor y)) 256)
        Z (% (math.abs (math.floor z)) 256)

        xf (- x (math.floor x))
        yf (- y (math.floor y))
        zf (- z (math.floor z))

        u (fade xf)
        v (fade yf)
        w (fade zf)

        ;; X-Y hashing
        A (% (+ (or (. p (+ 1 X)) 0) Y) 256)
        B (% (+ (or (. p (+ 1 (% (+ X 1) 256))) 0) Y) 256)

        ;; 3D hashing: combine with Z layer
        AA (% (+ (or (. p (+ 1 A)) 0) Z) 256)
        AB (% (+ (or (. p (+ 1 (% (+ A 1) 256))) 0) Z) 256)
        BA (% (+ (or (. p (+ 1 B)) 0) Z) 256)
        BB (% (+ (or (. p (+ 1 (% (+ B 1) 256))) 0) Z) 256)

        ;; Retrieve all 8 corner values of the 3D cube cell
        aaa (or (. p (+ 1 AA)) 0)
        aab (or (. p (+ 1 (% (+ AA 1) 256))) 0)
        aba (or (. p (+ 1 AB)) 0)
        abb (or (. p (+ 1 (% (+ AB 1) 256))) 0)
        baa (or (. p (+ 1 BA)) 0)
        bab (or (. p (+ 1 (% (+ BA 1) 256))) 0)
        bba (or (. p (+ 1 BB)) 0)
        bbb (or (. p (+ 1 (% (+ BB 1) 256))) 0)]

    ;; Trilinear interpolation across X, Y, and Z axes
    (- (/ (lerp (lerp (lerp aaa baa u)
                      (lerp aba bba u)
                      v)
                (lerp (lerp aab bab u)
                      (lerp abb bbb u)
                      v)
                w)
          128)
       1)))

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

(fn simulate-crt-trails []
  "Fades out alternating rows and instantly clears the scanline rows
   to prevent permanent pixel artifacts."
  (for [addr 0x0000 0x3FC0]
    (if (< (% addr 240) 120)
        (let [byte (peek addr)]
          (if (< 0 byte)
              (let [p1 (rshift byte 4)
                    p2 (band byte 0x0F)

                    p1-new (math.max 0 (- p1 1))
                    p2-new (math.max 0 (- p2 1))

                    new-byte (bor (lshift p1-new 4) p2-new)]
                (poke addr new-byte))))
        (poke addr 0))))

(fn my-noise-spiral [centerx centery radius color intensity rotations]
  (let [startradius (/ radius 10)
        noise-scale 0.9
        time-step (* myt 0.12)
        first-noise (perlin-noise-3d noise-scale time-step 0)
        first-radius (+ startradius (* first-noise 6 intensity))
        spiral {:startradius startradius
                :lastx (+ centerx first-radius) ;; cos(0) = 1
                :lasty centery}]                ;; sin(0) = 0
    (for [angle 5 (* 360 rotations) 5]
      (let [radians (math.rad angle)
            cos-r (math.cos radians)
            sin-r (math.sin radians)
            spatial-scale (* noise-scale (+ 1 (* angle 0.002)))
            base-growth (* 0.06 angle)
            n-val (perlin-noise-3d (* cos-r spatial-scale)
                                (+ (* sin-r spatial-scale) time-step) angle)
            thisradius (+ spiral.startradius base-growth (* n-val 6 intensity))
            x (+ centerx (* thisradius cos-r))
            y (+ centery (* thisradius sin-r))
            color-wave (% (+ (/ angle 30) (/ myt 4)) 6)
            dynamic-color (+ 2 (math.floor color-wave))]
        (line x y spiral.lastx spiral.lasty dynamic-color)
        (set spiral.lastx x)
        (set spiral.lasty y)))))

(var spiral-intensity 0)

(fn _G.TIC []
  ;; (cls 1)
  (simulate-crt-trails)
  (let [spiral-intensity (if (< myt 30) 0
                             (let [active-time (- myt 60)
                                   sine-wave (math.sin (- (* active-time 0.02) 1.5708))]
                               (/ (+ sine-wave 1) 2)))
        growth-sine (math.sin (- (* myt 0.015) 1.5708))
        normalized-growth (/ (+ growth-sine 1) 2)
        current-rotations (+ 2 (* normalized-growth 4))]
    (my-noise-spiral center-x center-y radius 4 spiral-intensity current-rotations))
  ;; (draw-fps)
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
