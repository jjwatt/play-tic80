;; title:   ledemo
;; author:  jwatt@broken.watch
;; desc:    My first full demo
;; site:    jjwatt/play-tic80
;; license: GPL3
;; version: 0.5
;; script:  fennel
;; strict:  true
(global WIDTH 240)
(global HEIGHT 136)
(var t 0)
(var scene-idx 1)
(var scene-t 0)

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

(fn pal [?c0 ?c1]
  "Palette swap helper."
  (if (and (= nil ?c0)
           (= nil ?c1))
      (for [i 0 15]
        (poke4 (+ i (* 0x3FF0 2)) i))
      (poke4 (+ ?c0 (* 0x3FF0 2)) ?c1)))

(fn get-bass-amplitude []
  "Peeks channel 0 & 1 volume register memory to gauge bass/kick volume."
  (let [ch0-vol (band (peek 0x0FF9C) 0x0F)
        ch1-vol (band (peek 0xFFA4) 0x0F)]
    (/ (+ ch0-vol ch1-vol) 30.0)))

(fn get-channel-vol [ch ?boost-factor]
  "Direct mapping to avoid any inline calculation shifting anomalies."
  (let [addr (match ch
               0 0x0FF9C
               1 0x0FFA4
               2 0x0FFAC
               3 0x0FFB4
               _ 0x0FF9C)
        raw-vol (band (peek addr) 0x0F)
        norm-vol (/ raw-vol 15.0)]
    (math.min 1.0 (* norm-vol (or ?boost-factor 1.0)))))

(fn draw-rotated-line [p1 p2 cx cy angle twist-factor scale-y color-idx line-state]
  "Rotates two points and draws a line between them in one go."
  (set line-state.total (+ line-state.total 1))

  (if (or (= line-state.max nil) (<= line-state.total line-state.max))
      (let [tx1 (- p1.x cx)
            ty1 (- p1.y cy)
            dist1 (math.sqrt (+ (* tx1 tx1) (* ty1 ty1)))
            angle1 (+ angle (* (/ dist1 40) twist-factor))
            cos-a1 (math.cos angle1)
            sin-a1 (math.sin angle1)
            rx1 (+ (- (* tx1 cos-a1) (* ty1 sin-a1)) cx)
            ry1 (+ (- (* tx1 sin-a1) (* ty1 cos-a1)) cy)
            sy1-final (+ (* (- ry1 cy) scale-y) cy)
            sx1 rx1
            sy1 sy1-final
            tx2 (- p2.x cx)
            ty2 (- p2.y cy)
            dist2 (math.sqrt (+ (* tx2 tx2) (* ty2 ty2)))
            angle2 (+ angle (* (/ dist2 40) twist-factor))
            cos-a2 (math.cos angle2)
            sin-a2 (math.sin angle2)
            rx2 (+ (- (* tx2 cos-a2) (* ty2 sin-a2)) cx)
            ry2 (+ (- (* tx2 sin-a2) (* ty2 cos-a2)) cy)
            sy2-final (+ (* (- ry2 cy) scale-y) cy)
            sx2 rx2
            sy2 sy2-final]
        (line sx1 sy1 sx2 sy2 color-idx))))

(fn draw-gasket [p1 p2 p3 depth cx cy angle twist-factor scale-y color-idx line-state]
  "Recursively draw the triangle outlines."
  (if (= depth 0)
      ;; Base case: draw outer edges of this triangle segment.
      (do
        (draw-rotated-line p1 p2 cx cy angle twist-factor scale-y color-idx line-state)
        (draw-rotated-line p2 p3 cx cy angle twist-factor scale-y color-idx line-state)
        (draw-rotated-line p3 p1 cx cy angle twist-factor scale-y color-idx line-state))
      ;; Recursive case: find the midpoints and subdivide into 3 smaller triangles.
      (let [m12 {:x (/ (+ p1.x p2.x) 2) :y (/ (+ p1.y p2.y) 2)}
            m23 {:x (/ (+ p2.x p3.x) 2) :y (/ (+ p2.y p3.y) 2)}
            m31 {:x (/ (+ p3.x p1.x) 2) :y (/ (+ p3.y p1.y) 2)}
            next-depth (- depth 1)]
        (draw-gasket p1 m12 m31 next-depth cx cy angle twist-factor scale-y color-idx line-state)
        (draw-gasket m12 p2 m23 next-depth cx cy angle twist-factor scale-y color-idx line-state)
        (draw-gasket m31 m23 p3 next-depth cx cy angle twist-factor scale-y color-idx line-state))))

(fn draw-star-tunnel [st]
  "Projects a 3D starfield tunnel moving forward over time."
  (let [cx (/ WIDTH 2)
        cy (/ HEIGHT 2)
        num-stars 180
        kick (get-channel-vol 0)
        snare (get-channel-vol 1)
        ;; Kick drum boosts star velocity.
        base-speed 1.5
        speed (+ base-speed (* kick 4.5))
        shake-x (* snare (- (math.random 0 6) 3))]
    (for [i 1 num-stars]
      (math.randomseed i)
      (let [base-x (- (math.random 0 WIDTH) cx)
            base-y (- (math.random 0 HEIGHT) cy)
            z (% (- (+ (math.random 1 255) (* st speed)) 1) 255)
            z-inv (- 255 z)]
        (when (> z-inv 0)
          (let [sx (+ cx (/ (* base-x 100) z-inv) shake-x)
                sy (+ cy (/ (* base-y 100) z-inv))
                ;; Map brightness based on depth.
                base-color (math.floor (mapvalue z-inv 0 255 2 15))
                color (if (< 0.5 snare)
                          (math.random 12 15)
                          base-color)]
            (when (and (>= sx 0) (< sx WIDTH) (>= sy 0) (< sy HEIGHT))
              (pix sx sy color))))))))

(fn draw-plasma [st]
  "Generates an interference pattern plasma mapped directly to pixel space."
  (let [hihat (get-channel-vol 0 2.5)
        bass (get-channel-vol 1 2.5)
        t-factor (+ (* st 0.04) (* hihat 0.2))
        color-spread (+ 3 (* bass 4))]
    (for [y 0 HEIGHT 2]
      (for [x 0 WIDTH 2]
        (let [v1 (math.sin (+ (* x 0.05) t-factor))
              v2 (math.sin (+ (* y 0.05) t-factor))
              v3 (math.sin (+ (* (+ x y) 0.03) t-factor))
              cx (+ x (* 10 (math.sin t-factor)))
              cy (+ y (* 10 (math.cos t-factor)))
              v4 (math.sin (* 0.05 (math.sqrt (+ (* cx cx) (* cy cy)))))

              total-v (+ v1 v2 v3 v4)
              color-idx (+ 4 (math.floor (* color-spread (+ total-v 4))))]
          (rect x y 2 2 (% color-idx 15)))))))

(fn draw-wave-tunnel [st]
  "Draws an oscillating tunnel using overlapping vector rings."
  (let [cx (/ WIDTH 2)
        cy (/ HEIGHT 2)
        ring-count 20
        audio-kick (get-channel-vol 0)]
    (for [i 1 ring-count]
      (let [ring-t (+ st (* i 8))
            ;; Push standard radius out wider if sound is louder.
            radius (* i (+ 6 (* audio-kick 4)))
            ;; Modulate line speed by audio peak velocity.
            offset-x (* (+ 30 (* audio-kick 20)) (math.sin (* ring-t 0.02)))
            offset-y (* (+ 20 (* audio-kick 15)) (math.cos (* ring-t 0.035)))
            color (math.max 2 (% (+ i (math.floor (/ st 4))) 15))]
        (circb (+ cx offset-x) (+ cy offset-y) radius color)))))

(fn draw-noise-spiral [st centerx centery radius intensity rotations]
  (let [startradius (/ radius 10)
        noise-scale 0.9
        time-step (* st 0.12)
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
            color-wave (% (+ (/ angle 30) (/ st 4)) 6)
            dynamic-color (+ 2 (math.floor color-wave))]
        (line x y spiral.lastx spiral.lasty dynamic-color)
        (set spiral.lastx x)
        (set spiral.lasty y)))))

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

(fn draw-ambient-background [st]
  "Draws a faint shifting starfield grid."
  (let [bass (get-channel-vol 1)
        time-step (* st 0.05)
        bg-scale 0.10
        threshold (- 0.5 (* bass 0.15))]
    (for [y 0 HEIGHT 8]
      (for [x 0 WIDTH 8]
        (let [n-val (perlin-noise (* x bg-scale) (+ (* y bg-scale) time-step))]
          (when (< threshold n-val)
            (let [color-wave (% (+ (* n-val 5) (/ st 8)) 5)
                  bg-palette [1 2 8 12 14]
                  bg-color (if (> bass 0.7)
                               15
                               (. bg-palette (+ 1 (math.floor color-wave))))]
              (pix x y bg-color))))))))

(fn apply-wipe [progress mode]
  "Draws a horizontal curtain screen-wipe based on progress (0 to 1)."
  (let [w (* WIDTH progress)]
    (if (= mode :out)
        ;; Wipe out (grow from left)
        (rect 0 0 w HEIGHT 0)
        ;; Wipe in (recede to right)
        (rect w 0 (- WIDTH w) HEIGHT 0))))

(fn apply-fade [progress mode]
  "Retro dither fade simulation using scanline masking."
  (let [step (math.floor (mapvalue progress 0 1 0 HEIGHT))]
    (if (= mode :out)
        (rect 0 0 WIDTH step 0)
        (rect 0 step WIDTH (- HEIGHT step) 0))))

(fn apply-scanlines [progress mode]
  "Draws alternating horizontal blinds that choke out or reveal the screen."
  (let [band-height 8]
    (for [y 0 HEIGHT band-height]
      (if (= mode :out)
          (let [current-h (* band-height progress)]
            (rect 0 y WIDTH current-h 0))
          (let [current-h (* band-height (- 1 progress))]
            (rect 0 y WIDTH current-h 0))))))

(fn draw-transition-effect [effect-type progress mode]
  "Dispatches to the correct visual."
  (match effect-type
    :wipe (apply-wipe progress mode)
    :fade (apply-fade progress mode)
    :scanlines (apply-scanlines progress mode)
    :none nil
    _     nil))

(local scenes
       [
        {:duration 240 :transition-out :fade :trans-time 30 :draw (fn [st] (draw-star-tunnel st))}
        {:duration 400 :transition-out :scanlines :trans-time 30 :draw (fn [st] (draw-plasma st))}
        {:duration 300 :transition-out :fade
         :trans-time 30
         :draw (fn [st] (draw-wave-tunnel st))}
        {:duration 400
         :transition-out :scanlines
         :trans-time 30
         :draw (fn [st]
                 (simulate-crt-trails)
                 (draw-ambient-background st)
                 (let [bass-drop (get-bass-amplitude)
                       radius (+ 20 (* bass-drop 15))
                       center-x (/ WIDTH 2)
                       center-y (/ HEIGHT 2)
                       bass-intensity (if (< st 30) 0
                                            (let [active-time (- st 60)
                                                  sine-wave (math.sin (- (* active-time 0.02) 1.5708))]
                                              (/ (+ sine-wave 1) 2)))
                       spiral-intensity (+ bass-intensity (* bass-drop 0.8))
                       growth-sine (math.sin (- (* st 0.015) 1.5708))
                       normalized-growth (/ (+ growth-sine 1) 2)
                       current-rotations (+ 2 (* normalized-growth 4) (* bass-drop 2))]
                   (draw-noise-spiral st center-x center-y radius spiral-intensity current-rotations))
)}
        ;; Gasket 1: Draw N lines at a time sequentially (no movement).
        {:duration 240
         :transition-out :none
         :trans-time 0
         :draw (fn [st]
                 ;; Gently increase max lines allowed to render over time.
                 (let [cx (/ WIDTH 2)
                       cy (/ HEIGHT 2)
                       p1 {:x 0 :y 1}
                       p2 {:x (/ WIDTH 2) :y HEIGHT}
                       p3 {:x WIDTH :y 1}
                       kick (get-channel-vol 0 2.0)
                       audio-boost (* kick 150)
                       base-lines (* (/ st 180) 729)
                       lines-to-draw (math.floor (+ base-lines audio-boost))
                       line-state {:total 0 :max lines-to-draw}]
                   (draw-gasket p1 p2 p3 5 cx cy 0 0 1.0 1 line-state)))}
        ;; Rotate gasket around X-axis 3D pitching
        {:duration 180
         :transition-out :none
         :trans-time 0
         :draw (fn [st]
                 (let [cx (/ WIDTH 2)
                       cy (/ HEIGHT 2)
                       p1 {:x 0 :y 0}
                       p2 {:x (/ WIDTH 2) :y HEIGHT}
                       p3 {:x WIDTH :y 0}
                       line-state {:total 0 :max nil}
                       scale-y (math.abs (math.cos (* st 0.03)))]
                   (draw-gasket p1 p2 p3 5 cx cy 0 0 scale-y 1 line-state)))}
        ;; Gasket spin
        {:duration 300
         :transition-out :scanlines
         :trans-time 30
         :draw (fn [st]
                 (let [cx (/ WIDTH 2)
                       cy (/ HEIGHT 2)
                       angle (* st (math.rad 0.75))
                       p1 {:x 0 :y 0}
                       p2 {:x (/ WIDTH 2) :y HEIGHT}
                       p3 {:x WIDTH :y 0}
                       line-state {:total 0 :max nil}]
                   (draw-gasket p1 p2 p3 5 cx cy angle 0 1.0 1 line-state)))}
        ;; Twisting Gasket
        {:duration 300
         :transition-out :none
         :trans-time 0
         :draw (fn [st]
                 (let [cx (/ WIDTH 2)
                       cy (/ HEIGHT 2)
                       angle (* (+ st 240) (math.rad 0.25))
                       p1 {:x 0 :y 0}
                       p2 {:x (/ WIDTH 2) :y HEIGHT}
                       p3 {:x WIDTH :y 0}
                       line-state {:total 0 :max nil}]
                   (draw-gasket p1 p2 p3 5 cx cy angle 1 1.0 1 line-state)))}
        ;; Copper Curtain Background Grid
        {:duration 300
         :transition-out :scanlines
         :trans-time 30
         :draw (fn [st]
                 (for [y 0 HEIGHT 2]
                   (line 0 y WIDTH y 1))
                 (let [cx (/ WIDTH 2)
                       cy (/ HEIGHT 2)
                       p1 {:x 0 :y 0}
                       p2 {:x (/ WIDTH 2) :y HEIGHT}
                       p3 {:x WIDTH :y 0}
                       line-state {:total 0 :max nil}
                       angle (* st (math.rad 0.5))]
                   (draw-gasket p1 p2 p3 5 cx cy angle 1 1.0 0 line-state)))}])


(fn _G.BOOT []
  (music 0))

(fn _G.BDR [y]
  "Raster interrupt for rotating palette, skip black."
  ;; Fires 136 times per frame (once per scanline)
  ;; y represents the current horizontal row being drawn.
  (let [scroll-speed 0.5
        line-index (+ y (* t scroll-speed))
        total-lines 64
        phase (/ (% line-index total-lines) total-lines)
        num (math.floor (+ 2 (* 13 phase)))]
    (pal 1 num)))

(fn _G.TIC []
  ;; Clear screen to background index
  (rect 0 0 WIDTH HEIGHT 0)
  ;; Disable mouse cursor
  (poke 0x3FFB 0)
  (let [current-scene (. scenes scene-idx)
        prev-scene (. scenes (- scene-idx 1))]
    (if current-scene
        (let [sdur current-scene.duration
              ttime (or current-scene.trans-time 0)
              etype current-scene.transition-out]
          (current-scene.draw scene-t)
          ;; Process transition out.
          (if (and etype (< (- sdur ttime) scene-t) (< 0 ttime))
              (let [progress (/ (- scene-t (- sdur ttime)) ttime)]
                (draw-transition-effect etype progress :out))
              (and prev-scene prev-scene.transition-out (< scene-t ttime))
              (let [prev-ttime (or prev-scene.trans-time 0)]
                (when (< 0 prev-ttime)
                  (let [progress (/ scene-t prev-ttime)]
                    (draw-transition-effect prev-scene.transition-out progress :in)))))
          ;; Set clock timers forward.
          (set scene-t (+ scene-t 1))
          (when (<= sdur scene-t)
            (set scene-idx (+ scene-idx 1))
            (set scene-t 0)))
        ;; Reset to beginning if sequence finishes.
        (do
          (set scene-idx 1)
          (set scene-t 0))))
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
;; 000:010021005100a100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100300000000000
;; 001:1307230533025300630083009300a300b300c300d300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300000400000000
;; 002:1307f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300000400000000
;; 003:100030004000500080009000a000b000b000c000c000c000c000c000d000d000e000e000e000f000f000f000f000f000f000f000f000f000f000f000000400000000
;; 004:0007100820072008400760086007700880079008a007b008c007c000d000e000f000f000f000f000f000f000f000f000f000f000f000f000f000f000000400000000
;; </SFX>

;; <PATTERNS>
;; 000:400006000000000000000000000000000000000000000000400006400006000000000000000000000000000000000000400006000000000000000000000000000000000000000000400006400006000000000000000000000000000000000000400006000000000000000000000000000000000000000000400006400006000000000000000000000000000000000000400006000000000000000000000000000000000000000000400006400006000000000000000000000000000000000000
;; 001:000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000
;; 002:000000000000400026000000000000000000400026000000000000000000400026000000000000000000400026400026000000000000400026000000000000000000400026000000000000000000400026000000000000000000400026400026000000000000400026000000000000000000400026000000000000000000400026000000000000000000400026400026000000000000400026000000000000000000400026000000000000000000400026000000000000000000400026400026
;; 003:400036800036b00036400038000000000000000000000000000000000000000000000000000000000000000000000000400036800036b00036400038000000000000000000000000000000000000000000000000000000000000000000000000400036800036b00036400038000000000000000000000000000000000000000000000000000000000000000000000000400036800036b00036400038000000000000000000000000000000000000000000000000000000000000000000000000
;; 004:400036800036b00036400038440636830636b2063640a638000000000000000000000000000000000000000000000000400036800036b00036400038440636830636b2063640a638000000000000000000000000000000000000000000000000400036800036b00036400038440636830636b2063640a638000000000000000000000000000000000000000000000000400036800036b00036400038440636830636b2063640a638000000000000000000000000000000000000000000000000
;; 005:000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016000000000000000000000000000000000000000000400016400016400016400016
;; 006:000000000000400026000000000000000000400026000000000000000000400026000000000000000000400026400026000000000000400026000000000000000000400026000000000000000000400026000000000000000000400026400026000000000000400026000000000000000000400026000000000000000000400026000000000000000000400026400026000000000000400026000000000000000000400026000000000000000000400026000000000000000000000000000000
;; </PATTERNS>

;; <TRACKS>
;; 000:1803001817011817411803001817011806410000000000000000000000000000000000000000000000000000000000002e0000
;; </TRACKS>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>
