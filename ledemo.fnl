;; title:   ledemo
;; author:  jwatt@broken.watch
;; desc:    My first full demo
;; site:    jjwatt/play-tic80
;; license: GPL3
;; version: 0.8
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
        (when (< 0 z-inv)
          (let [sx (+ cx (/ (* base-x 100) z-inv) shake-x)
                sy (+ cy (/ (* base-y 100) z-inv))
                ;; Map brightness based on depth.
                base-color (math.floor (mapvalue z-inv 0 255 2 15))
                color (if (< 0.5 snare)
                          (math.random 12 15)
                          base-color)]
            (when (and (<= 0 sx) (< sx WIDTH) (<= 0 sy) (< sy HEIGHT))
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
                  bg-color (if (< 0.7 bass)
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

(fn draw-greets-bg [st]
  "Draws a dark, subtle sine grid that reacts gently to the kick."
  (let [kick (get-channel-vol 0 1.5)
        wave-t (* st 0.03)
        amplitude (+ 8 (* kick 12))]
    (for [x 0 WIDTH 12]
      (let [y-offset (* amplitude (math.sin (+ wave-t (* x 0.025))))]
        (line x 0 (+ x y-offset) HEIGHT 1)))
    (for [y 0 HEIGHT 12]
      (let [x-offset (* amplitude (math.cos (+ wave-t (* y 0.025))))]
        (line 0 y WIDTH (+ y x-offset) 1)))))

(local greets-data
       [{:duration 150 :names ["GREETS:" "FARBRAUSCH" "RAZOR 1911" "FAIRLIGHT" "ATARI SCENE"]}
        {:duration 300 :names ["@lukelightCO" "@center_of_chaos" "@torquevoid" "@devabram" "@olavostauros"]}
        {:duration 300 :names ["@Thelonius_1" "@monads4meals" "@HSVSphere" "@ludwigABAP"]}
        {:duration 300 :names ["@WadeGrimridge" "@traits_reality" "@LewisCTech" "@Duskyy78"]}
        {:duration 300 :names ["@imnonplussed" "@LukasHozda" "@CWood_sdf" "@BarellTitor44"]}
        {:duration 300 :names ["@LXIXthenumber" "@Aliasing__" "@e0syn" "@zuhaitz_dev" "@sudo_goreng"]}
        {:duration 300 :names ["@valigo" "@tsoding"]}
        {:duration 300 :names ["technomancy" "fennel-lang"]}
        {:duration 300 :names ["TPOT AND ALL TIC-80 HACKERS!" "ledemo" "by (infinite-jes)" "2026"]}])

(local greets-duration
       ((fn []
          (var total 0)
          (each [_ slide (ipairs greets-data)]
            (set total (+ total slide.duration)))
          total)))

(local scenes
       [
        {:duration 240 :transition-out :fade :trans-time 30 :draw (fn [st] (draw-star-tunnel st))}
        {:duration 400 :transition-out :scanlines :trans-time 30 :draw (fn [st] (draw-plasma st))}
        {:duration 300 :transition-out :fade
         :trans-time 30
         :draw (fn [st] (draw-wave-tunnel st))}
        {:duration 300
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
                   (draw-noise-spiral st center-x center-y radius spiral-intensity current-rotations)))}
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
                       kick (get-channel-vol 0 2.0)
                       base-scale (math.abs (math.cos (* st 0.03)))
                       scale-y (+ base-scale (* kick 0.5))]
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
                       line-state {:total 0 :max nil}
                       snare (get-channel-vol 1 1.5)
                       base-angle (* st (math.rad 0.75))
                       angle (+ base-angle (* snare (math.rad 20.0)))]
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
                       line-state {:total 0 :max nil}
                       kick (get-channel-vol 0 1.0)
                       snare (get-channel-vol 1 1.0)
                       base-angle (* (+ st 240) (math.rad 0.25))
                       angle (+ base-angle (* kick (math.rad 15)))
                       twist-factor (+ 1 (* snare 3.0))]
                   (draw-gasket p1 p2 p3 5 cx cy angle twist-factor 1.0 1 line-state)))}
        ;; Copper Curtain Background Grid
        {:duration 300
         :transition-out :scanlines
         :trans-time 30
         :draw (fn [st]
                 (let [snare (get-channel-vol 1 2.0)
                       cx (/ WIDTH 2)
                       cy (/ HEIGHT 2)
                       p1 {:x 0 :y 0}
                       p2 {:x (/ WIDTH 2) :y HEIGHT}
                       p3 {:x WIDTH :y 0}
                       line-state {:total 0 :max nil}
                       angle (* st (math.rad 0.5))
                       line-spacing (if (< 0.5 snare) 4 2)]
                   (for [y 0 HEIGHT line-spacing]
                     (line 0 y WIDTH y 1))
                   (let [gasket-color (if (< 0.6 snare) 12 0)]
                     (draw-gasket p1 p2 p3 5 cx cy angle 1 1.0 gasket-color line-state))))}
        ;; Greets
        {:duration greets-duration
         :transition-out :none
         :trans-time 0
         :draw (fn [st]
                 (draw-greets-bg st)
                 (var local-t st)
                 (var slide nil)
                 (var slide-idx 1)
                 (for [i 1 (# greets-data)]
                   (let [s (. greets-data i)]
                     (if (and (= slide nil) (< local-t s.duration))
                         (set slide s)
                         (when (= slide nil)
                           (set local-t (- local-t s.duration))
                           (set slide-idx (+ slide-idx 1))))))
                 (when slide
                   (let [kick (get-channel-vol 0 1.2)
                         snare (get-channel-vol 1 1.5)
                         center-y (/ HEIGHT 2)
                         num-names (# slide.names)
                         font-scale 1]
                     (for [i 1 num-names]
                       (let [name (. slide.names i)
                             direction (if (= (% (+ i slide-idx) 2) 0) 1 -1)
                             (print-w text-w) (print name 0 -20 0 false 1 true)
                             target-x (- (/ (- WIDTH print-w) 2) 20)
                             start-x (if (< 0 direction) (+ WIDTH 20) (- (+ print-w 20)))
                             entry-progress (math.min 1.0 (/ local-t 25))
                             eased-t (- 1 (math.pow (- 1 entry-progress) 3))
                             current-x (lerp start-x target-x eased-t)
                             line-y (- (+ (- center-y (* num-names 8)) (* i 20)) 10)
                             base-color 12
                             text-color (if (< 0.6 snare) 14 base-color)
                             final-x (if (< 0.5 kick) (+ current-x (- (math.random 0 2) 1)) current-x)]
                         (print name (+ final-x 1) (+ line-y 1) 0 false font-scale)
                         (print name final-x line-y text-color false font-scale))))))}
        {:duration 400
         :transition-out :scanlines
         :trans-time 30
         :draw (fn [st]
                 (draw-greets-bg st)
                 (let [logo-id 256
                       logo-w-tiles 16
                       logo-h-tiles 16
                       logo-w-px (* logo-w-tiles 8)
                       logo-h-px (* logo-h-tiles 8)
                       base-x (/ (- WIDTH logo-w-px) 2)
                       base-y (/ (- HEIGHT logo-h-px) 2)
                       kick (get-channel-vol 0 1.0)
                       snare (get-channel-vol 1 1.5)
                       shake-x (* snare (- (math.random 0 4) 2))
                       thump-y (if (< 0.4 kick) 2 0)
                       final-x (+ base-x shake-x)
                       final-y (+ base-y thump-y)
                       scale-factor 1
                       transparent-color 0]
                   (spr logo-id final-x final-y transparent-color scale-factor 0 0 logo-w-tiles logo-h-tiles)))}])

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

;; <SPRITES>
;; 002:0000000000000000ffffffffffffffffffffffffffffffffffffffffffffffff
;; 003:00000000ffffffffff0fffffffffffffffffffffffffffffffffffffffffff76
;; 004:fffffffffffffffffffffffffffffffffffffffffffffffffffff76766666666
;; 005:ffffcff0ffffeff0fffffff0fffffff0fffffff0fffffff0fffffffeffffff00
;; 018:ffffffffffffffff00ffffff00ffffff00ffffff00ffffff00ffffff00dfffff
;; 019:766666666666666676666666f6666666f6666666f6666666f6666666f6666666
;; 020:6666666666666666666666666666666666666666666666666666666666666666
;; 021:fffff0ee7fffffff6fffffff6fffffff6fffffff6fffffff67ffffff66ffffff
;; 022:000000000000000000000000000000000000000000000000f0000000f000ffff
;; 023:00000000000000000000000000000000000000000000000000000000fffff000
;; 025:000000000000000000000000000000df000000ef000000ef000fffff000fffff
;; 026:000000000000000000000000fff00000ffffff00ffffff00fffffff0fffffeff
;; 034:000fffff000fffff000fffff000fffff000fffff000f0fff0000ffff000fffff
;; 035:f7666666ff666666ff666666ff666666ff666666ff666666ff766666fff66666
;; 036:6666666666666666666666666666666666666666666666666666666666666666
;; 037:66ffffff66ffffff66ffffff66ffffff667fffff666fffff666fffff666fffff
;; 038:f000fffff000fffff000fffff000ffffff00ffffff00ffffffffffffffffffff
;; 039:ffffffffffffffffffffffffffffffffffffffffffffffffff767fffff666666
;; 040:ffffffffffffffffffffffffffffffffffffffffffffffffffffffff6667ffff
;; 041:fdfffffffdfffffffffffffffffffffffffffffffffffffffffffffffffffff6
;; 042:fffffffffffffffffffffffffffffffff66fffff6666ffff66666fff6666667f
;; 043:fff00000fff00000ffff0000ffffff00ffffffcfffffffcfffffffffffffffff
;; 044:000000000000000000000000000000000000000000000000f0000000f0000000
;; 050:0000ffff0000ffff0000ffff0000ffff0000ffff0000ffff0000ffff000000ff
;; 051:fff66666fff66666fff76666ffff6666ffff6666ffff6666ffff6666ffff6666
;; 052:6666666666666666666666666666666666666666666666666666666666666666
;; 053:666fffff666fffff666fffff666fffff6666ffff6666ffff6666ffff6666ffff
;; 054:ffffcfffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; 055:ff666666ff666666ff666666ff666666f7666666f6666666f6666666f6666666
;; 056:6666ffff6666ffff6666ffff6666ffff6666ffff6666ffff6666ffff6666ffff
;; 057:fffffff6ffffff66fffff766fffff666ffff6666ffff6666fff66666ff766666
;; 058:6666666666666666666666666666666666666666666666676666666f666666ff
;; 059:ffffffff6fffffff6fffffff6ffffffffffffffffffffffffffffffffffffff0
;; 060:f0000000f0000000f0000000f000000000000000000000000000000000000000
;; 066:000000ff000000ff000000ff000000ff000000ff000000ff000000ff0000000f
;; 067:fffff666fffff666fffff666fffff666fffff666ffffff66ffffff66ffffff66
;; 068:6666666666666666666666666666666666666666666666666666666f666666ff
;; 069:6666ffff6666ffff6666ffff666fffff66ffffff6fffffffffffffffffffffff
;; 070:fffffffffffffffffff0ffffffffffffffffffffffffffffffffffffffffffff
;; 071:f6666666f6666666766666667666666666666666666666666666666666666666
;; 072:666fffff666fffff666fffff666fffff666fffff667ffff766fffff666fffff6
;; 073:ff666666f7666666f66666666666666666666666666666666666666f66666fff
;; 074:666667ff66666fff6666ffff666fffff66ffffff6fffffffffffffffffffffff
;; 075:fffffff0ffff0000ffffdd00ff0f0000ffff0000fff00000fff00000fff00000
;; 082:0000000f0000000f0000000f0000000f0000000f0000000f0000ff0f0000ff00
;; 083:ffffff76fffffff6fffffff6fffffff6ffffffffffffffffffffffff0fffffff
;; 084:66666fff6666ffff66ffffff66ffffffffffffffffffffffffffffffffffffee
;; 085:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffedddddcc
;; 086:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffcccddddd
;; 087:6666666666666ffff6ffffffffffffffffffffffffffffffffffffffeeefffff
;; 088:fffffff6ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; 089:66ffffff6fffffffffffffffffffffffffffffffffffffffffff0fffff000fff
;; 090:fffffffffffffffffffffffffffffffffffff0f0fffffff0ffffff00ffffff00
;; 091:00000000ff000000000000000000000000000000000000000000000000000000
;; 097:00000000000000000000000f0000000f000000ff0000ffff000fffff000fffff
;; 098:000fffffff0fffffffeffffffffffffffffffffffffffffefffffeedffffedcc
;; 099:fffffffffffffffefffffeddfffedcccfedcccccddcccccccccccccccccccccc
;; 100:ffeeddddeddccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 101:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 102:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 103:cddddeefccccccddcccccccccccccccccccccccccccccccccccccccccccccccc
;; 104:ffffffffeeffffffcddeffffccccdeffccccccdecccccccdcccccccccccccccc
;; 105:ff00ffffffffffffffffffffffffffffffffffffdeffffffcdeeffffcccdefff
;; 106:ffffff00fffff000fff00000fff00000fff00000fffe0000ffffffe0fffff0e0
;; 112:00000000000000000000000f000000ff000000ff000000fe00000fff000fff0f
;; 113:00ffffff0ffffffffffffffffffffffffffffffdffffffdcfffffedcffffedcc
;; 114:fffedcccffedccccfecccccceccccccccccccccccccccccccccccccccccccccc
;; 115:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 116:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 117:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 118:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 119:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 120:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 121:ccccdeffcccccddecccccccdcccccccccccccccccccccccccccccccccccccccc
;; 122:ffffff00ffffffffefffffffdeffffffcdefffffccdfffffccceffffccccefff
;; 123:0000000000000000ff000000fff00000fff00000ffff0000ffff0000ffffff00
;; 128:000fffff000fffff00ffffff00ffffff00ffffff00ffffff0effffff0fdffffe
;; 129:fffedcccfffdccccffedccccfedcccccfdccccccedccccccecccccccdccccccc
;; 130:cccccccccccccccccccccccccccccccccccccccccccccccccccccccdccccccde
;; 131:ccccccccccccccccccccccccccccccccccccccccddeeeddceeffffedffffffff
;; 132:ccccccccccccccccccccccccccccccccccccccccccccccccdcccccccedcccccc
;; 133:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 134:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 135:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccd
;; 136:ccccccccccccccccccccccccccccccccccccccccccddeeedddeffffeefffffff
;; 137:ccccccccccccccccccccccccccccccccccccccccdcccccccedccccccffdccccc
;; 138:ccccdeffcccccdefccccccefccccccdecccccccdcccccccdcccccccccccccccc
;; 139:ffffff00ffffff00fffffff0fffffff0fffffff0efffffffdffffffedeffffff
;; 144:0ffffffe0fffffedffffffedffffffecffffffdcfffffedcfffffedcfffffedc
;; 145:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 146:ccccceffccccdeffcccdefffcccdfffeccdeffedccdffedccceffdcccdefeccc
;; 147:ffffffffffffffffffffffffefffffffddefffffccdeffffccceffffcccdffff
;; 148:fedcccccffedccccfffdccccfffedcccffffdcccffffecccffffedccfffffdcc
;; 149:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 150:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 151:ccccccdeccccccefcccccdffccccdeffccccdfffcccceffdcccdefeccccdefec
;; 152:fffffffffffffffffffffffffeefffffeddeffffccccefffccccdeffccccceff
;; 153:fffeccccffffdcccffffedccfffffdccfffffedcffffffdcffffffecffffffed
;; 154:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 155:ceffffffcdefffffcdefffffcceffffeccdfffffccdeffffccdeffffccdeffff
;; 156:e0000000e000000000000000f0000000f0000000f0000000f0000000f0000000
;; 160:fffffdccfffffdccfffffdccfffffdccfffffdccfffffdccfffffdccfffffdcc
;; 161:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 162:cdefeccccdefeccccdffecccceffedcccefffdcccefffedcceffffeecdffffff
;; 163:cccdffffcccdffffcccdffffcccdffffccdeffffccefffffdeffffffffffffff
;; 164:fffffeccfffffeccfffffeccffffffccffffffdcffffffccffffffccfffffecc
;; 165:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 166:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 167:ccceffdcccceffdcccceffdcccceffecccceffedcccefffeccceffffccceffff
;; 168:cccccdffcccccdffccccceffccccdeffccccdeffcccdefffeeeeffffffffffff
;; 169:ffffffedfffffffdfffffffefffffffefffffffefffffffefffffffefffffffe
;; 170:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 171:cccdffffcccdffffcccdffffcccdffffcccdffffcccdffffcccdffffcccdffff
;; 172:f0000000f0000000f0000000f0000000f0000000f0000000f0000000f0000000
;; 176:fffffdccfffffeccfffffedcfffffedcffffffdcffffffdcffffffecffffffed
;; 177:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 178:cdefffffcdefffffccefffffccefffffccdeffffccdeffffcccdffffccccefff
;; 179:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; 180:fffffeccfffffeccffffedccffffedccffffecccfffedcccfffeccccffedcccc
;; 181:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 182:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 183:ccceffffcccdffffcccdefffccccefffccccefffccccdeffcccccdffcccccdef
;; 184:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; 185:ffffffedffffffedffffffedffffffecffffffdcfffffedcfffffdccffffedcc
;; 186:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 187:cccdffffccdeffffccdeffffccdeffffccdfffffccefffffcdefffffcdefffff
;; 188:f0000000f0000000f0000000f0000000f0000000f0000000f0000000f0000000
;; 192:0ffffffe0ffffffe0fffffff0fffffff00ffffff00f0ffff00ffffff00ffffff
;; 193:ccccccccccccccccdcccccccecccccccfdccccccfedcccccffdcccccffedcccc
;; 194:ccccdeffcccccdefccccccdecccccccdcccccccccccccccccccccccccccccccc
;; 195:fffffffffffffffffffffffedeeeeeddcdddddcccccccccccccccccccccccccc
;; 196:ffdcccccfeccccccdccccccccccccccccccccccccccccccccccccccccccccccc
;; 197:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 198:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 199:ccccccdfcccccccecccccccccccccccccccccccccccccccccccccccccccccccc
;; 200:ffffffffefffffffdeffffffcddeefeeccddddddcccccccccccccccccccccccc
;; 201:fffedcccffedccccfedcccccddcccccccccccccccccccccccccccccccccccccc
;; 202:cccccccccccccccccccccccdcccccccdccccccddccccccdecccccddfccccddef
;; 203:ceffffffdeffffffdfffffffeffffff0fffffffefffffff0fffffff0fffffff0
;; 208:000eefff00000fff00000fff00000fff0000000f000000ff0000000f0000000f
;; 209:fffeccccffffdcccfffffdccfffffedcffffffedfffffffeffffffffffffffff
;; 210:ccccccccccccccccccccccccccccccccccccccccdcccccccedccccccfedccccc
;; 211:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 212:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 213:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 214:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 215:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 216:cccccccccccccccccccccccccccccccccccccccccccccccccccccc4dcccc4ddd
;; 217:cccccccccccccccccccccc4dcccc4dddcc4ddddd4dddddddddddddddddddddde
;; 218:cccddeff44dddfffddddffffdddeffffddefffffdeffffffefffffffffffffff
;; 219:ffffff00ffffff00ffff0000ffff0000fff00000fff00000ff000000ff000000
;; 225:0fffffff00ffffff000fffff000fffff0000ffff000000ff0000000f0000000f
;; 226:ffedccccfffeddccfffffedcffffffedffffffffffffffffffffffffee0fffff
;; 227:ccccccccccccccccccccccccccccccccedccccccfeeddcccffffeddcffffffed
;; 228:ccccccccccccccccccccccccccccccccccccccccccccccccccccccccdccccccc
;; 229:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 230:cccccccccccccccccccccccccccccccccccccccccccccccccccccc4dcccc4ddd
;; 231:cccccccccccccccccccccc4dcccc4dddcc4ddddd4ddddddddddddddddddddddd
;; 232:cc4ddddd4ddddddddddddddddddddddddddddddeddddddeeddddefffdeefffff
;; 233:ddddddefddddeeffdddeffffdeefffffefffffffffffffffffffffffffffffff
;; 234:fffffffffffffff0ffffff00ffffff00fffff000fff00000ff000000ff000000
;; 242:ee0fffff000fffff000000ff0000000000000000000000000000000000000000
;; 243:ffffffffffffffffffffffffffffffffffffffff000fffff0000000f00000000
;; 244:eeedddccfffeeeedffffffffffffffffffffffffffffffffffffffff0000ffff
;; 245:ccccccccddddddddeeeeeeddffffffffffffffffffffffffffffffffffffffff
;; 246:ccc4d4dddddddddddddeeeeeffffffffffffffffffffffffffffffffffffffff
;; 247:ddddddeeddeeeeffeffffffffffffffffffffffffffffffffffffffffffff000
;; 248:effffffffffffffffffffffffffffff0fffffff0ffffff00ff00000000000000
;; 249:ffffffffffffff00fff00000e0000000e0000000000000000000000000000000
;; 250:f000000000000000000000000000000000000000000000000000000000000000
;; </SPRITES>

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

