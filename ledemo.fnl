;; title:   gasketdemo
;; author:  jwatt@broken.watch
;; desc:    sierpinksi gasket demo
;; site:    jjwatt/play-tic80
;; license: GPL3
;; version: 0.2
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

(fn pal [?c0 ?c1]
  "Palette swap helper."
  (if (and (= nil ?c0)
           (= nil ?c1))
      (for [i 0 15]
        (poke4 (+ i (* 0x3FF0 2)) i))
      (poke4 (+ ?c0 (* 0x3FF0 2)) ?c1)))

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
        speed 1.8]
    (for [i 1 num-stars]
      (math.randomseed i)
      (let [base-x (- (math.random 0 WIDTH) cx)
            base-y (- (math.random 0 HEIGHT) cy)
            z (% (- (+ (math.random 1 255) (* st speed)) 1) 255)
            z-inv (- 255 z)]
        (when (> z-inv 0)
          (let [sx (+ cx (/ (* base-x 100) z-inv))
                sy (+ cy (/ (* base-y 100) z-inv))
                color (math.floor (mapvalue z-inv 0 255 2 15))]
            (when (and (>= sx 0) (< sx WIDTH) (>= sy 0) (< sy HEIGHT))
              (pix sx sy color))))))))

(fn draw-plasma [st]
  "Generate an interference pattern plasma mapped directly to pixel space."
  (let [t-factor (* st 0.04)]
    (for [y 0 HEIGHT 2]
      (for [x 0 WIDTH 2]
        (let [v1 (math.sin (+ (* x 0.05) t-factor))
              v2 (math.sin (+ (* y 0.05) t-factor))
              v3 (math.sin (+ (* (+ x y) 0.03) t-factor))
              cx (+ x (* 10 (math.sin t-factor)))
              cy (+ y (* 10 (math.cos t-factor)))
              v4 (math.sin (* 0.05 (math.sqrt (+ (* cx cx) (* cy cy)))))
              ;; Combine waves.
              total-v (+ v1 v2 v3 v4)
              color-idx (+ 4 (math.floor (* 3 (+ total-v 4))))]
          (rect x y 2 2 (% color-idx 15)))))))

(fn draw-wave-tunnel [st]
  "Draws an oscillating tunnel using overlapping vector rings."
  (let [cx (/ WIDTH 2)
        cy (/ HEIGHT 2)
        ring-count 20]
    (for [i 1 ring-count]
      (let [ring-t (+ st (* i 8))
            radius (* i 6)
            ;; Oscillate centers independently across the screen.
            offset-x (* 30 (math.sin (* ring-t 0.02)))
            offset-y (* 20 (math.cos (* ring-t 0.035)))
            color (math.max 2 (% (+ i (math.floor (/ st 4))) 15))]
        (circb (+ cx offset-x) (+ cy offset-y) radius color)))))

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
        {:duration 300 :transition-out :fade :trans-time 30 :draw (fn [st] (draw-wave-tunnel st))}
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
                       lines-to-draw (math.floor (* (/ st 180) 729))
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

