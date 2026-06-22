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
(var t 0)

(fn pal [c0 c1]
  "Palette swap helper."
  (if (and (= nil c0)
           (= nil c1))
      (for [i 0 15]
        (poke4 (+ i (* 0x3FF0 2)) i))
      (poke4 (+ c0 (* 0x3FF0 2)) c1)))

(fn draw-rotated-line [p1 p2 cx cy angle]
  "Rotates two points and draws a line between them in one go."
  ;; Rotate pt 1.
  (let [tx1 (- p1.x cx)
        ty1 (- p1.y cy)
        ;; Distance from center for Point 1
        dist1 (math.sqrt (+ (* tx1 tx1) (* ty1 ty1)))
        ;; Warp angle based on distance and time.
        angle1 (+ angle (/ dist1 40))
        cos-a1 (math.cos angle1)
        sin-a1 (math.sin angle1)
        rx1 (+ (- (* tx1 cos-a1) (* ty1 sin-a1)) cx)
        ry1 (+ (- (* tx1 sin-a1) (* ty1 cos-a1)) cy)
        sx1 (+ (- rx1) WIDTH)
        sy1 (+ (- ry1) HEIGHT)]

    ;; Rotate pt 2.
    (let [tx2 (- p2.x cx)
          ty2 (- p2.y cy)
          ;; Distance from center for Point 2
          dist2 (math.sqrt (+ (* tx2 tx2) (* ty2 ty2)))
          angle2 (+ angle (/ dist2 40))
          cos-a2 (math.cos angle2)
          sin-a2 (math.sin angle2)
          rx2 (+ (- (* tx2 cos-a2) (* ty2 sin-a2)) cx)
          ry2 (+ (- (* tx2 sin-a2) (* ty2 cos-a2)) cy)
          sx2 (+ (- rx2) WIDTH)
          sy2 (+ (- ry2) HEIGHT)]
      (line sx1 sy1 sx2 sy2 1))))

(fn draw-gasket [p1 p2 p3 depth cx cy angle]
  "Recursively draw the triangle outlines."
  (if (= depth 0)
      ;; Base case: draw outer edges of this triangle segment.
      (do
        (draw-rotated-line p1 p2 cx cy angle)
        (draw-rotated-line p2 p3 cx cy angle)
        (draw-rotated-line p3 p1 cx cy angle))
      ;; Recursive case: find the midpoints and subdivide into 3 smaller triangles.
      (let [m12 {:x (/ (+ p1.x p2.x) 2) :y (/ (+ p1.y p2.y) 2)}
            m23 {:x (/ (+ p2.x p3.x) 2) :y (/ (+ p2.y p3.y) 2)}
            m31 {:x (/ (+ p3.x p1.x) 2) :y (/ (+ p3.y p1.y) 2)}
            next-depth (- depth 1)]
        (draw-gasket p1 m12 m31 next-depth cx cy angle)
        (draw-gasket m12 p2 m23 next-depth cx cy angle)
        (draw-gasket m31 m23 p3 next-depth cx cy angle))))

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
  (cls 0)
  (let [cx (/ WIDTH 2)
        cy (/ HEIGHT 2)
        angle (* t (math.rad 0.25))

        p1 {:x 0           :y HEIGHT}
        p2 {:x (/ WIDTH 2) :y 0}
        p3 {:x WIDTH       :y HEIGHT}]
    (draw-gasket p1 p2 p3 5 cx cy angle))
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

