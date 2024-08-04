;; title:   spn2fen
;; author:  jwatt@broken.watch
;; desc:    Spiral noise 2 with fennel
;; site:    website link
;; license: GPL3
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
(local SCREEN_SIZE (* WIDTH HEIGHT))
(local VRAM_ADDR 0x0000)
(local OFFSCREEN_ADDR 0x8000) ;; Start of free RAM

;; let's try some let macros.
(macro let* [bindings body & rest]
  (let [car (fn [lst]
              (. lst 1))
        cdr (fn [lst]
              (icollect [i v (ipairs lst)]
                (if (not= 1 i) v)))
        empty? (fn [t]
                 (if (= nil (next t))
                     true
                     false))]
  (if (empty? bindings)
      `(do ,body ,(table.unpack rest))
      `(let ,(car bindings)
            (let* ,(cdr bindings) ,body ,rest)))))

;; Trying to write directly to memory and do double-buffering
;; Doesn't seem to work.
(fn bltBuffer []
  (memcpy OFFSCREEN_ADDR VRAM_ADDR SCREEN_SIZE))
(fn clearBuffer []
  (memset OFFSCREEN_ADDR 0 SCREEN_SIZE))
(fn db-pix [x y color]
  (poke4 (+ (* y WIDTH) OFFSCREEN_ADDR x) color))
(fn db-line [x1 y1 x2 y2 color]
  (db-pix x1 y1 color)
  (var dx (math.abs (- x2 x1)))
  (var dy (math.abs (- y2 y1)))
  (for [x x1 dx 1]
    (var y (* (+ y1 dy) (/ (- x x1) dx)))
    (db-pix x y color)))

(fn custom-random []
  (- 1 (math.pow (math.random) 5)))

(lambda my-spiral2 [centerx
                    centery
                    radius
                    ?color
                    ?startradius
                    ?step]
  (let [spiral {:radius (or ?startradius (/ radius 10))
                :lastx (- 1)
                :lasty (- 1)}
        color (or ?color 1)]
    (for [angle 0 (* 360 (math.random 1 10)) (or ?step 8)]
      (tset spiral :radius (+ spiral.radius 0.25))
      (let [radians (math.rad angle)
            x (+ centerx (* spiral.radius (math.cos radians)))
            y (+ centery (* spiral.radius (math.sin radians)))]
        (when (> spiral.lastx (- 1))
          (line x y spiral.lastx spiral.lasty color))
        (tset spiral :lastx x)
        (tset spiral :lasty y)))))

;; working
(fn my-spiral [centerx centery radius color]
  (var startradius (/ radius 10))
  (var lastx (- 999))
  (var lasty (- 999))
  (for [angle 0 1440 5]
    (set startradius (+ startradius 0.25))
    (let [radians (math.rad angle)
          x (+ centerx (* startradius (math.cos radians)))
          y (+ centery (* startradius (math.sin radians)))]
      (when (> lastx (- 999))
        (line x y lastx lasty color))
      (set lastx x)
      (set lasty y))))


(fn my-noise-spiral [centerx centery radius color]
  (var startradius (/ radius 10))
  (var lastx (- 999))
  (var lasty (- 999))
  (var radius-noise (math.random startradius))
  (for [angle 0 (* 360 4) 8]
    (set radius-noise (+ radius-noise 0.08))
    (var thisradius (+ startradius
                       (* radius-noise
                            (- 1 (custom-random)))))
      (set startradius (+ startradius 0.2
                          (- 1 (custom-random))))
      (var radians (math.rad angle))
      (local x (+ centerx (* thisradius (math.cos radians))))
      (local y (+ centery (* thisradius (math.sin radians))))
      (when (> lastx (- 999))
        (line x y lastx lasty color))
      (set (lastx lasty)
           (values x y))))
  
(fn _G.TIC []
  ;; (my-spiral center-x center-y radius 5)
  ;; (cls 1)
  ;; (when (= 0 (% myt 4))
  ;;   (cls 1)
  ;;   (my-noise-spiral center-x center-y radius 4))
  ;; (clearBuffer)
  (when (= 0 (% myt 4))
    (cls 1)
    (my-noise-spiral center-x center-y radius 4))
  ;; (bltBuffer)
  ;; (circb center-x center-y radius 5)
  (set myt (+ myt 1)))

;; sort-of working -- blast effect
;; (fn my-noise-spiral [centerx centery radius color]
;;   (for [i 0 10]
;;     (var startradius (math.random 1 radius))
;;     (var radius-noise (math.random 10))
;;     (local startangle (math.random 0 90))
;;     (local endangle (+ (* 360 4) (math.random (* 360 4))))
;;     (local anglestep (+ 2 (math.random 1 2)))
;;     (var (lastx lasty) (values (- 999) (- 999)))
;;     (for [angle startangle endangle anglestep]
;;       (set radius-noise (+ radius-noise 0.08))
;;       (var thisradius (+ startradius (* radius-noise (- 1 (custom-random)))))
;;       (var startradius (+ startradius 0.05 (- 1 (custom-random))))
;;       (var radians (math.rad angle))
;;       (local x (+ centerx (* thisradius (math.cos radians))))
;;       (local y (+ centery (* thisradius (math.sin radians))))
;;       (when (> lastx (- 999))
;;         (line x y lastx lasty color))
;;       (set lastx x)
;;       (set lasty y))))

;; This is the big one. Wanna get the "little" one working first
;; I'll know what I'm talking about
;; (fn my-noise-spiral [centerx centery radius color]
;;   (for [i 0 10]
;;     ;; (var startradius (math.random 1 radius))
;;     ;; (var radius-noise (math.random 10))
;;     ;; (local startangle (math.random 0 90))
;;     ;; (local endangle (+ (* 360 4) (math.random (* 360 4))))
;;     ;; (local anglestep (+ 2 (math.random 1 2)))
;;     ;; (var (lastx lasty) (values (- 999) (- 999)))
;;     (var lastx (- 999))
;;     (var lasty (- 999))
;;     (var startradius (math.random 1 radius))
;;     (var radius-noise (math.random 10))
;;     (let [startangle (math.random 0 90)
;;           endangle (+ (* 360 4) (math.random (* 360 4)))
;;           anglestep (+ 2 (math.random 1 2))]
;;       (for [angle startangle endangle anglestep]
;;         (set radius-noise (+ radius-noise 0.08))
;;         (var thisradius (+ startradius (* radius-noise (- 1 (custom-random)))))
;;         (var startradius (+ startradius 0.05 (- 1 (custom-random))))
;;         (var radians (math.rad angle))
;;         (local x (+ centerx (* thisradius (math.cos radians))))
;;         (local y (+ centery (* thisradius (math.sin radians))))
;;         (when (> lastx (- 999))
;;           (line x y lastx lasty color))
;;         (set lastx x)
;;         (set lasty y)))))

;; (fn my-noise-spiral [centerx centery radius color]
;;   (var startradius (/ radius 10))
;;   (var lastx (- 999))
;;   (var lasty (- 999))
;;   (var radius-noise (math.random startradius))
;;   (for [angle 0 (* 360 4) 5]
;;     (set radius-noise (+ radius-noise 0.08))
;;     (var thisradius (+ startradius
;;                           (* radius-noise
;;                              (- 1 (custom-random)))))
;;     (set startradius (+ startradius 0.2
;;                         (- 1 (custom-random))))
;;     (var radians (math.rad angle))
;;     (local x (+ centerx (* thisradius (math.cos radians))))
;;     (local y (+ centery (* thisradius (math.sin radians))))
;;     (when (> lastx (- 999))
;;       (line x y lastx lasty color))
;;     (set (lastx lasty)
;;             (values x y))))


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

