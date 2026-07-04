;; title:   Fireworks
;; author:  jwatt@broken.watch
;; desc:    Demoscene-style exploding fireworks
;; site:    website link
;; license: GPL3
;; version: 0.1
;; script:  fennel
;; strict:  true

(local screen-width 240)
(local screen-height 136)
(local gravity 0.05)
(local friction 0.98)

;; Particle pool
(local particles [])

(var text-exploded? false)
(var frame-count 0)

(fn explode-text [str start-x start-y text-color]
  "Scans the text directly from the active frame buffer."
  ;; Scan the exact 100-pixel wide bounding box where the text lives
  (for [y start-y (+ start-y 10)]
    (for [x start-x (+ start-x 100)]
      (if (= (pix x y) text-color)
          (let [center-x 120
                dx (- x center-x)
                ;; Radial blast outward from center
                vx (+ (* dx 0.06) (* (- (math.random) 0.5) 0.4))
                vy (- (* (math.random) 1.6) 0.4)]
            (table.insert particles {: x 
                                     : y 
                                     : vx 
                                     : vy 
                                     :life (+ 45 (math.random 35)) 
                                     :color text-color}))))))
(fn spawn-firework [cx cy color]
  "Spawns a ring of particles at a center point with random velocities."
  (let [count 60]
    (for [i 1 count]
      (let [angle (* (/ i count) (* 2 math.pi))
            speed (+ 1 (math.random))
            vx (* (math.cos angle) speed)
            vy (* (math.sin angle) speed)]
        (table.insert particles {:x cx
                                 :y cy
                                 :vx vx
                                 :vy vy
                                 :life (+ 40 (math.random 30))
                                 :color color})))))

(fn update-particles []
  "Updates positions, applies gravity/friction, and filters out dead particles."
  (for [i (length particles) 1 -1]
    (let [p (. particles i)]
      (set p.x (+ p.x p.vx))
      (set p.y (+ p.y p.vy))
      (set p.vy (+ p.vy gravity))
      (set p.vx (* p.vx friction))
      (set p.vy (* p.vy friction))
      (set p.life (- p.life 1))

      ;; Remove if life expires or goes off-screen
      (if (or (<= p.life 0)
              (< p.x 0) (> p.x screen-width)
              (< p.y 0) (> p.y screen-height))
          (table.remove particles i)))))

(fn draw-particles []
  "Renders each particle, dimming its color index as its life expires."
  (each [_ p (ipairs particles)]
    (let [;; Calculate a lifetime ratio between 0.0 and 1.0
          ;; (Assuming a max life of ~70 based on spawn-firework)
          life-ratio (/ p.life 70)
          
          ;; Shift the color down if life is running low.
          ;; If ratio is high, keep the bright original color.
          ;; If ratio is low, drop the color index toward darker values.
          final-color (if (< life-ratio 0.3)
                          (math.max 1 (math.min p.color 2))  ; Fade to dark blue/dark red
                          (< life-ratio 0.6)
                          (math.max 1 (- p.color 2))         ; Mid-life dimming
                          p.color)]                          ; Fresh and bright
      
      (pix p.x p.y final-color))))

(fn simulate-crt-trails []
  "Fades out alternating rows and instantly clears the scanline rows
   to prevent permanent pixel artifacts."
  (for [addr 0x0000 0x3FC0]
    (if (< (% addr 240) 120)
        (let [byte (peek addr)]
          (if (> byte 0)
              (let [p1 (rshift byte 4)
                    p2 (band byte 0x0F)

                    p1-new (math.max 0 (- p1 1))
                    p2-new (math.max 0 (- p2 1))

                    new-byte (bor (lshift p1-new 4) p2-new)]
                (poke addr new-byte))))
        (poke addr 0))))

;; Initialize with a couple of active fireworks
(spawn-firework 120 40 11)

;; Main TIC-80 Loop
(fn _G.TIC []
  ;; Disable mouse cursor
  (poke 0x3FFB 0)
  (set frame-count (+ frame-count 1))

  (simulate-crt-trails)

  (if (not text-exploded?)
      (do
        (print "Happy July 4th" 78 60 12 true 1)
        (if (< frame-count 60)
            ;; Phase 1: Draw the text normally for 60 frames
            (print "Happy July 4th" 78 60 12 true 1)
            
            ;; Phase 2: Hit frame 60, explode it once, flip the switch
            (do
              (explode-text "Happy July 4th" 60 60 12)
              (set text-exploded? true))))
      
      ;; Phase 3: Text has shattered, run standard firework ambient loop
      (if (= (math.random 1 30) 1)
          (spawn-firework (math.random 40 200) (math.random 20 60) (math.random 1 15))))

  (update-particles)
  (draw-particles))

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
