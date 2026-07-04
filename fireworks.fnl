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
  "Renders each particle as a pixel."
  (each [_ p (ipairs particles)]
    ;; Dim the color slightly near the end of life if desired,
    ;; or just draw the raw particle color.
    (pix p.x p.y p.color)))

(fn simulate-trails []
  "Demoscene trick: Loops through screen VRAM (0x0000 to 0x3FC0) 
   and safely fades pairs of 4-bit pixels without color bleeding."
  (for [addr 0x0000 0x3FC0]
    (let [byte (peek addr)]
      (if (> byte 0)
          (let [;; Extract pixels (high and low 4-bit nibbles)
                p1 (rshift byte 4)          ; Left pixel
                p2 (band byte 0x0F)     ; Right pixel
                
                ;; Decrement color indices toward 0 (black)
                p1-new (math.max 0 (- p1 1))
                p2-new (math.max 0 (- p2 1))
                
                ;; Pack them back into a single 8-bit byte
                new-byte (bor (lshift p1-new 4) p2-new)]
            (poke addr new-byte))))))

;; Initialize with a couple of active fireworks
(spawn-firework 120 40 11)

;; Main TIC-80 Loop
(fn _G.TIC []
  ;; Option A: Crisp clean background
  ;; (cls 0)
  
  ;; Option B: Uncomment line below and comment out (cls 0) for demoscene trails!
  (simulate-trails)

  ;; Randomly launch new fireworks
  (if (= (math.random 1 45) 1)
      (spawn-firework (math.random 40 200) 
                      (math.random 30 70) 
                      (math.random 1 15)))

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

