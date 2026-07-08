;; title:   Scanline Text Shatter
;; author:  jwatt@broken.watch
;; site:    jjwatt/play-tic80
;; license: GPL3
;; version: 0.1
;; script:  fennel
;; strict:  true

(local text "Hello Scene")
(local text-x 60)
(local text-y 60)
(local text-w 200)
(local text-h 8)

(fn _G.TIC []
  (cls 0)
  (let [t (/ (time) 1000)
        shatter-amount (math.max 0 (- t 1.5))]
    (if (= shatter-amount 0)
        (print text text-x text-y 14 false 2)
        (for [row 0 (- text-h 1)]
          (let [wave (math.sin (+ (* row 0.8) (* t 10)))
                dx (* wave shatter-amount 15)
                dy (* text-y row (* shatter-amount row 0.5))]
            (clip text-x dy text-w 1)
            (print text (+ text-x dx) text-y 14 false 2))))
    (clip)))

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

