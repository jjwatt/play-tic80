(local WIDTH 240)
(local HEIGHT 136)
(local SCREEN_SIZE (* WIDTH HEIGHT))
(var VRAM_ADDR 0x0000)
(var OFFSCREEN_ADDR 0x8000) ;; Start of free RAM

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
(fn _G.TIC []
  ;; (my-spiral center-x center-y radius 5)
  ;; (cls 1)
  ;; (when (= 0 (% myt 4))
  ;;   (cls 1)
  ;;   (my-noise-spiral center-x center-y radius 4))
  (clearBuffer)
  (db-pix 1 1 4)
  (bltBuffer)
  ;; (circb center-x center-y radius 5)
  (set myt (+ myt 1)))
