(global t 0)

(global radius 20)

(global center-x 120)

(global center-y 75)

(global num-points 150)

(fn _G.custom-random []
  (- 1 (math.pow (math.random) 5)))

(fn _G.my-circb [centerx centery radius color]
  (for [i 0 num-points]
    (local angle (* (/ i num-points) math.pi 2))
    (local x (+ centerx (* radius (math.cos angle))))
    (local y (+ centery (* radius (math.sin angle))))
    (pix x y color)))

(fn _G.my-spiral [centerx centery radius color]
  (global startradius (/ radius 10))
  (global lastx (- 999))
  (global lasty (- 999))
  (for [angle 0 1440 5]
    (global startradius (+ startradius 0.25))
    (global radians (math.rad angle))
    (local x (+ centerx (* startradius (math.cos radians))))
    (local y (+ centery (* startradius (math.sin radians))))
    (when (> lastx (- 999)) (line x y lastx lasty color))
    (global lastx x)
    (global lasty y)))

(fn _G.my-noise-spiral [centerx centery radius color]
  (global startradius (/ radius 10))
  (global (lastx lasty) (values (- 999) (- 999)))
  (global radius-noise (math.random startradius))
  (for [angle 0 (* 360 4) 5]
    (global radius-noise (+ radius-noise 0.08))
    (global thisradius (+ startradius (* radius-noise (- 1 (custom-random)))))
    (global startradius (+ startradius 0.2 (- 1 (custom-random))))
    (global radians (math.rad angle))
    (local x (+ centerx (* thisradius (math.cos radians))))
    (local y (+ centery (* thisradius (math.sin radians))))
    (when (> lastx (- 999)) (line x y lastx lasty color))
    (global (lastx lasty) (values x y))))

(fn _G.TIC [] (cls 1) (my-noise-spiral center-x center-y radius 4))

