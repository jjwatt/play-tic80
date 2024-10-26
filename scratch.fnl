;; normalize
(fn norm [value low high]
  (/ (- value low) (- high low)))

;; linear interpolation
(fn lerp [low high amt]
  (+ low (* amt (- high low))))

(fn mapvalue [value low1 high1 low2 high2]
  "Map from one range of values to another."
  (let [normalized (norm value low1 high1)
        remapped (lerp low2 high2 normalized)]
    remapped))

(global WIDTH 240)
(global HEIGHT 136)
(global vertices [[0 0]
                  [(/ WIDTH 2) HEIGHT]
                  [WIDTH 0]])

(fn generate-point [lastx lasty vertices]
  "Generate a new point halfway between current point and random vertex"
  (let [j (math.random 1 3)
        x (/ (+ lastx (. vertices j 1)) 2)
        y (/ (+ lasty (. vertices j 2)) 2)]
    (values x y)))
(fn generate-points [x y vertices count points]
  "Recursively generate points for Sierpinski triangle"
  ;; stopping condition
  (if (= 0 count)
      points
      ;; else
      (let [(new-x new-y) (generate-point x y vertices)]
        (tset points count [new-x new-y])
        (generate-points new-x new-y vertices (- count 1) points))))

(generate-points 50 50 vertices 20 {})

(fn rotate-point [x y cx cy angle]
  "Rotate point by angle in radians."
  ;; Translate point to origin.
  (let [translated-x (- x cx)
        translated-y (- y cy)
        rotated-x (- (* translated-x (math.cos angle))
                     (* translated-y (math.sin angle)))
        rotated-y (- (* translated-x (math.sin angle))
                     (* translated-y (math.cos angle)))
        new-x (+ rotated-x cx)
        new-y (+ rotated-y cy)]
    (values new-x new-y)))
