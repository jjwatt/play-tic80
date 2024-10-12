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
