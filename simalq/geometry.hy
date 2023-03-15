(require
  hyrule [unless]
  simalq.macros [defdataclass])
(setv  T True  F False)


(defdataclass Map []
  [wrap-x wrap-y data]
  :frozen T

  (defn [property] width [self]
    (len self.data))
  (defn [property] height [self]
    (len (get self.data 0))))


(defdataclass Direction []
  [name x y]
  :frozen T)

(for [[d [x y]] (zip
    (.split "north east  south  west   northeast southeast southwest northwest")
    [        [0 1] [1 0] [0 -1] [-1 0] [1 1]     [1 -1]    [-1 -1]   [1 -1]])]
  (setv (get (globals) (.upper d)) (Direction d x y)))


(defdataclass Pos []
  [map x y]
  :frozen T

  (defn __init__ [self m x y]
    (when m.wrap-x
      (%= x m.width))
    (when m.wrap-y
      (%= y m.height))
    (unless (and (<= 0 x (- m.width 1)) (<= 0 y (- m.height 1)))
      (raise (GeometryError f"Illegal position: {x}, {y}")))
    (for [[k v] (.items (dict  :map m  :x x  :y y))]
      ; Call `object.__setattr__` to bypass `dataclass`'s frozen
      ; checks.
      (object.__setattr__ self k v))))

(defn pos+ [pos direction]
  (Pos pos.map (+ direction.x pos.x) (+ direction.y pos.y)))


(defclass GeometryError [Exception])
