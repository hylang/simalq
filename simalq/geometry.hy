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
  :frozen T)

(defn pos+ [pos direction]
  (setv  m pos.map  x (+ direction.x pos.x)  y (+ direction.y pos.y))
  (when m.wrap-x
    (%= x m.width))
  (when m.wrap-y
    (%= y m.height))
  (unless (and (<= 0 x (- m.width 1)) (<= 0 y (- m.height 1)))
    (raise (GeometryError f"Illegal pos+: ({pos.x}, {pos.y}) {direction}")))
  (Pos m x y))


(defclass GeometryError [Exception])
