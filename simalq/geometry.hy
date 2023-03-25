(require
  hyrule [unless]
  simalq.macros [defdataclass])
(setv  T True  F False)


(defdataclass Map []
  [wrap-x wrap-y data]
    ; `wrap-x` and `wrap-y` are Booleans.
    ; `data` is a tuple of tuples representing the squares of the map.
    ; Each tile is itself a list representing a stack of tiles on
    ; that square. An empty stack means that the tile has only floor.
  :frozen T

  (defn [classmethod] make [self wrap-x wrap-y width height]
    "Create a new blank map."
    (Map
      wrap-x
      wrap-y
      (tuple (gfor
        _ (range width)
        (tuple (gfor  _ (range height)  []))))))

  (defn [property] width [self]
    (len self.data))
  (defn [property] height [self]
    (len (get self.data 0))))


(defdataclass Direction []
  [name x y]
  :frozen T)
((fn []
  ; Define the direction constants (`Direction.N`, `.NE`, etc.)
  ; and collections thereof (`Direction.orths`, `.diags`, `.all`).
  (setv Direction.orths (tuple (map Direction
    ["north" "east" "south" "west"]
    [0       1       0      -1]
    [1       0      -1       0])))
  (for [d Direction.orths]
    (setattr Direction (.upper (get d.name 0)) d))
  (setv Direction.diags (tuple (gfor
    d1 [Direction.N Direction.S]
    d2 [Direction.E Direction.W]
    :setv new (Direction (+ d1.name d2.name) (+ d1.x d2.x) (+ d1.y d2.y))
    :do (setattr Direction (.upper (+ (get d1.name 0) (get d2.name 0))) new)
    new)))
  (setv Direction.all (+ Direction.orths Direction.diags))
  ; Define opposite directions.
  (setv opposites (dfor
    d1 Direction.all
    d2 Direction.all
    :if (and (= d1.x (- d2.x)) (= d1.y (- d2.y)))
    d1 d2))
  (setv Direction.opposite (property (fn [self]
    (get opposites self))))))


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

(defn at [pos]
  (get pos.map.data pos.x pos.y))


(defclass GeometryError [Exception])
