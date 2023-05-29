(require
  hyrule [unless do-n]
  simalq.macros [defdataclass])
(import
  itertools [chain]
  toolz [unique]
  simalq.util [seq sign]
  simalq.game-state [G])
(setv  T True  F False)


(defdataclass Map []
  "A level layout."

  [wrap-x wrap-y data]
    ; `wrap-x` and `wrap-y` are Booleans.
    ; `data` is a tuple of tuples representing the squares of the map.
    ; Each tile is itself a list representing a stack of tiles on
    ; that square. An empty stack means that the tile has only floor.
  :frozen T :eq F

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
  (setv arrows
    ["↑"     "→"    "↓"     "←"])
  (for [d Direction.orths]
    (setattr Direction (.upper (get d.name 0)) d))
  (setv Direction.arrows (dict (zip Direction.orths arrows)))
  (setv Direction.diags (tuple (gfor
    d1 [Direction.N Direction.S]
    d2 [Direction.E Direction.W]
    :setv new (Direction (+ d1.name d2.name) (+ d1.x d2.x) (+ d1.y d2.y))
    :do (setattr Direction (.upper (+ (get d1.name 0) (get d2.name 0))) new)
    new)))
  (setv Direction.all #(
     Direction.N Direction.NE Direction.E Direction.SE
     Direction.S Direction.SW Direction.W Direction.NW))
  (setv Direction.from-coords (dfor
    d Direction.all
    #(d.x d.y) d))
  ; Define opposite directions.
  (setv opposites (dfor
    d1 Direction.all
    d2 Direction.all
    :if (and (= d1.x (- d2.x)) (= d1.y (- d2.y)))
    d1 d2))
  (setv Direction.opposite (property (fn [self]
    (get opposites self))))))


(defdataclass Pos []
  "A position; a point on a map."

  [map x y]
  :frozen T

  (defn __init__ [self map x y]
    (when map.wrap-x
      (%= x map.width))
    (when map.wrap-y
      (%= y map.height))
    (unless (and (<= 0 x (- map.width 1)) (<= 0 y (- map.height 1)))
      (raise (GeometryError f"Illegal position: {x}, {y}")))
    (for [[k v] (.items (dict  :map map  :x x  :y y))]
      ; Call `object.__setattr__` to bypass `dataclass`'s frozen
      ; checks.
      (object.__setattr__ self k v)))

  (defn __str__ [self]
    "Provide a concise representation, without the linked map."
    f"<Pos {self.x},{self.y}>")

  (defn __hash__ [self]
    (hash #(self.x self.y (id self.map))))

  (defn [property] xy [self]
    #(self.x self.y)))

(hy.repr-register Pos str)

(defn pos+ [pos direction]
  (try
    (Pos pos.map (+ direction.x pos.x) (+ direction.y pos.y))
    (except [GeometryError])))

(defn ray [pos direction length]
  "Return a line of `length` points in `direction` from `pos`, not
  including `pos`. If it wraps far enough that it would get to `pos`,
  it stops just before it."

  (setv out [pos])
  (do-n length
    (setv new (pos+ (get out -1) direction))
    (when (or (is new None) (= new pos))
      (break))
    (.append out new))
  (tuple (cut out 1 None)))

(defn at [pos]
  (get pos.map.data pos.x pos.y))

(defn adjacent? [p1 p2]
  (= (dist p1 p2) 1))

(defn dist [p1 p2]
  "Chebyshev distance as the crow flies between the given positions,
  accounting for the possibilty of wrapping."
  (setv m p1.map)
  (unless (is p2.map m)
    (raise (ValueError "Tried to compute a distance between maps")))
  (setv dx (abs (- p1.x p2.x)))
  (when m.wrap-x
    (setv dx (min dx (- m.width dx))))
  (setv dy (abs (- p1.y p2.y)))
  (when m.wrap-y
    (setv dy (min dy (- m.height dy))))
  (max dx dy))

(defn dir-to [p1 p2]
  "The most logical direction for a first step from `p1` to `p2`. If
  `p2` is the same distance walking with or without wrapping, then the
  preference is not to wrap."
  (setv m p1.map)
  (unless (is p2.map m)
    (raise (ValueError "Tried to find a direction between maps")))
  (when (= p1 p2)
    (return None))
  (setv dx (- p2.x p1.x))
  (when (and m.wrap-x (> (abs dx) (/ m.width 2)))
    (*= dx -1))
  (setv dy (- p2.y p1.y))
  (when (and m.wrap-y (> (abs dy) (/ m.height 2)))
    (*= dy -1))
  (get Direction.from-coords #((sign dx) (sign dy))))

(defn pos-seed [pos]
  "Using a `Pos`, get a number you could use as an RNG seed. Nearby
  `Pos`es should return different values."
  (+
    (* G.level-n 1,000,003)
      ; The multiplier is chosen to be (a) prime and (b) bigger
      ; than the area of most levels.
    pos.x
    (* G.map.width pos.y)))


(defn burst [center size [exclude-center False]]
  "Return a generator of all distinct points within distance `size` of
  `center`. Thus the points form a square that's `2 * size + 1`
  squares wide. The order in which they're generated spirals outwards
  like this (with size = 2):

      21 20 19 18 17
      22  7  6  5 16
      23  8  0  4 15
      24  1  2  3 14
       9 10 11 12 13

  This follows `SpiralX` and `SpiralY` in IQ (but upside-down). An
  important property of it is that activating monsters in this order
  allows monsters closer to the player to move first, so a line of
  monsters can march toward the player without creating gaps.

  If `exclude-center` is true, the center position isn't returned."

  (unique (gfor
    c (seq 0 (min size (max center.map.width center.map.height)))
    [x y] (py "chain(
      (( x, -c) for x in seq(    -c,      c,  1)),
      (( c,  y) for y in seq(-c + 1,      c,  1)),
      (( x,  c) for x in seq( c - 1,     -c, -1)),
      ((-c,  y) for y in seq( c - 1, -c + 1, -1)))")
    :setv p (try
      (Pos center.map (+ center.x x) (+ center.y y))
      (except [GeometryError]))
    :if (and p (not (and exclude-center (= p center))))
    p)))


(defclass GeometryError [Exception])
