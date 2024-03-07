"Test types and functions for map geometry."


(require
  hyrule [ebranch])
(import
  copy [deepcopy]
  hyrule [thru]
  pytest
  simalq.geometry [
    Map Pos Direction ray adjacent? dir-to dist burst
    GeometryError])
(setv  T True  F False)


(defn test-map []
  (setv  w 5  h 3)
  (setv m (Map.from-data :wrap-x F :wrap-y F :data
    (tuple (gfor
      x (range w)
      (tuple (gfor
        y (range h)
        [(+ (* x 10) y)]))))))

  (assert (= m.width w))
  (assert (= m.height h))
  (assert (= (get m.data 0 0) [0]))
  (assert (= (get m.data 0 2) [2]))
  (assert (= (get m.data 4 0) [40]))
  (assert (= (get m.data 4 2) [42]))
  (with [(pytest.raises IndexError)]
    (get m.data m.width 0))
  (with [(pytest.raises IndexError)]
    (get m.data 0 m.height))

  (setv m2 (deepcopy m))
  (assert (!= m m2)))


(defn test-pos []
  (defn example-map []
    (Map.from-data :data (* #((* #([T]) 3)) 3) :wrap-x F :wrap-y F))

  (setv m (example-map))
  (Pos :map m :x 0 :y 0)
  (Pos m 0 1)
  (Pos m 0 2)
  (with [(pytest.raises GeometryError)]
    (Pos m 0 3))
  (with [(pytest.raises GeometryError)]
    (Pos m 0 -1))

  (assert (= (Pos m 0 2) (Pos m 0 2)))
  (assert (!= (Pos m 0 2) (Pos m 2 0)))
  (assert (=
    (get {(Pos m 0 2) "a"  (Pos m 2 0) "b"} (Pos m 2 0))
    "b"))

  (assert (= (str (Pos m 0 2)) "<Pos 0,2>"))

  (setv m2 (example-map))
  (assert (!= m m2))
  (assert (!= (Pos m 0 1) (Pos m2 0 1))))


(defn test-pos-add []
  (setv map-data (* #((* #([T]) 5)) 5))

  (setv m (Map.from-data :data map-data :wrap-x F :wrap-y F))
  (assert (=
    (+ (Pos m 0 0) Direction.N)
    (Pos m 0 1)))
  (assert (=
    (+ (Pos m 2 2) Direction.W)
    (Pos m 1 2)))
  (assert (=
    (+ (Pos m 0 0) Direction.NE)
    (Pos m 1 1)))
  (assert (is
    (+ (Pos m 0 0) Direction.W)
    None))

  (setv m (Map.from-data :data map-data :wrap-x T :wrap-y F))
  (assert (=
    (+ (Pos m 0 0) Direction.W)
    (Pos m 4 0)))
  (assert (=
    (+ (Pos m 4 0) Direction.E)
    (Pos m 0 0)))
  (assert (is
    (+ (Pos m 0 0) Direction.S)
    None)))


(defn test-ray []
  (defn r [direction length]
    (lfor  p (ray p0 direction length)  c p.xy  c))

  (setv p0 (Pos :x 3 :y 2
    :map (Map.make :wrap-x F :wrap-y F :width 10 :height 10)))

  (assert (= (r Direction.N 0)
    []))
  (assert (= (r Direction.N 1)
    [3 3]))
  (assert (= (r Direction.N 4)
    [3 3  3 4  3 5  3 6]))
  (assert (= (r Direction.NE 4)
    [4 3  5 4  6 5  7 6]))
  (assert (= (r Direction.S 4)
    [3 1  3 0]))

  (setv p0 (Pos :x 3 :y 2
    :map (Map.make :wrap-x T :wrap-y F :width 10 :height 10)))
  (assert (= (r Direction.W 20)
    [2 2  1 2  0 2  9 2  8 2  7 2  6 2  5 2  4 2]))

  (assert (= (r Direction.NW Inf)
    [2 3  1 4  0 5  9 6  8 7  7 8  6 9])))


(defn test-direction []
  (assert (= Direction.N.opposite Direction.S))
  (assert (= Direction.SW.opposite Direction.NE))
  (assert (= Direction.SW.abbr "SW"))
  (assert (= Direction.SW.name "southwest"))
  (assert (= (len Direction.all) 8)))


(defn test-adjacent? []
  (defn all-adjacencies [[wrap-x F] [wrap-y F]]
    "Get all adjacencies from (0, 0)."
    (setv m (Map.make :wrap-x wrap-x :wrap-y wrap-y :width 4 :height 5))
    (lfor
      y (reversed (range m.height))
      x (range m.width)
      (adjacent? (Pos m 0 0) (Pos m x y))))

  (assert (= (all-adjacencies) [
    0 0 0 0
    0 0 0 0
    0 0 0 0
    1 1 0 0
    0 1 0 0]))
  (assert (= (all-adjacencies :wrap-x T) [
    0 0 0 0
    0 0 0 0
    0 0 0 0
    1 1 0 1
    0 1 0 1]))
  (assert (= (all-adjacencies :wrap-y T) [
    1 1 0 0
    0 0 0 0
    0 0 0 0
    1 1 0 0
    0 1 0 0]))
  (assert (= (all-adjacencies :wrap-x T :wrap-y T) [
    1 1 0 1
    0 0 0 0
    0 0 0 0
    1 1 0 1
    0 1 0 1])))


(defn test-dir-to []
  (defn all-dirs [[wrap-x F] [wrap-y F]]
    "Get all directions from (1, 1)."
    (setv m (Map.make :wrap-x wrap-x :wrap-y wrap-y :width 5 :height 6))
    (lfor
      y (reversed (range m.height))
      x (range m.width)
      (dir-to (Pos m 1 1) (Pos m x y))))
  (defmacro dirs [#* abbreviations]
    (hy.models.List (gfor
      a abbreviations
      (if (= a '0) None `(. Direction ~a)))))

  (assert (= (all-dirs) (dirs
    NW  N NE NE NE
    NW  N NE NE NE
    NW  N NE NE NE
    NW  N NE NE NE
     W  0  E  E  E
    SW  S SE SE SE)))
  (assert (= (all-dirs :wrap-x T) (dirs
    NW  N NE NE NW
    NW  N NE NE NW
    NW  N NE NE NW
    NW  N NE NE NW
     W  0  E  E  W
    SW  S SE SE SW)))
  (assert (= (all-dirs :wrap-y T) (dirs
    SW  S SE SE SE
    NW  N NE NE NE
    NW  N NE NE NE
    NW  N NE NE NE
     W  0  E  E  E
    SW  S SE SE SE)))

  (assert (= (all-dirs :wrap-x T :wrap-y T) (dirs
    SW  S SE SE SW
    NW  N NE NE NW
    NW  N NE NE NW
    NW  N NE NE NW
     W  0  E  E  W
    SW  S SE SE SW))))


(defn test-dist []
  (setv d 7)
  (defn all-dists [[wrap-x F] [wrap-y F]]
    "Get all distances from (1, 1)."
    (setv m (Map.make :wrap-x wrap-x :wrap-y wrap-y :width d :height d))
    (lfor
      y (reversed (range m.height))
      x (range m.width)
      (dist (Pos m 1 1) (Pos m x y))))

  (assert (= (all-dists) [
    5 5 5 5 5 5 5
    4 4 4 4 4 4 5
    3 3 3 3 3 4 5
    2 2 2 2 3 4 5
    1 1 1 2 3 4 5
    1 0 1 2 3 4 5
    1 1 1 2 3 4 5]))
  (assert (= (all-dists :wrap-x T) [
    5 5 5 5 5 5 5
    4 4 4 4 4 4 4
    3 3 3 3 3 3 3
    2 2 2 2 3 3 2
    1 1 1 2 3 3 2
    1 0 1 2 3 3 2
    1 1 1 2 3 3 2]))
  (assert (= (all-dists :wrap-y T) [
    2 2 2 2 3 4 5
    3 3 3 3 3 4 5
    3 3 3 3 3 4 5
    2 2 2 2 3 4 5
    1 1 1 2 3 4 5
    1 0 1 2 3 4 5
    1 1 1 2 3 4 5]))
  (assert (= (all-dists :wrap-x T :wrap-y T) [
    2 2 2 2 3 3 2
    3 3 3 3 3 3 3
    3 3 3 3 3 3 3
    2 2 2 2 3 3 2
    1 1 1 2 3 3 2
    1 0 1 2 3 3 2
    1 1 1 2 3 3 2])))


(defn test-burst []
  (for [size [2 6]  exclude-center [F T]  wrap-x [F T]  wrap-y [F T]]
    (setv m (Map.make :width 5 :height 7
      :wrap-x wrap-x :wrap-y wrap-y))
    (setv p0 (Pos m 1 1))
    (setv b (tuple (burst p0 size exclude-center)))
    ; A burst shouldn't duplicate positions.
    (assert (= (len b) (len (set b))))
    ; A point should be in a burst if and only if it's close enough to
    ; the center.
    (for [x (range m.width)  y (range m.height)]
      (setv p (Pos m x y))
      (assert (=
        (if (= p p0) (not exclude-center) (<= (dist p p0) size))
        (in p b))))))


(defn test-burst-spiral []
  "`burst` should return points in IQ's spiral order (but actually
  upside-down, since our y-coordinates have the opposite meaning)."

  (setv iq-spiral-size 6)
  (setv iq-spiral (list (zip
    ; Translated directly from `SetSpiralX` and `SetSpiralY` in IQ.
    (gfor  i (thru 168)  (ebranch (in i it)
      #(121 157 158 159 160 161 162 163 164 165 166 167 168)  -6
      #(81 122 156 111 112 113 114 115 116 117 118 119 120)  -5
      #(49 73 74 75 76 77 78 79 80 82 110 123 155)  -4
      #(25 43 44 45 46 47 48 50 72 83 109 124 154)  -3
      #(9 21 22 23 24 26 42 51 71 84 108 125 153)  -2
      #(1 7 8 10 20 27 41 52 70 85 107 126 152)  -1
      #(0 2 6 11 19 28 40 53 69 86 106 127 151)  0
      #(3 4 5 12 18 29 39 54 68 87 105 128 150)  1
      #(13 14 15 16 17 30 38 55 67 88 104 129 149)  2
      #(31 32 33 34 35 36 37 56 66 89 103 130 148)  3
      #(57 58 59 60 61 62 63 64 65 90 102 131 147)  4
      #(91 92 93 94 95 96 97 98 99 100 101 132 146)  5
      #(133 134 135 136 137 138 139 140 141 142 143 144 145)  6))
    (gfor  i (thru 168) (ebranch (in i it)
      #(145 146 147 148 149 150 151 152 153 154 155 156 157)  6
      #(101 102 103 104 105 106 107 108 109 110 111 144 158)  5
      #(65 66 67 68 69 70 71 72 73 100 112 143 159)  4
      #(37 38 39 40 41 42 43 64 74 99 113 142 160)  3
      #(17 18 19 20 21 36 44 63 75 98 114 141 161)  2
      #(5 6 7 16 22 35 45 62 76 97 115 140 162)  1
      #(0 4 8 15 23 34 46 61 77 96 116 139 163)  0
      #(1 2 3 14 24 33 47 60 78 95 117 138 164)  -1
      #(9 10 11 12 13 32 48 59 79 94 118 137 165)  -2
      #(25 26 27 28 29 30 31 58 80 93 119 136 166)  -3
      #(49 50 51 52 53 54 55 56 57 92 120 135 167)  -4
      #(81 82 83 84 85 86 87 88 89 90 91 134 168)  -5
      #(121 122 123 124 125 126 127 128 129 130 131 132 133)  -6)))))

  (setv m (Map.make :width 20 :height 20 :wrap-x F :wrap-y F))
  (setv p0 (Pos m 10 10))
  (setv sq-spiral (lfor
    p (burst p0 iq-spiral-size)
    #((- p.x p0.x) (- p.y p0.y))))

  (assert (= sq-spiral iq-spiral)))


(defn test-burst-infinite []
  (setv m (Map.make :width 20 :height 20 :wrap-x T :wrap-y T))
  (setv b (list (burst (Pos m 4 7) Inf)))
  (assert (= (len b) (* 20 20))))
