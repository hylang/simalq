(import
  copy [deepcopy]
  pytest
  simalq.geometry [Map Pos Direction pos+ adjacent? dist GeometryError])
(setv  T True  F False)


(defn test-map []
  (setv  w 5  h 3)
  (setv m (Map :wrap-x F :wrap-y F :data
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
    (Map :data (* #((* #([T]) 3)) 3) :wrap-x F :wrap-y F))

  (setv m (example-map))
  (Pos m 0 0)
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


(defn test-pos+ []
  (setv map-data (* #((* #([T]) 5)) 5))

  (setv m (Map :data map-data :wrap-x F :wrap-y F))
  (assert (=
    (pos+ (Pos m 0 0) Direction.N)
    (Pos m 0 1)))
  (assert (=
    (pos+ (Pos m 2 2) Direction.W)
    (Pos m 1 2)))
  (assert (=
    (pos+ (Pos m 0 0) Direction.NE)
    (Pos m 1 1)))
  (with [(pytest.raises GeometryError)]
    (pos+ (Pos m 0 0) Direction.W))

  (setv m (Map :data map-data :wrap-x T :wrap-y F))
  (assert (=
    (pos+ (Pos m 0 0) Direction.W)
    (Pos m 4 0)))
  (assert (=
    (pos+ (Pos m 4 0) Direction.E)
    (Pos m 0 0)))
  (with [(pytest.raises GeometryError)]
    (pos+ (Pos m 0 0) Direction.S)))


(defn test-direction []
  (assert (= Direction.N.opposite Direction.S))
  (assert (= Direction.SW.opposite Direction.NE))
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
