(import
  pytest
  simalq.geometry [Map Pos NORTH EAST WEST SOUTH NORTHEAST pos+ GeometryError])
(setv  T True  F False)


(defn test-map []
  (setv  w 5  h 3)
  (setv m (Map :wrap-x F :wrap-y F :data
    (lfor
      x (range w)
      (lfor
        y (range h)
        (+ (* x 10) y)))))

  (assert (= m.width w))
  (assert (= m.height h))
  (assert (= (get m.data 0 0) 0))
  (assert (= (get m.data 0 2) 2))
  (assert (= (get m.data 4 0) 40))
  (assert (= (get m.data 4 2) 42))
  (with [(pytest.raises IndexError)]
    (get m.data m.width 0))
  (with [(pytest.raises IndexError)]
    (get m.data 0 m.height)))


(defn test-pos []
  (setv m (Map :data (* [(* [T] 3)] 3) :wrap-x F :wrap-y F))
  (Pos m 0 0)
  (Pos m 0 1)
  (Pos m 0 2)
  (with [(pytest.raises GeometryError)]
    (Pos m 0 3))
  (with [(pytest.raises GeometryError)]
    (Pos m 0 -1)))


(defn test-pos+ []
  (setv map-data (* [(* [T] 5)] 5))

  (setv m (Map :data map-data :wrap-x F :wrap-y F))
  (assert (=
    (pos+ (Pos m 0 0) NORTH)
    (Pos m 0 1)))
  (assert (=
    (pos+ (Pos m 2 2) WEST)
    (Pos m 1 2)))
  (assert (=
    (pos+ (Pos m 0 0) NORTHEAST)
    (Pos m 1 1)))
  (with [(pytest.raises GeometryError)]
    (pos+ (Pos m 0 0) WEST))

  (setv m (Map :data map-data :wrap-x T :wrap-y F))
  (assert (=
    (pos+ (Pos m 0 0) WEST)
    (Pos m 4 0)))
  (assert (=
    (pos+ (Pos m 4 0) EAST)
    (Pos m 0 0)))
  (with [(pytest.raises GeometryError)]
    (pos+ (Pos m 0 0) SOUTH)))
