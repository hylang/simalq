(import
  pytest
  tests.lib [init]
  simalq.geometry [at Pos]
  simalq.game-state [G])


(defn test-unmodifiable-tiles []
  (init "Boot Camp 2")

  (setv [t] (at (Pos G.map 13 5)))
  (assert (= t.stem "locked door"))
  (with [(pytest.raises AttributeError)]
    (setv t.fooey 5))
  (with [(pytest.raises AttributeError)]
    (setv t.pos (Pos G.map 11 6))))
