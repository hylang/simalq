(import
  pytest
  simalq.game-state [G]
  simalq.geometry [Pos at]
  tests.lib [init-boot-camp])


(defn test-no-new-attrs []
  "You can't accidentally create new attributes on most instances."

  (init-boot-camp)

  ; Global
  (with [(pytest.raises AttributeError)]
    (setv G.foo 7))
  ; GameState
  (assert (is-not G.state None))
  (with [(pytest.raises AttributeError)]
    (setv G.state.foo 7))
  ; Rules
  (assert (is-not G.rules None))
  (with [(pytest.raises AttributeError)]
    (setv G.rules.foo 7))
  ; Player
  (assert (is-not G.player None))
  (with [(pytest.raises AttributeError)]
    (setv G.player.foo 7))
  ; wall
  (setv [t] (at (Pos G.map 0 5)))
  (assert (= t.stem "wall"))
  (with [(pytest.raises AttributeError)]
    (setv t.foo 7)))


(defn test-tilepedia []
  "Just check that calling all these info methods doesn't crash."
  (assert (isinstance (hy.I.simalq/tile/tilepedia.get-info) dict)))
