(require
  tests.lib [cant wk])
(import
  pytest
  tests.lib [init set-square]
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


(defn test-one-way-door []
  (init "Boot Camp 2")

  (setv G.player-pos (Pos G.map 3 13))
  (cant (wk S) "That one-way door must be entered from the east.")
  (wk W)
  (set-square 'S)
    ; We remove a wall so that stepping into these one-way doors
    ; diagonally won't be blocked by it.
  (cant (wk SE) "That one-way door must be entered from the east.")
  (cant (wk SW) "That one-way door must be entered from the north.")
  (wk W)
  (wk S) ; Now we're on the door.
  (cant (wk N) "You can only go south from this one-way door.")
  (wk S)
  (cant (wk N) "That one-way door must be entered from the north."))
