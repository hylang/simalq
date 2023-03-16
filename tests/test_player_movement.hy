(import
  pytest
  simalq.game-state [G]
  simalq.geometry [Pos NORTH EAST SOUTH WEST]
  simalq.un-iq [read-quest iq-quest]
  simalq.player-actions [do-action Move ActionError]
  simalq.main [start-quest])


(defmacro cant [#* body]
  `(with [(pytest.raises ActionError)]
    ~@body))


(defn test-bootcamp-level1 []
  (start-quest (read-quest (iq-quest "Boot Camp 2")))
  (assert (= G.level-n 1))

  (defn mv [direction]
    (do-action (Move direction)))

  ; We start at the extreme northwest.
  (assert (= G.player-pos (Pos G.map 0 15)))
  ; Walk south 1 step.
  (mv SOUTH)
  (assert (= G.player-pos (Pos G.map 0 14)))
  ; Try going west, bumping into the level border.
  (cant (mv WEST)))
