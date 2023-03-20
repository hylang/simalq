(import
  pytest
  simalq.game-state [G]
  simalq.geometry [Pos at NORTH EAST SOUTH WEST NORTHEAST]
  simalq.un-iq [read-quest iq-quest]
  simalq.player-actions [do-action Move ActionError]
  simalq.main [start-quest])


(defmacro cant [form msg-check]
  (setv e (hy.gensym))
  `(do
    (with [~e (pytest.raises ActionError)]
      ~form)
    (assert (in ~msg-check (. ~e value args [0])))))



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
  (cant (mv WEST) "The border of the dungeon blocks your movement.")
  ; Try walking into a wall tile.
  (mv SOUTH)
  (cant (mv SOUTH) "Your way is blocked.")
  ; Walk into the (plain) door to the east.
  (mv NORTHEAST)
  (for [_ (range 3)]
    (mv EAST))
  (assert (= (. (at G.player-pos) [0] stem) "door"))
  ; Try walking diagonally past the wall to the north.
  (cant (mv NORTHEAST) "That diagonal is blocked by a neighbor."))
