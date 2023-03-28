(require
  tests.lib [wk])
(import
  pytest
  tests.lib [init]
  simalq.game-state [G]
  simalq.geometry [Pos Direction pos+ at]
  simalq.player-actions [ActionError])


(defmacro cant [form msg-check]
  (setv e (hy.gensym))
  `(do
    (with [~e (pytest.raises ActionError)]
      ~form)
    (assert (in ~msg-check (. ~e value args [0])))))


(defn test-bootcamp-level1 []
  (init "Boot Camp 2")
  (assert (= G.level-n 1))

  ; We start at the extreme northwest.
  (assert (= G.player-pos (Pos G.map 0 15)))
  ; Walk south 1 step.
  (wk S)
  (assert (= G.player-pos (Pos G.map 0 14)))
  ; Try going west, bumping into the level border.
  (cant (wk W) "The border of the dungeon blocks your movement.")
  ; Try walking into a wall tile.
  (wk S)
  (cant (wk S) "Your way is blocked.")
  ; Walk into the (plain) door to the east.
  (wk NE)
  (wk E 3)
  (assert (= (. (at G.player-pos) [0] stem) "door"))
  ; Try walking diagonally past the wall to the north.
  (cant (wk NE) "That diagonal is blocked by a neighbor.")
  ; Walk diagonally between some pillars.
  (setv G.player-pos (Pos G.map 3 1))
  (assert (= (. (get G.map.data 3 2 0) stem) "pillar"))
  (assert (= (. (get G.map.data 4 1 0) stem) "pillar"))
  (wk NE)
  (assert (= G.player-pos (Pos G.map 4 2)))
  ; Try some one-way doors.
  (setv G.player-pos (Pos G.map 3 13))
  (cant (wk S) "That one-way door must be entered from the east.")
  (wk W)
  (setv (cut (at (pos+ G.player-pos Direction.S))) [])
    ; We remove a wall so that stepping into these one-way doors
    ; diagonally won't be blocked by it.
  (cant (wk SE) "That one-way door must be entered from the east.")
  (cant (wk SW) "That one-way door must be entered from the north.")
  (wk W)
  (wk S) ; Now we're on the door.
  (cant (wk N) "You can only go south from this one-way door.")
  (wk S)
  (cant (wk N) "That one-way door must be entered from the north."))
