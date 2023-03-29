(require
  tests.lib [cant wk])
(import
  pytest
  tests.lib [init assert-at]
  simalq.game-state [G]
  simalq.geometry [Pos Direction pos+ at])


(defn test-bootcamp-level1 []
  (init "Boot Camp 2")
  (assert (= G.level-n 1)) ; The level counter is 1-based.

  ; We start at the extreme northwest.
  (assert (= G.player-pos (Pos G.map 0 15)))
  (assert (= G.turn-n 0)) ; The turn counter is 0-based.
  ; Walk south 1 step.
  (wk S)
  (assert (= G.player-pos (Pos G.map 0 14)))
  (assert (= G.turn-n 1))
  ; Try going west, bumping into the level border.
  (cant (wk W) "The border of the dungeon blocks your movement.")
  (assert (= G.turn-n 1))   ; Failed attempts at actions don't take turns.
  ; Try walking into a wall tile.
  (wk S)
  (assert (= G.turn-n 2))
  (cant (wk S) "Your way is blocked.")
  ; Walk into the (plain) door to the east.
  (wk NE)
  (wk E 3)
  (assert-at 'here "door")
  ; Try walking diagonally past the wall to the north.
  (cant (wk NE) "That diagonal is blocked by a neighbor.")
  ; Walk diagonally between some pillars.
  (setv G.player-pos (Pos G.map 3 1))
  (assert-at 'N "pillar")
  (assert-at 'E "pillar")
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


(defn test-locked-doors []
  (init "Boot Camp 2")
  (setv G.keys 2)

  (setv G.player-pos (Pos G.map 13 6))
  (assert-at 'S "locked door")
  (setv p G.player-pos)
  (assert (= G.turn-n 0))
  (wk S)  ; This just unlocks the door, without moving us.
  (assert-at 'S "door")
  (assert (= G.player-pos p))
  (assert (= G.keys 1))
  (assert (= G.turn-n 1))  ; But it still takes a turn to do this.

  (setv G.player-pos (Pos G.map 11 2))
  (assert-at 'W "locked disappearing door")
  (setv p G.player-pos)
  (wk W)
  (assert-at 'W 'floor)
  (assert (= G.player-pos p))
  (assert (= G.keys 0))
  (assert (= G.turn-n 2)))


(defn test-exit []
  (init "Boot Camp 2")

  ; Exit from level 1.
  (setv G.player-pos (Pos G.map 0 1))
  (assert (= G.level-n 1))
  (setv map-was G.map)
  (assert-at 'N "exit")
  (wk N)
  (assert (= G.level-n 2))
  (assert (!= G.map map-was))

  ; Exit from the last level, winning the game.
  (init "Boot Camp 2" 26)
  (assert (= G.player-pos (Pos G.map 0 9)))
  (wk E 14)
  (with [e (pytest.raises hy.M.simalq/util.GameOverException)]
    (wk E))
  (assert (= e.value.args #('won))))
