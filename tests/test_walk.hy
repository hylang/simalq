"Test the walk action, as well as the wait action."


(require
  tests.lib [cant wk])
(import
  fractions [Fraction :as f/]
  pytest
  tests.lib [init assert-at wait mk-quest]
  simalq.util [GameOverException]
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
  ; Wait 1 turn.
  (wait)
  (assert (= G.player-pos (Pos G.map 0 14)))
  (assert (= G.turn-n 2))
  ; Try going west, bumping into the level border.
  (cant (wk W) "The border of the dungeon blocks your movement.")
  (assert (= G.turn-n 2))   ; Failed attempts at actions don't take turns.
  ; Try walking into a wall tile.
  (wk S)
  (assert (= G.turn-n 3))
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


(defn test-wrapping []
  (init (mk-quest
    [:width 20 :height 20 :wrap-y True
      :tiles ["exit"]]
    [:width 20 :height 20 :wrap-x True :wrap-y True]))

  (assert (= G.player-pos (Pos G.map 0 0)))
  (wk S)
  (assert (= G.player-pos (Pos G.map 0 19)))
  (wk S 19)
  (assert (= G.player-pos (Pos G.map 0 0)))
  (cant (wk W) "The border of the dungeon blocks your movement.")
  (wk E)

  (assert (= G.level-n 2))
  (wk W)
  (assert (= G.player-pos (Pos G.map 19 0)))
  (wk S)
  (assert (= G.player-pos (Pos G.map 19 19)))
  (wk NE)
  (assert (= G.player-pos (Pos G.map 0 0))))


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

  ; Exit from the penultimate level.
  (init "Boot Camp 2" 25)
  (assert (= G.player-pos (Pos G.map 9 21)))
  (setv G.player-pos (Pos G.map 26 9))
  (assert-at 'NE "exit")
  (wk NE)
  (assert (= G.level-n 26))
  (assert (= G.player-pos (Pos G.map 0 9)))

  ; Exit from the last level, winning the game.
  (wk E 14)
  (with [e (pytest.raises GameOverException)]
    (wk E))
  (assert (= e.value.args #('won))))


(defn test-ambient-poison []
  (defn check [poison hp]
    (assert (and
      (isinstance G.poison-dose f/)
      (= G.poison-dose poison)
      (= G.player-hp hp))))

  (init "Boot Camp 2")
  (check (f/ 0  ) 500)
  (assert (= G.level.poison-intensity (f/ 1 5)))
  (wait 4)
  (check (f/ 4 5) 500)
  (wait)
  (check (f/ 0  ) 499)
  (setv G.player-hp 10)
  (wait 49)
  (check (f/ 4 5)   1)
  (with [e (pytest.raises GameOverException)]
    (wait))
  (assert (= e.value.args #('dead)))

  ; Check what happens when you move between levels with different
  ; poison intensities. This works quite differently from IQ's integer
  ; poison counter.
  (init (mk-quest
    [:poison-intensity (f/ 1 3) :tiles ["exit"]]
    [:poison-intensity (f/ 1 5)]))
  (check (f/  0  3) 100)
  (wait 2)
  (check (f/  2  3) 100)
  (wait)
  (check (f/  0  3)  99)
  (wait 2)
  (check (f/  2  3)  99)
  (wk E)
  (assert (= G.level-n 2))
  (check (f/ 13 15)  99)
  (wait)
  (check (f/  1 15)  98)

  ; Try a level with no poison.
  (init "Boot Camp 2" 26)
  (check (f/ 0) 500)
  (assert (= G.level.poison-intensity (f/ 0)))
  (wait 50)
  (check (f/ 0) 500)

  ; Try a lot of poison (more than IQ would support).
  (init (mk-quest
    [:poison-intensity (+ 2 (f/ 1 3))]))
  (check (f/ 0  ) 100)
  (wait)
  (check (f/ 1 3)  98)
  (wait)
  (check (f/ 2 3)  96)
  (wait)
  (check (f/ 0  )  93))
