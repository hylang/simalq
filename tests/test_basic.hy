"Test core features."


(require
  tests.lib [cant wk])
(import
  fractions [Fraction :as f/]
  pytest
  tests.lib [init assert-at wait mk-quest mv-player]
  simalq.util [GameOverException]
  simalq.game-state [G]
  simalq.geometry [Pos])


(defn test-bootcamp-level1 []
  "Test the basics of walking, waiting, walls, plain doors, and pillars."

  (init "Boot Camp 2")
  (assert (= G.level-n 1)) ; The level counter is 1-based.

  ; We start at the extreme northwest.
  (assert (= G.player.pos (Pos G.map 0 15)))
  (assert (= G.turn-n 0)) ; The turn counter is 0-based.
  ; Walk south 1 step.
  (wk S)
  (assert (= G.player.pos (Pos G.map 0 14)))
  (assert (= G.turn-n 1))
  ; Wait 1 turn.
  (wait)
  (assert (= G.player.pos (Pos G.map 0 14)))
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
  (assert-at 'here ['player "door"])
  ; Try walking diagonally past the wall to the north.
  (cant (wk NE) "That diagonal is blocked by a neighbor.")

  ; Walk diagonally between some pillars.
  (mv-player 3 1)
  (assert-at 'N "pillar")
  (assert-at 'E "pillar")
  (wk NE)
  (assert (= G.player.pos (Pos G.map 4 2))))


(defn test-walk-wrapping []
  (init (mk-quest
    [:width 20 :height 20 :wrap-y True
      :tiles ["exit"]]
    [:width 20 :height 20 :wrap-x True :wrap-y True]))

  (assert (= G.player.pos (Pos G.map 0 0)))
  (wk S)
  (assert (= G.player.pos (Pos G.map 0 19)))
  (wk S 19)
  (assert (= G.player.pos (Pos G.map 0 0)))
  (cant (wk W) "The border of the dungeon blocks your movement.")
  (wk E)

  (assert (= G.level-n 2))
  (wk W)
  (assert (= G.player.pos (Pos G.map 19 0)))
  (wk S)
  (assert (= G.player.pos (Pos G.map 19 19)))
  (wk NE)
  (assert (= G.player.pos (Pos G.map 0 0))))


(defn test-ambient-poison []
  ; This also tests dying from hit-point loss.

  (defn check [poison hp]
    (assert (and
      (isinstance G.player.poison-dose f/)
      (= G.player.poison-dose poison)
      (= G.player.hp hp))))

  (init "Boot Camp 2")
  (check (f/ 0  ) 500)
  (assert (= G.level.poison-intensity (f/ 1 5)))
  (wait 4)
  (check (f/ 4 5) 500)
  (wait)
  (check (f/ 0  ) 499)
  (setv G.player.hp 10)
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
  ; Since the player always gets the first move on each level, she
  ; hasn't yet breathed in any of the new poison.
  (check (f/  2  3)  99)
  (wait)
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
