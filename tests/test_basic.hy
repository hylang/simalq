"Test core features."


(require
  tests.lib [cant])
(import
  fractions [Fraction :as f/]
  pytest
  tests.lib [init init-boot-camp assert-at wait mv-player wk shoot assert-player-at assert-hp]
  simalq.game-state [G]
  simalq.save-load [save-game load-game]
  simalq.quest-definition [mk-tile]
  simalq.commands [move-blocked-msgs])
(setv  T True  F False)


(defn test-bootcamp-level1 []
  "Test the basics of walking, waiting, walls, plain doors, and pillars."

  (init-boot-camp)
  (assert (= G.level-n 1)) ; The level counter is 1-based.

  ; We start at the extreme northwest.
  (assert-player-at 0 15)
  (assert (= G.state-i 0)) ; The state index is 0-based.
  (assert (= G.turn-n 0)) ; Likewise the turn counter.
  ; Walk south 1 step.
  (wk 'S)
  (assert-player-at 0 14)
  (assert (= G.state-i 1))
  (assert (= G.turn-n 1))
  ; Wait 1 turn.
  (wait)
  (assert-player-at 0 14)
  (assert (= G.state-i 2))
  (assert (= G.turn-n 2))
  ; Try going west, bumping into the level border.
  (cant (wk 'W) move-blocked-msgs.map-border)
  (assert (= G.state-i 2))
    ; Failed attempts at actions don't advance the game-state index.
  (assert (= G.turn-n 2))
    ; Nor take turns.
  ; Try walking into a wall tile.
  (wk 'S)
  (assert (= G.turn-n 3))
  (cant (wk 'S) move-blocked-msgs.simple)
  ; Walk into the (plain) door to the east.
  (wk 'NE)
  (wk 'E 3)
  (assert-at 'here 'player "door")
  ; Try walking diagonally past the wall to the north.
  (cant (wk 'NE) move-blocked-msgs.diag)

  ; Walk diagonally between some pillars.
  (mv-player 3 1)
  (assert-at 'N "pillar")
  (assert-at 'E "pillar")
  (wk 'NE)
  (assert-player-at 4 2))


(defn test-shoot []
  (init
    :reality-bubble-size 4
    :tiles ['floor 'floor 'floor 'floor ["orc" :hp 2]])

  (assert (= G.turn-n 0))
  (shoot 'E)
  (assert (= G.turn-n 1))
  ; The orc is out of range and thus unharmed.
  (assert-hp [5 0] 2)
  ; Walk east. The orc is now in the reality bubble, and advances.
  (wk 'E)
  (assert-hp [4 0] 2)
  ; Now shoot and hit it for 1 damage.
  (assert (= G.score 0))
  (shoot 'E)
  (assert (= G.score 3))
  (assert-hp [3 0] 1)
  ; Finish it off with another shot.
  (shoot 'E)
  (assert (= G.score 6))
  (for [x (range (+ G.player.pos.x 1) G.map.width)]
    (assert-at [x 0] 'floor))
  ; Shooting the level border is allowed (but has no effect).
  (shoot 'W)

  ; Shots are blocked by walls.
  (init
    :tiles ["wall" "orc"])
  (assert-hp [2 0] 1)
  (shoot 'E)
  (assert-hp [2 0] 1))


(defn test-walk-wrapping []
  (init
    [:width 20 :height 20 :wrap-y True
      :tiles ["exit"]]
    [:width 20 :height 20 :wrap-x True :wrap-y True])

  (assert-player-at 0 0)
  (wk 'S)
  (assert-player-at 0 19)
  (wk 'S 19)
  (assert-player-at 0 0)
  (cant (wk 'W) move-blocked-msgs.map-border)
  (wk 'E)

  (assert (= G.level-n 2))
  (wk 'W)
  (assert-player-at 19 0)
  (wk 'S)
  (assert-player-at 19 19)
  (wk 'NE)
  (assert-player-at 0 0))


(defn test-shoot-wrapping []
  (init
    :map "
      . o . . @ ."
    :wrap-x T)
  ; We can shoot an arrow that wraps around the level to kill the orc.
  (assert-at [1 0] "orc")
  (shoot 'E)
  (for [x [0 1 2]]
     (assert-at [x 0] 'floor))
  ; A wrapped arrow stops just before it reaches the player's square,
  ; without hitting anything in it.
  (mk-tile G.player.pos "orc")
  (shoot 'E)
  (assert-at 'here "orc" 'player))


(defn test-ambient-poison []

  (defn check [poison hp]
    (assert (and
      (isinstance G.player.poison-dose f/)
      (= G.player.poison-dose poison)
      (= G.player.hp hp))))

  (init-boot-camp)
  (check (f/ 0  ) 500)
  (assert (= G.level.poison-intensity (f/ 1 5)))
  (wait 4)
  (check (f/ 4 5) 500)
  (wait)
  (check (f/ 0  ) 499)
  (setv G.player.hp 10)
  (wait 49)
  (check (f/ 4 5)   1)
  (assert (is G.player.game-over-state None))
  (wait)
  (assert (= G.player.game-over-state 'dead))

  ; Check what happens when you move between levels with different
  ; poison intensities. This works quite differently from IQ's integer
  ; poison counter.
  (init
    [:poison-intensity (f/ 1 3) :tiles ["exit"]]
    [:poison-intensity (f/ 1 5)])
  (check (f/  0  3) 100)
  (wait 2)
  (check (f/  2  3) 100)
  (wait)
  (check (f/  0  3)  99)
  (wait 2)
  (check (f/  2  3)  99)
  (wk 'E)
  (assert (= G.level-n 2))
  ; Since the player always gets the first move on each level, she
  ; hasn't yet breathed in any of the new poison.
  (check (f/  2  3)  99)
  (wait)
  (check (f/ 13 15)  99)
  (wait)
  (check (f/  1 15)  98)

  ; Try a level with no poison.
  (init-boot-camp 26)
  (check (f/ 0) 500)
  (assert (= G.level.poison-intensity (f/ 0)))
  (wait 50)
  (check (f/ 0) 500)

  ; Try a lot of poison (more than IQ would support).
  (init
     :poison-intensity (+ 2 (f/ 1 3)))
  (check (f/ 0  ) 100)
  (wait)
  (check (f/ 1 3)  98)
  (wait)
  (check (f/ 2 3)  96)
  (wait)
  (check (f/ 0  )  93))


(defn test-ambient-poison-with-factor []

  (init
    :poison-factor (f/ 1 4)
    :poison-intensity (f/ 1 3))
  (assert (= G.player.hp 100))
  (wait 11)
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 99))

  (init
    :poison-factor (f/ 0)
    :poison-intensity (f/ 1 3))
  (assert (= G.player.hp 100))
  (wait 50)
  (assert (= G.player.hp 100)))


(defn test-time-limit []
  (init
    [:time-limit 3
      :poison-intensity (f/ 1)
      :map "d @ k k k"
      :map-marks {"k " "key"}]
    [])
  (defn check [level-n turn player-hp keys]
    (assert (= G.level.n level-n))
    (assert (= G.turn-n turn))
    (assert (= G.player.hp player-hp))
    (assert (= G.player.keys keys)))

  ; Walk east, collecting the keys and getting shot by the devil (and
  ; taking poison damage). Contra IQ, the last turn fully elapses
  ; (including e.g. poison) before we go to the next level.
  (wk 'E)
  (check 1 1 89 1)
  (wk 'E)
  (check 1 2 78 2)
  (wk 'E)
  (check 2 3 67 3)
  (wait 10)
  (check 2 13 67 3))


(defn test-game-state-history []
  (init
    :tiles ["handful of gems" "orc"])
  (defn check [state turn score hp t0 t1 t2]
    (assert (and
      (= G.state-i state) (= G.turn-n turn)
      (= G.score score) (= G.player.hp hp)))
    (assert-at [0 0] t0)
    (assert-at [1 0] t1)
    (assert-at [2 0] t2))

  ; Take two actions.
  (check  0 0 0 100  'player "handful of gems" "orc")
  (wk 'E)
  (check  1 1 250 97  'floor 'player "orc")
  (wk 'E)
  (check  2 2 253 97  'floor 'player 'floor)
  ; Undo.
  (G.set-state-i 1)
  (check  1 1 250 97  'floor 'player "orc")
  ; Undo one more action.
  (G.set-state-i 0)
  (check  0 0 0 100  'player "handful of gems" "orc")
  ; Redo.
  (G.set-state-i 1)
  (check  1 1 250 97  'floor 'player "orc")
  ; Undo, then do an "effective redo" where we repeat our previous
  ; action.
  (G.set-state-i 0)
  (wk 'E)
  (check  1 1 250 97  'floor 'player "orc")
  ; That preserved further redo history, so we can redo again.
  (G.set-state-i 2)
  (check  2 2 253 97  'floor 'player 'floor)
  ; Undo, then take a new action.
  (G.set-state-i 0)
  (wk 'N)
  (check  1 1 0 100  'floor "handful of gems" 'floor)
  ; We can no longer redo to the old state 2, since we branched off
  ; the history.
  (assert (= (len G.states) 2))
  (with [(pytest.raises ValueError)]
    (G.set-state-i 2)))


(defn test-saveload [tmp-path]
  (init
    :tiles ["key" "locked disappearing door"])

  (defn check [i n-states score keys thing px]
    (assert (and
      (= G.state-i i) (= G.turn-n i) (= (len G.states) n-states)
      (= G.score score) (= G.player.keys keys)
      (= G.player.pos.xy #(px 0))))
    (assert-at [2 0] thing))

  ; Pick up a key, open a locked disappering door, and step there.
  (wk 'E 3)
  (check  3 4  50 0  'player  2)
  ; Undo the last two actions.
  (G.set-state-i 1)
  (check  1 4  50 1  "locked disappearing door"  1)
  ; Save.
  (save-game (/ tmp-path "mygame"))
  ; Take a new action, discarding the redo history.
  (wk 'W)
  (check  2 3  50 1  "locked disappearing door"  0)
  ; Load, restoring the previous state and the previous history.
  (load-game (/ tmp-path "mygame"))
  (check  1 4  50 1  "locked disappearing door"  1))


(defn test-player-death []
  (init
    :player-starting-hp 20
    :player-start [1 0]
    :tiles [["Dark Knight" :hp 10] 'floor 'floor "orc"])
  (defn check [state-i player-hp orc-x]
    (assert (= G.state-i state-i))
    (when (is-not player-hp None)
      (assert (= G.player.hp player-hp)))
    (assert-at [orc-x 0] "orc"))

  (check 0 20 5)
  (wk 'E)
  (check 1 8 4)
  ; Get killed. Since turn processing stops at the moment of Tris's
  ; death, the orc doesn't get to advance.
  (wk 'E)
  (check 2 None 4)
  ; Now that Tris is dead, she can't do anything.
  (cant (wk 'E) "You're dead. You can undo or load a saved game.")
  (cant (wait) "You're dead. You can undo or load a saved game.")
  ; Undo, and instead do something that doesn't get Tris killed.
  (G.set-state-i 1)
  (check 1 8 4)
  (wk 'W)
  (check 2 8 3))
