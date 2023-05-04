(require
  hyrule [ecase]
  tests.lib [cant wk])
(import
  pytest
  tests.lib [init mk-quest assert-at set-square mv-player xy]
  simalq.util [GameOverException]
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

  (mv-player 3 13)
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
  (cant (wk N) "That one-way door must be entered from the north.")

  ; On a one-way door, you can still bump in the forbidden directions.
  ; IQ is inconsistent about this.
  (init (mk-quest
    [:tiles ["one-way door (north)"]]))
  (mv-player 1 0)
  ; Try unlocking a door.
  (set-square 'E "locked door")
  (+= G.player.keys 1)
  (wk E)
  (assert-at 'E "door")
  (cant (wk E) "You can only go north from this one-way door.")
  ; Try attacking a monster.
  (set-square 'E "orc")
  (wk E)
  (assert-at 'E 'floor)
  (cant (wk E) "You can only go north from this one-way door."))


(defn test-locked-doors []
  (init "Boot Camp 2")
  (setv G.player.keys 2)

  ; Unlocked a locked door.
  (mv-player 13 6)
  (assert-at 'S "locked door")
  (setv p (xy G.player.pos))
  (assert (= G.turn-n 0))
  (wk S)  ; This just unlocks the door, without moving us.
  (assert-at 'S "door")
  (assert (= (xy G.player.pos) p))
  (assert (= G.player.keys 1))
  (assert (= G.turn-n 1))  ; But it still takes a turn to do this.

  ; Unlocked a locked disappearing door.
  (mv-player 11 2)
  (assert-at 'W "locked disappearing door")
  (setv p (xy G.player.pos))
  (wk W)
  (assert-at 'W 'floor)
  (assert (= (xy G.player.pos) p))
  (assert (= G.player.keys 0))
  (assert (= G.turn-n 2))

  ; Try and fail to unlock a locked door.
  (set-square 'E "locked door")
  (cant (wk E) "It's locked, and you're keyless at the moment.")
  (assert (= G.turn-n 2))  ; This doesn't take a turn.

  ; Several locks on the same square each take their own key and their
  ; own action to unlock.
  (set-square 'E #* (* ["locked disappearing door"] 2))
  (setv G.player.keys 2)
  (wk E 2)
  (assert-at 'E 'floor)
  (assert (= G.turn-n 4))
  (assert (= G.player.keys 0)))


(defn test-exit []
  (init "Boot Camp 2")

  ; Exit from level 1.
  (mv-player 0 1)
  (assert (= G.level-n 1))
  (assert (= G.score 0))
  (setv map-was G.map)
  (assert-at 'N "exit")
  (wk N)
  (assert (= G.level-n 2))
  (assert (= G.score 0))
    ; In IQ, there was apparently an intention at some point to grant
    ; 100 points when using an exit (see `SetScorePlus`), but it's not
    ; fully implemented. I don't implement it because I think it would
    ; be a bit weird, compared to how points are otherwise scored in
    ; the game.
  (assert (!= G.map map-was))

  ; Exit from the penultimate level.
  (init "Boot Camp 2" 25)
  (assert (= G.player.pos (Pos G.map 9 21)))
  (mv-player 26 9)
  (assert-at 'NE "exit")
  (wk NE)
  (assert (= G.level-n 26))
  (assert (= G.player.pos (Pos G.map 0 9)))

  ; Exit from the last level, winning the game.
  (wk E 14)
  (with [e (pytest.raises GameOverException)]
    (wk E))
  (assert (= e.value.args #('won)))

  ; If the player steps on a tile with two exits, she should only
  ; advance one level, because her turn should end as soon as the
  ; first exit triggers.
  (init (mk-quest [] [] []))
  (set-square 'E #* (* ["exit"] 2))
  (wk E)
  (assert (= G.level-n 2)))


(defn test-cracked-wall []

  ; Destroy a wall with 4 HP.
  (init "Boot Camp 2")
  (mv-player 7 7)
  (assert-at 'N "cracked wall")
  (wk N)
  (assert-at 'N "cracked wall")
  (wk N)
  (assert-at 'N 'floor)
  (assert (= G.player.pos (Pos G.map 7 7)))

  ; Destroy a wall with 10 HP.
  (init (mk-quest
    [:tiles [["cracked wall" :hp 10]]]))
  (assert-at 'E "cracked wall")
  (wk E 4)
  (assert-at 'E "cracked wall")
  (wk E)
  (assert-at 'E 'floor)
  (assert (= G.player.pos (Pos G.map 0 0))))


(defn test-wallfall-trap []
  (init (mk-quest [
    :map "
      @ t1t0t2
      . t1t0t2
      . W1W0W2"
    :map-marks {
      "t0" ["wallfall trap" :wallnum 0]
      "t1" ["wallfall trap" :wallnum 1]
      "t2" ["wallfall trap" :wallnum 2]
      "W0" ["trapped wall" :wallnum 0]
      "W1" ["trapped wall" :wallnum 1]
      "W2" ["trapped wall" :wallnum 2]}]))
  (setv G.rules.reality-bubble-size 0)
    ; Wallfall traps are unaffected by the reality bubble.
  (defn check [#* ts]
    (for [[t [x y]] (zip ts (lfor
        y (reversed (range G.map.height))
        x (range G.map.width)
        [x y]))]
      (assert-at (Pos G.map x y) (ecase t
        "." 'floor
        "@" 'player
        "t" "wallfall trap"
        "W" "trapped wall"))))

  (check
    "@" "t" "t" "t"
    "." "t" "t" "t"
    "." "W" "W" "W")
  ; Traps of type 1 destroy matching walls, type-0 walls, and other
  ; traps of type 1.
  (wk E)
  (check
    "." "@" "t" "t"
    "." "." "t" "t"
    "." "." "." "W")
  ; Traps of type 0 destroy all wallfall traps and walls.
  (wk E)
  (check
    "." "." "@" "."
    "." "." "." "."
    "." "." "." "."))
