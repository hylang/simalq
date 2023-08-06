(require
  hyrule [ecase do-n]
  tests.lib [cant])
(import
  fractions [Fraction :as f/]
  pytest
  tests.lib [init init-boot-camp assert-at assert-full-name set-square mv-player assert-player-at wk wait shoot]
  simalq.util [GameOverException StatusEffect]
  simalq.geometry [at Pos]
  simalq.game-state [G]
  simalq.quest-definition [mk-tile])


(defn test-unmodifiable-tiles []
  (init-boot-camp)

  (setv [t] (at (Pos G.map 13 5)))
  (assert (= t.stem "locked door"))
  (with [(pytest.raises AttributeError)]
    (setv t.fooey 5))
  (with [(pytest.raises AttributeError)]
    (setv t.pos (Pos G.map 11 6))))


(defn test-one-way-door []
  (init-boot-camp)

  (mv-player 3 13)
  (cant (wk 'S) "That one-way door must be entered from the east.")
  (wk 'W)
  (set-square 'S)
    ; We remove a wall so that stepping into these one-way doors
    ; diagonally won't be blocked by it.
  (cant (wk 'SE) "That one-way door must be entered from the east.")
  (cant (wk 'SW) "That one-way door must be entered from the north.")
  (wk 'W)
  (wk 'S) ; Now we're on the door.
  (cant (wk 'N) "You can only go south from this one-way door.")
  (wk 'S)
  (cant (wk 'N) "That one-way door must be entered from the north.")

  ; On a one-way door, you can still bump in the forbidden directions.
  ; IQ is inconsistent about this.
  (init
    [:tiles ["one-way door (north)"]])
  (mv-player 1 0)
  ; Try unlocking a door.
  (set-square 'E "locked door")
  (+= G.player.keys 1)
  (wk 'E)
  (assert-at 'E "door")
  (cant (wk 'E) "You can only go north from this one-way door.")
  ; Try attacking a monster.
  (set-square 'E "orc")
  (wk 'E)
  (assert-at 'E 'floor)
  (cant (wk 'E) "You can only go north from this one-way door."))


(defn test-locked-doors []
  (init-boot-camp)
  (setv G.player.keys 2)

  ; Unlocked a locked door.
  (mv-player 13 6)
  (assert-at 'S "locked door")
  (setv p G.player.pos.xy)
  (assert (= G.turn-n 0))
  (wk 'S)  ; This just unlocks the door, without moving us.
  (assert-at 'S "door")
  (assert (= G.player.pos.xy p))
  (assert (= G.player.keys 1))
  (assert (= G.turn-n 1))  ; But it still takes a turn to do this.

  ; Unlocked a locked disappearing door.
  (mv-player 11 2)
  (assert-at 'W "locked disappearing door")
  (setv p G.player.pos.xy)
  (wk 'W)
  (assert-at 'W 'floor)
  (assert (= G.player.pos.xy p))
  (assert (= G.player.keys 0))
  (assert (= G.turn-n 2))

  ; Try and fail to unlock a locked door.
  (set-square 'E "locked door")
  (cant (wk 'E) "It's locked, and you're keyless at the moment.")
  (assert (= G.turn-n 2))  ; This doesn't take a turn.

  ; Several locks on the same square each take their own key and their
  ; own action to unlock.
  (set-square 'E #* (* ["locked disappearing door"] 2))
  (setv G.player.keys 2)
  (wk 'E 2)
  (assert-at 'E 'floor)
  (assert (= G.turn-n 4))
  (assert (= G.player.keys 0)))


(defn test-chest []
  (init [])
  (set-square [1 0] "treasure chest" "pile of gold")
  (set-square [2 0] "treasure chest" "meal")

  ; Fail to unlock the chest.
  (cant (wk 'E) "It's locked, and you're keyless at the moment.")
  ; Open the chest.
  (setv G.player.keys 2)
  (wk 'E)
  ; Take the gold that was in the chest.
  (assert (= G.score 0))
  (wk 'E)
  (assert (= G.score 100))
  ; Fire an arrow. Contra IQ, chests don't protect their contents
  ; from arrows.
  (assert-at 'E ["treasure chest" "meal"])
  (shoot 'E)
  (assert-at 'E "treasure chest"))


(defn test-exit []
  (init-boot-camp)

  ; Exit from level 1.
  (mv-player 0 1)
  (assert (= G.level-n 1))
  (assert (= G.score 0))
  (setv map-was G.map)
  (assert-at 'N "exit")
  (wk 'N)
  (assert (= G.level-n 2))
  (assert (= G.score 0))
    ; In IQ, there was apparently an intention at some point to grant
    ; 100 points when using an exit (see `SetScorePlus`), but it's not
    ; fully implemented. I don't implement it because I think it would
    ; be a bit weird, compared to how points are otherwise scored in
    ; the game.
  (assert (!= G.map map-was))

  ; Exit from the penultimate level.
  (init-boot-camp 25)
  (assert-player-at 9 21)
  (mv-player 26 9)
  (assert-at 'NE "exit")
  (wk 'NE)
  (assert (= G.level-n 26))
  (assert-player-at 0 9)

  ; Exit from the last level, winning the game.
  (wk 'E 15)
  (assert (= G.player.game-over-state 'won))
  (cant (wk 'E) "You won the game. You can undo or load a saved game.")

  ; If the player steps on a tile with two exits, she should only
  ; advance one level, because her turn should end as soon as the
  ; first exit triggers.
  (init [] [] [])
  (set-square 'E #* (* ["exit"] 2))
  (wk 'E)
  (assert (= G.level-n 2)))


(defn test-cracked-wall []

  ; Destroy a wall with 4 HP.
  (init-boot-camp)
  (mv-player 7 7)
  (assert-at 'N "cracked wall")
  (assert-full-name 'N "a cracked wall (HP 4)")
  (wk 'N)
  (assert-at 'N "cracked wall")
  (wk 'N)
  (assert-at 'N 'floor)
  (assert-player-at 7 7)

  ; Destroy a wall with 10 HP.
  (init
    [:tiles [["cracked wall" :hp 10]]])
  (assert-at 'E "cracked wall")
  (wk 'E 4)
  (assert-at 'E "cracked wall")
  (wk 'E)
  (assert-at 'E 'floor)
  (assert-player-at 0 0))


(defn test-pushblock []
  (init
    [:tiles ["pushblock" "pile of gold"]])

  ; Anything in the target square will block a pushblock.
  (cant (wk 'E) "There's no room to push the block there.")
  ; Walk around to the east side of the block and push it back to
  ; our starting position.
  (wk 'NE)
  (wk 'SE)
  (wk 'W)
  (assert-at [0 0] "pushblock")
  (assert-at [1 0] 'player))


(defn test-fountains []
  (init [
    :poison-intensity (f/ 1 5)
    :map "
      pfpfwf
      pf@ wf
      ☠ . ."
    :map-marks {
      "wf" "water fountain"
      "pf" "poisonous fountain"
      "☠ " "jar of poison"}])

  ; When next to at least one water fountain, the player takes no
  ; damage from ambient poison or from poison fountains.
  (assert (= G.player.hp 100))
  (wait 10)
  (assert (= G.player.hp 100))
  (set-square 'E)
  (wait 10)
  (assert (= G.player.hp 100))
  ; Water fountains don't protect against other sources of poison
  ; damage.
  (shoot 'SW)
  (assert (= G.player.hp 80))

  ; Without a water fountain, poisonous fountains do 2 damage per
  ; turn. This stacks with ambient poison, but it only applies once no
  ; matter how many poison fountains are adjacent.
  (set-square 'NE)
  (wait)
  (assert (= G.player.hp 78))
  (wait 3)
  (assert (= G.player.hp 72))
  (wait)
  (assert (= G.player.hp 69)))


(defn test-gate []
  (init [])
  (defn t [] (Pos G.map 5 6))

  (mk-tile [1 0] ["gate" :target (t)])
  (assert-full-name [1 0] "a gate (dest <Pos 5,6>)")
  (set-square (t) "orc" "pile of gold" "exit")
  (assert (and (= G.turn-n 0) (= G.player.pos (Pos G.map 0 0))))
  ; Walking into the gate warps us to the target square, but
  ; preserves what's there, and doesn't trigger any tile effects
  ; for walking into the target.
  (wk 'E)
  (assert (and (= G.turn-n 1) (= G.player.pos (t))))
  (assert-at 'here ['player "orc" "pile of gold" "exit"]))


(defn test-gate-mapsym-target []
  (init [
    :map "
      . . o
      @ {}."
    :map-marks {
      "{}" ["gate" :target "o "]}])

  (wk 'E)
  (assert-player-at 2 1)
  (assert-at 'here ['player "orc"]))


(defn test-teleporter []

  ; With no other teleporters in range, nothing happens when you walk
  ; into a teleporter.
  (init
    [:tiles ["teleporter"]])
  (wk 'E)
  (assert-at 'here ['player "teleporter"])
  ; Teleporters can't be walked past diagonally.
  (wk 'E)
  (cant (wk 'NW) "That diagonal is blocked by a neighbor.")

  ; With multiple porters in range, you get sent to one of the
  ; nearest. Re-entering the original sends you to different nearest
  ; porters in sequence.
  (init
    [:map "
      ██┣┫████. . .
      ██. ██████┣┫.
      @ ┣┫. ┣┫██. ."])
  (wk 'E)
  (assert-player-at 2 0)
  (mv-player 0 0)
  (wk 'E)
  (assert-player-at 1 1)
  (mv-player 0 0)
  (wk 'E)
  (assert-player-at 2 0)

  ; The destination porter needs to be in the reality bubble, but the
  ; target square need not be.
  (for [size [2 3]]
    (init [
      :height 1
      :tiles ["teleporter" "wall" "wall" "teleporter"]])
    (setv G.rules.reality-bubble-size size)
    (wk 'E)
    (assert-player-at (if (= size 2) 1 5) 0))

  ; If you come out of the same teleporter several times, you'll
  ; arrive at its various adjacent free squares in a loop (north
  ; first, per `Direction.all`).
  (init [
    :tiles ["teleporter" "wall" "teleporter"]])
  (setv targets [])
  (do-n 5
    (mv-player 0 0)
    (wk 'E)
    (.append targets G.player.pos.xy))
  (assert (= targets (list (map tuple [
    [3 1] [4 1] [4 0] [2 1] [3 1]]))))

  ; Squares with items or scenery aren't eligible targets, but squares
  ; with monsters are. The monsters die (with all the normal
  ; consequences of monsters dying, contra IQ).
  (init [
    :map "
      ████d 0 ┣┫
      @ ┣┫$ ┣┫++"
    :map-marks {
      "$ " "pile of gold"
      "d " ["devil" :hp 3]
      "0 " "standard bomb"
      "++" "door"}])
   (wk 'E)
   (assert-player-at 2 1)
   (assert-at 'here 'player)
   (assert (= G.score 15)))


(defn test-wallfall-trap []
  (init [
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
      "W2" ["trapped wall" :wallnum 2]}])
  (assert-full-name 'E "a wallfall trap (type 1)")
  (assert-full-name [3 0] "a trapped wall (type 2)")

  (setv G.rules.reality-bubble-size 0)
    ; Wallfall traps are unaffected by the reality bubble.

  (defn check [#* ts]
    (for [[t [x y]] (zip ts (lfor
        y (reversed (range G.map.height))
        x (range G.map.width)
        [x y]))]
      (assert-at [x y] (ecase t
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
  (wk 'E)
  (check
    "." "@" "t" "t"
    "." "." "t" "t"
    "." "." "." "W")
  ; Traps of type 0 destroy all wallfall traps and walls.
  (wk 'E)
  (check
    "." "." "@" "."
    "." "." "." "."
    "." "." "." "."))


(defn test-damaging-trap []
  (init
    [:tiles ["fixed damaging trap"]])

  (assert (= G.player.hp 100))
  ; Step on the trap, taking 5 damage.
  (wk 'E)
  (assert (= G.player.hp 95))
  ; We don't take further damage just for hanging around, but the
  ; trap remains.
  (wait)
  (assert (= G.player.hp 95))
  (assert-at 'here ['player "fixed damaging trap"]))


(defn test-paralysis-trap []
  (init
    [:tiles ["paralysis trap" "orc"]])
  (setv para-msg "You're paralyzed. You can only wait for it to pass.")

  ; Per IQ, after Tris gets paralyzed, monsters get 3 actions before
  ; she can act again.
  (wk 'E)
  (assert (= G.player.hp 97))
  (cant (wk 'E) para-msg)
  (wait)
  (assert (= G.player.hp 94))
  (cant (wk 'E) para-msg)
  (wait)
  (assert (= G.player.hp 91))
  (wk 'E)
  (assert (= G.player.hp 91)))


(defn test-web []
  (init
    [:tiles ["web"]])
  (assert-at 'E "web")
  ; Walking onto the web destroys it and paralyzes Tris.
  (wk 'E)
  (assert-at 'here 'player)
  (assert (= (get G.player.status-effects StatusEffect.Para) 2)))
