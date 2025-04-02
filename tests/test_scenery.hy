(require
  hyrule [ecase do-n]
  tests.lib [cant])
(import
  fractions [Fraction :as f/]
  pytest
  tests.lib [init init-boot-camp locate assert-at assert-hp assert-full-name assert-textmap set-square mv-player assert-player-at top wk wait shoot use-item use-cport]
  simalq.util [GameOverException StatusEffect]
  simalq.geometry [at Pos]
  simalq.game-state [G]
  simalq.quest-definition [mk-tile]
  simalq.commands [move-blocked-msgs])
(setv  T True  F False)


(defn test-unmodifiable-tiles []
  (init-boot-camp)

  (setv [t] (at (Pos G.map 13 5)))
  (assert (= t.stem "locked door"))
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
    :tiles ["one-way door (north)"])
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


(defn test-portcullis []
  (init
    :tiles ["closed portcullis"])

  (cant (wk 'E) "It's locked, and you're keyless at the moment.")
  (setv G.player.keys 1)
  (wk 'E)
  (assert-player-at 0 0)
  (assert-at 'E "open portcullis")
  (wk 'E)
  (assert-at 'here 'player "open portcullis")
  ; After an open portcullis is walked off of, it closes.
  (wk 'E)
  (assert-player-at 2 0)
  (assert-at 'W "closed portcullis"))


(defn test-chest []
  (init)
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
  (assert-at 'E "treasure chest" "meal")
  (shoot 'E)
  (assert-at 'E "treasure chest"))


(defn test-metal-door []
  (init :player-start [1 0])

  (set-square 'W ["metal-door control" :target (locate 'E)])
  ; Use the control to create a door.
  (wk 'W)
  (assert-at 'E "metal door")
  (cant (wk 'E) move-blocked-msgs.simple)
  ; Use the control to destroy the door.
  (wk 'W)
  (assert-at 'E 'floor)
  ; Create the door again while other stuff is there, destroying it.
  (set-square 'E "orc" "pile of gold" "wall")
  (assert (= G.score 0))
  (wk 'W)
  (assert-at 'E "metal door")
  ; Contra IQ, we get points for killing the orc this way.
  (assert (= G.score 3)))


(defn test-exit []
  (init-boot-camp)

  ; Exit from level 1.
  (mv-player 0 1)
  (assert (= G.level-n 1))
  (assert (= G.score 0))
  (setv map-was G.map)
  (assert-full-name 'N "an exit (to level 2)")
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
  (wk 'E 14)
  (assert-full-name 'E "an exit (to victory)")
  (wk 'E)
  (assert (= G.player.game-over-state 'won))
  (cant (wk 'E) "You won the game. You can undo or load a saved game.")

  ; If the player steps on a tile with two exits, she should only
  ; advance one level, because her turn should end as soon as the
  ; first exit triggers.
  (init [] [] [])
  (set-square 'E #* (* ["exit"] 2))
  (wk 'E)
  (assert (= G.level-n 2))

  ; The `level-n` field of an exit takes precedence over the
  ; `next-level` field of the level itself.
  (for [use-special-exit? [F T]]
    (init
      [
        :map "> @ >1"
        :map-marks {">1" ["special exit" :level-n 1]}]
      [])
    (assert-full-name 'W "an exit (to level 2)")
    (assert-full-name 'E "a special exit (back to level 1)")
    (wk (if use-special-exit? 'E 'W))
    (assert (= G.level-n (if use-special-exit? 1 2)))))


(defn test-timed-exit []

  (setv e1 [7 9])
  (setv e2 [8 8])
  (setv center [7 8])

  (init-boot-camp 10)
  (assert-full-name e1 "a timed exit (to level 11, time left 5, next at <Pos 8,8>)")
  (assert-full-name e2 "a timed exit (to level 11, inactive, next at <Pos 7,7>)")
  ; An inactive exit doesn't take you anywhere.
  (mv-player #* center)
  (wk 'E)
  (assert (= G.level-n 10))
  (mv-player #* center)
  (assert (=  (top e1 'deactivates-on-turn) 4))
  (assert (is (top e2 'deactivates-on-turn) None))
  ; Eventually, the next exit becomes active.
  (wait 3)
  (assert (= G.level-n 10))
  (assert (=  (top e1 'deactivates-on-turn) 4))
  (assert (is (top e2 'deactivates-on-turn) None))
  (wait 1)
  (assert (= G.level-n 10))
  (assert (=  (top e1 'deactivates-on-turn) None))
  (assert (is (top e2 'deactivates-on-turn) 9))
  (wk 'E)
  (assert (= G.level-n 11))

  ; If an exit activates while you're standing on it, you
  ; automatically go through it.
  (init-boot-camp 10)
  (mv-player #* e2)
  (wait 4)
  (assert (= G.level-n 10))
  (wait 1)
  (assert (= G.level-n 11)))


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
    :tiles [["cracked wall" :hp 10]])
  (assert-at 'E "cracked wall")
  (wk 'E 4)
  (assert-at 'E "cracked wall")
  (wk 'E)
  (assert-at 'E 'floor)
  (assert-player-at 0 0))


(defn test-breakable-wall []
  (setv marks {
    "| " "breakable wall (meridional)"
    "- " "breakable wall (zonal)"})
  (init
    :reality-bubble-size 2
      ; Destruction propogates outside the reality bubble.
    :map "
      . | | | .
      . | | | .
      . | | | .
      @ - - | -"
    :map-marks marks)

  (wk 'E)
  (assert-textmap :map-marks marks :text "
    . | | | .
    . | | | .
    . | | | .
    @ . . | -")

  (wk 'NE)
  (assert-textmap :map-marks marks :text "
    . . | | .
    . . | | .
    . . | | .
    @ . . | -")

  (shoot 'NE)
  (assert-textmap :map-marks marks :text "
    . . . | .
    . . . | .
    . . . | .
    @ . . | -")

  ; Other means of destroying a wall don't cause chain reactions of breakage.
  (wk 'E)
  (use-item "wall-destroying wand" [3 2])
  (assert-textmap :map-marks marks :text "
    . . . | .
    . . . . .
    . . . | .
    . @ . | -"))


(defn test-fading-wall []
  (init
    :map "
      @ ^█^█^█^█{}. . . . . . x ^█"
    :map-marks {
      "{}" ["gate" :target "x "]
      "x " 'floor})

  ; Fading walls near you at the very start of a level don't actually
  ; get a chance to fade till the end of your first turn.
  (setv marks {"{}" "gate"})
  (assert-textmap :map-marks marks :text "
    @ ^█^█^█^█{}. . . . . . . ^█")
  (wait)
  (assert-textmap :map-marks marks :text "
    @ . . . ^█{}. . . . . . . ^█")
  ; The fading radius is 3.
  (wk 'E)
  (assert-textmap :map-marks marks :text "
    . @ . . . {}. . . . . . . ^█")
  ; A fading wall will still fade if you approach it by teleportation
  ; instead of walking.
  (wk 'E 4)
  (assert-textmap :map-marks marks :text "
    . . . . . {}. . . . . . @ . "))


(defn test-phasing []
  (setv marks {
    "X " "phasing wall (in phase)"
    "o " "phasing wall (out of phase)"
    "| " "phase trigger"})
  (init
    :reality-bubble-size 1
      ; Phasing occurs across the whole level.
    :map "
      X X X | X X o
      o X o @ X o o "
    :map-marks marks)

  ; Phase triggers can be triggered by sword or by arrow.
  (wk 'N)
  (assert-textmap :map-marks marks :text "
    o o o | o o X
    X o X @ o X X ")
  (shoot 'N)
  (assert-textmap :map-marks marks :text "
    X X X | X X o
    o X o @ X o o ")
  ; You can walk through out-of-phase walls, but not in-phase walls.
  (cant (wk 'E) move-blocked-msgs.simple)
  (wk 'W)
  (assert-at 'here 'player "phasing wall (out of phase)"))


(defn test-phasing-iq-quest []
  "Tiles can be constructed a little differently for IQ quests, so ensure
  that phasing hooks are still set correctly."

  (init-boot-camp 16)
  (mv-player 11 0)
  (assert-at [10 0] "phasing wall (out of phase)")
  (assert-at [11 3] "phasing wall (in phase)")
  (shoot 'W)
  (assert-at [10 0] "phasing wall (in phase)")
  (assert-at [11 3] "phasing wall (out of phase)"))


(defn test-explosive-wall []
  (init
    :reality-bubble-size 1
    :map "
      @ X█o X█K X███X█
      X█X█X███X█X███X█
      T T T T T T T T"
    :map-marks {
      "T " ["thorn tree" :hp 100]
      "K " ["Dark Knight" :hp 100]})

  ; Hit one of the explosive walls, setting off a chain reaction
  ; (which can extend past the reality bubble).
  (wk 'E)
  (assert-textmap
    :text "
      @ . . . K . ██X█
      . . . ██. . ██X█
      . . . . . . . T"
    :map-marks {
      "T " ["thorn tree" :hp 100]
      "K " ["Dark Knight" :hp (- 100 (* 4 3))]})
  ; The player is unhurt.
  (assert (= G.player.hp 100))
  ; And she gets points for harmed monsters, as normal.
  (assert (= G.score (+ 3 (* 10 7))))
  ; Hitting an explosive wall takes a turn. In IQ, it doesn't, which
  ; is presumably a bug.
  (assert (= G.turn-n 1)))


(defn test-pushblock []
  (init
    :tiles ["pushblock" "pile of gold"])

  ; Anything in the target square will block a pushblock.
  (cant (wk 'E) "There's no room to push the block there.")
  ; Walk around to the east side of the block and push it back to
  ; our starting position.
  (wk 'NE)
  (wk 'SE)
  (wk 'W)
  (assert-at [0 0] "pushblock")
  (assert-at [1 0] 'player))


(defn test-pushblock-limited []
  (init
    :tiles [["pushblock" :n-pushes 3]])

  (assert-full-name 'E f"a pushblock (pushes left = 3)")
  (wk 'E)
  (assert-full-name 'E f"a pushblock (pushes left = 2)")
  (wk 'E)
  (assert-full-name 'E f"a pushblock (pushes left = 1)")
  (wk 'E)
  (assert-at 'E "wall"))


(defn test-wall-generator []
  (init
    :reality-bubble-size 2
    :map "
      . →|. . o . . .
      @ →|. . . . . .")

  ; Wall generators aren't limited by the reality bubble.
  (wk 'E)
  (assert-textmap "
    . →|. . o . . .
    @ ██████████████")
  ; They can be triggered with an arrow. And they're stopped by any
  ; nonempty tile; monsters are unhurt.
  (cant (wk 'NE) move-blocked-msgs.diag)
  (shoot 'NE)
  (assert-textmap "
    . ██████o . . .
    @ ██████████████"))


(defn test-fountains []
  (init
    :poison-intensity (f/ 1 5)
    :map "
      pfpfwf
      pf@ wf
      ☠ . ."
    :map-marks {
      "wf" "water fountain"
      "pf" "poisonous fountain"
      "☠ " "jar of poison"})

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
  (for [one-shot? [F T]]

    (setv stem (+ (if one-shot? "one-shot " "") "gate"))
    (init)
    (defn t [] (Pos G.map 5 6))

    (mk-tile [1 0] [stem :target (t)])
    (assert-full-name [1 0] f"a {stem} (target <Pos 5,6>)")
    (set-square (t) "orc" "pile of gold" "exit")
    (assert (and (= G.turn-n 0) (= G.player.pos (Pos G.map 0 0))))
    ; Walking into the gate warps us to the target square, but
    ; preserves what's there, and doesn't trigger any tile effects
    ; for walking into the target.
    (wk 'E)
    (assert (and (= G.turn-n 1) (= G.player.pos (t))))
    (assert-at 'here 'player "orc" "pile of gold" "exit")
    ; A one-shot gate is destroyed after use. A regular gate perseveres.
    (assert-at [1 0] (if one-shot? 'floor "gate"))))


(defn test-gate-mapsym-target []
  (init
    :map "
      . . o
      @ {}."
    :map-marks {
      "{}" ["gate" :target "o "]})

  (wk 'E)
  (assert-player-at 2 1)
  (assert-at 'here 'player "orc"))


(defn test-teleporter []

  ; With no other teleporters in range, nothing happens when you walk
  ; into a teleporter.
  (init
    :tiles ["teleporter"])
  (wk 'E)
  (assert-at 'here 'player "teleporter")
  ; Teleporters can't be walked past diagonally.
  (wk 'E)
  (cant (wk 'NW) move-blocked-msgs.diag)

  ; With multiple porters in range, you get sent to one of the
  ; nearest. Re-entering the original sends you to different nearest
  ; porters in sequence.
  (init
    :map "
      ██┣┫████. . .
      ██. ██████┣┫.
      @ ┣┫. ┣┫██. ."
    :map-marks
      {"┣┫" "teleporter"})
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
    (init
      :reality-bubble-size size
      :height 1
      :tiles ["teleporter" "wall" "wall" "teleporter"])
    (wk 'E)
    (assert-player-at (if (= size 2) 1 5) 0))

  ; If you come out of the same teleporter several times, you'll
  ; arrive at its various adjacent free squares in a loop (north
  ; first, per `Direction.all`).
  (init
    :tiles ["teleporter" "wall" "teleporter"])
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
  (init
    :map "
      ████d 0 ┣┫
      @ ┣┫$ ┣┫++"
    :map-marks {
      "┣┫" "teleporter"
      "d " ["devil" :hp 3]
      "0 " "standard bomb"
      "++" "door"})
   (wk 'E)
   (assert-player-at 2 1)
   (assert-at 'here 'player)
   (assert (= G.score 15)))


(defn test-controllable-teleporter []

  (init
    :width 30 :height 1
    :tiles ['floor "controllable teleporter" "wall" "orc" "pile of gold"])
  ; Usually, the `UseControllableTeleporter` action just does the same
  ; thing as `Walk`. The user interface doesn't let you pick the
  ; action if there's no teleporter there, though.
  (use-cport 'E 0 0)
  (assert-player-at 1 0)
  ; You can't teleport into scenery, monsters, or items. Nor can you
  ; go outside the reality bubble. There's no error; you just stay on
  ; the teleporter's square, and a turn is used as normal. (Otherwise,
  ; we'd end up with situations where we want to cancel the player's
  ; action with an error, but we already changed the game state with
  ; hooks triggered by the movement.)
  (setv turn-n 1)
  (for [x [3 4 5 20]]
    (use-cport 'E x 0)
    (assert-player-at 2 0)
    (wk 'W)
    (+= turn-n 2)
    (assert (= G.turn-n turn-n)))
  ; Here's a valid teleport.
  (use-cport 'E 7 0)
  (assert-player-at 7 0)
  (assert (= G.turn-n (+ turn-n 1)))

  (for [teleport? [F T]]
    (init
      :tiles ["open portcullis" 'floor])
    (wk 'E)
    (set-square 'E "controllable teleporter" "key")
    (setv tx 5)
    (use-cport 'E (if teleport? tx 2) 0)
    (assert-player-at (if teleport? tx 2) 0)
    ; Whether or not you actually teleport, hooks for walking off your
    ; origin square run as normal.
    (assert-at [1 0] "closed portcullis")
    ; If you teleport, some hooks for tiles beneath the teleporter
    ; don't run. If you don't, they do.
    (assert (= G.player.keys (if teleport? 0 1)))))


(defn test-wallfall-trap []
  (init
    :reality-bubble-size 0
      ; Wallfall traps are unaffected by the reality bubble.
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
      "W2" ["trapped wall" :wallnum 2]})
  (assert-full-name 'E "a wallfall trap (type 1)")
  (assert-full-name [3 0] "a trapped wall (type 2)")

  (defn check [text]
    (assert-textmap text {
      "t " "wallfall trap"
      "W " "trapped wall"}))

  (check "
    @ t t t
    . t t t
    . W W W")
  ; Traps of type 1 destroy matching walls, type-0 walls, and other
  ; traps of type 1.
  (wk 'E)
  (check "
    . @ t t
    . . t t
    . . . W")
  ; Traps of type 0 destroy all wallfall traps and walls.
  (wk 'E)
  (check "
    . . @ .
    . . . .
    . . . ."))


(defn test-damaging-trap []
  (init
    :tiles ["fixed damaging trap" "one-shot damaging trap"])

  (assert (= G.player.hp 100))
  ; Step on the trap, taking 5 damage.
  (wk 'E)
  (assert (= G.player.hp 95))
  ; We don't take further damage just for hanging around, but the
  ; trap remains.
  (wait)
  (assert (= G.player.hp 95))
  (assert-at 'here 'player "fixed damaging trap")
  ; The other kind of damaging trap is destroyed after being triggered
  ; once.
  (assert-at 'E "one-shot damaging trap")
  (wk 'E)
  (assert (= G.player.hp 90))
  (assert-at 'here 'player))


(defn test-paralysis-trap []
  (init
    :tiles ["paralysis trap" "orc"])
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
    :tiles ["web"])
  (assert-at 'E "web")
  ; Walking onto the web destroys it and paralyzes Tris.
  (wk 'E)
  (assert-at 'here 'player)
  (assert (= (get G.player.status-effects StatusEffect.Para) 2)))


(defn test-weakness-trap []

  (init
    :tiles ["weakness trap" ["orc" :hp 10]])
  ; Get weakened.
  (wk 'E)
  ; The weakness trap is still there.
  (assert-at 'here 'player "weakness trap")
  ; Hit the orc. While Tris is weak, her sword does only 1 damage.
  (assert-hp 'E 10)
  (wk 'E)
  (assert-hp 'E 9)
  ; Arrows are unaffected; they still do 1 damage.
  (shoot 'E)
  (assert-hp 'E 8)
  ; Likewise, magic arrows still do 3 damage.
  (setv G.player.magic-arrows 1)
  (shoot 'E)
  (assert-hp 'E 5)

  ; In IQ, the result of being weak while having the Holy Sword
  ; depends on the order in which you get these effects. This is
  ; undocumented and probably a bug. In SQ, the two effects cancel out
  ; (making your sword do 2 damage), regardless of order.
  (for [reverse? [F T]]
    (setv ts ["Holy Sword" "weakness trap"])
    (when reverse?
      (setv ts (reversed ts)))
    (init
      :height 1
      :tiles [#* ts ["orc" :hp 10]])
    (wk 'E 3)
    (assert-hp 'E 8)))


(defn test-anti-magic-trap []

  (init :tiles [
    "cloak of invisibility"
    "potion of speed"
    "amulet of invulnerability"
    "anti-magic trap"])
  (defn check [ivln ivis fast]
    (assert (= (.player-has? StatusEffect.Ivln) ivln))
    (assert (= (.player-has? StatusEffect.Ivis) ivis))
    (assert (= (.player-has? StatusEffect.Fast) fast)))

  (wk 'E 3)
  (check T T T)
  ; Disenchantment removes effects in a standard order, unlike IQ's
  ; random order.
  (wk 'E)
  (check F T T)
  (wk 'W)
  (wk 'E)
  (check F F T)
  (wk 'W)
  (wk 'E)
  (check F F F))


(defn test-poison-plate []
  (init
    :tiles ["poison pressure plate" "poison-protecting pressure plate"]
    :poison-intensity (f/ 3 7))

  (assert (= G.level.poison-intensity (f/ 3 7)))
  ; Step on the poison plate, doubling the ambient poison and
  ; destroying the plate.
  (wk 'E)
  (assert (= G.level.poison-intensity (f/ 6 7)))
  (assert-at 'here 'player)
  ; Halve it back.
  (wk 'E)
  (assert (= G.level.poison-intensity (f/ 3 7)))
  (assert-at 'here 'player)
  ; Tris took 6/7 poison at the end of turn 0 and 3/7 poison at the
  ; end of turn 1. The sum is 1 + 2/7.
  (assert (= G.player.hp (- 100 1)))
  (assert (= G.player.poison-dose (f/ 2 7))))


(defn test-phase-trap []
  (init :tiles [
    "phase trap"
    "phasing wall (out of phase)"
    "phasing wall (in phase)"])

  ; A phase trap works like a phase trigger.
  (wk 'E)
  (assert-at [2 0] "phasing wall (in phase)")
  (assert-at [3 0] "phasing wall (out of phase)")
  ; It's destroyed after use.
  (assert-at 'here 'player))


(defn test-wall-making-trap []
  (init
    :tiles ["wall-making trap"])
  (wk 'E)
  (assert-at 'here 'player "wall-making trap")
  (wk 'E)
  (assert-at 'W "wall"))


(defn test-barrier-projector []
  "The behavior of magical barrier generators in IQ is pretty finicky, so
  a lot of the details are different in SQ."

  (init :map "
      ████████████████@ ||██████
      []◀▶o $ ||==. ====[]==| []")
  (assert-full-name 'SE "a barrier projector (active)")
  ; Hitting the projector turns off barriers in an unbroken straight
  ; line from it.
  (wk 'SE)
  (assert-textmap "
      ████████████████@ . ██████
      []◀▶o $ ||==. . . []. | []")
  (assert-full-name 'SE "a barrier projector (time left 7)")
  ; Hitting it again resets the timer.
  (wait 1)
  (assert-full-name 'SE "a barrier projector (time left 6)")
  (wk 'SE)
  (assert-full-name 'SE "a barrier projector (time left 7)")
  ; After 7 turns, the projector reactivates, creating a new barrier
  ; to eligible projectors. Contra IQ, there's no distance limiit.
  ; - No barrier is projected north because there's no projector.
  ; - No barriers are projected east because the column blocks the
  ;   other projector.
  ; - The hole, gold, and barrier of the wrong orientation are still there.
  ; - The orc is slain.
  ; - No extra barrier was created on the square that already had one
  ;   (of the correct orientation).
  (wait 6)
  (assert-textmap "
      ████████████████@ . ██████
      []◀▶o $ ||==. . . []. | []")
  (wait)
  (assert-textmap
    :text "
      ████████████████@ . ██████
      []01==0203========[]. | []"
    :map-marks {
      "01" #("magical barrier (zonal)" "hole")
      "02" #("magical barrier (zonal)" "pile of gold")
      "03" #("magical barrier (zonal)" "magical barrier (meridional)")})
  ; You get points for monsters killed by barriers.
  (assert (= G.score 3))

  ; A projector can connect with itself on a wrapped level.
  (init :wrap-x T :wrap-y T :map "
    . . . .
    . @ [].
    . . . .
    . . . .")
  ; Projectors can be deactivated by shooting them.
  (shoot 'E)
  (wait 10)
  (wk 'N)
  (assert-textmap "
     . @ ||.
     ====[]==
     . . ||.
     . . ||.")
  ; Tris took 5 damage for being caught in a newly projected barrier
  ; (but no additional damage for just waiting on it, after it was
  ; created).
  (assert (= G.player.hp 95)))
