(require
  tests.lib [cant wk])
(import
  tests.lib [init mk-quest assert-at set-square mv-player shoot]
  simalq.geometry [Pos Direction pos+ at]
  simalq.game-state [G])


(defn test-treasure []
  (init "Boot Camp 2")

  (mv-player 6 14)
  (assert (= G.score 0))
  (assert-at 'E "pile of gold")
  (wk E)
  (assert (= G.score 100))
  (assert-at 'here 'player)

  (mv-player 5 10)
  (assert-at 'N "handful of gems")
  (wk N)
  (assert (= G.score 350))
  (assert-at 'here 'player))


(defn test-key-get []
  (init "Boot Camp 2")
  (mv-player 13 10)
  (assert (and (= G.player.keys 0) (= G.score 0)))
  (wk S)
  (assert (and (= G.player.keys 1) (= G.score 50)))

  (init "Boot Camp 2")
  (mv-player 13 10)
  (setv G.player.keys G.rules.max-keys)
  (assert (and (= G.player.keys G.rules.max-keys) (= G.score 0)))
  (cant (wk S) "Your keyring has no room for another key.")
  (assert (and (= G.player.keys G.rules.max-keys) (= G.score 0)))

  ; Being maxed out on keys doesn't prevent you from unlocking a door
  ; on the same square.
  (init (mk-quest []))
  (set-square 'E "key" "locked door")
  (assert-at 'E ["key" "locked door"])
  (setv G.player.keys G.rules.max-keys)
  (wk E)
  (assert-at 'E ["key" "door"])
  (assert (= G.player.keys (- G.rules.max-keys 1)))
  (wk E)
  (assert-at 'here ['player "door"]))


(defn test-food-eat []
  (init (mk-quest [:tiles [
     "snack" "meal" "jar of poison" "rotten food" "empty platter"
     "dessert" "snack" "dessert"]]))
  (assert (= G.player.hp 100))
  (wk E)
  (assert (= G.player.hp 125))
  (wk E)
  (assert (= G.player.hp 225))
  (wk E)
  (assert (= G.player.hp 175))
  (wk E)
  (assert (= G.player.hp  75))
  (wk E)
  (assert (= G.player.hp  75))
  (wk E)
  (assert (= G.player.hp 500))
  (wk E)
  (assert (= G.player.hp 525))
  ; The second dessert has no effect, because we're already at 500 HP.
  (wk E)
  (assert (= G.player.hp 525)))


(defn test-food-shoot []
  (init (mk-quest [
    :map "
      @ % ☠ ██o o
      ████████G ██
      ████o ██████"
    :map-marks {
      "% " "snack"
      "☠ " "jar of poison"
      "o " ["orc" :hp 4]}]))
  ; Shooting a snack (or most other foods) just destroys it.
  (assert-at 'E "snack")
  (shoot 'E)
  (assert-at 'E 'floor)
  ; Shooting a jar of poison creates a 5 × 5 cloud of poison, which
  ; can damage both the player and monsters. It goes through walls.
  (wk E)
  (assert-at 'E "jar of poison")
  (assert (= G.player.hp 100))
  (shoot 'E)
  (assert-at 'E 'floor)
  ; The player takes 20 damage.
  (assert (= G.player.hp 80))
  ; The two orcs in the blast radius take 3 damage each.
  (assert (= (. (at (Pos G.map 2 0)) [0] hp) 1))
  (assert (= (. (at (Pos G.map 4 2)) [0] hp) 1))
  ; The ghost, being undead, takes no damage from poison.
  (assert (= (. (at (Pos G.map 4 1)) [0] hp) 1))
  ; The third orc is outside of the blast radius.
  (assert (= (. (at (Pos G.map 5 2)) [0] hp) 4)))
