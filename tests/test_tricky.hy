"Tests of finicky interactions not associated with a specific tile."


(require
  tests.lib [wk])
(import
  tests.lib [init mk-quest assert-at shoot]
  simalq.game-state [G]
  simalq.geometry [Pos at])
(setv  T True  F False)


(defn test-player-goes-first []
  ; The player acts first on every level.
  (init (mk-quest
    [:tiles [["orc" :hp 3] "exit"]]
    [:tiles [["orc" :hp 1]]]))
  (wk E 2)
  (assert (and (= G.player.hp 97)))
  (wk E 2)
  ; We're now on level 2, but the second monster won't get a hit in.
  (assert (and (= G.level-n 2)))
  (assert (and (= G.player.hp 97)))
  (assert-at 'E "orc")
  (wk E)
  (assert (and (= G.player.hp 97)))
  (assert-at 'E 'floor))


(defn test-refresh-level []
  ; Levels are refreshed when you revisit them.
  (init (mk-quest [
    :next-level 1 :tiles [["orc" :hp 3] "key" "exit"]]))

  (assert (= G.player.pos (Pos G.map 0 0)))
  (assert (= G.level-n 1))
  (wk E 4)
  (assert (and (= G.player.hp 97) (= G.player.keys 1)))
  (wk E)
  (assert (= G.player.pos (Pos G.map 0 0)))
  (assert (= G.level-n 1))
  ; The monster and the item are back now.
  (wk E 4)
  (print G.player.hp G.player.keys)
  (assert (and (= G.player.hp 94) (= G.player.keys 2))))


(defn test-shoot-through []
  "Test which tile types can be shot through by the player or monsters."

  (defn check [tile player-can-shoot-through mon-can-shoot-through]
    (init (mk-quest
      [:tiles ['floor tile 'floor ["devil" :hp 2]]]))
    (assert (= G.player.hp 100))
    (shoot 'E)
    (assert (=
      (. (at (Pos G.map (+ 3 mon-can-shoot-through) 0)) [0] hp)
      (if player-can-shoot-through 1 2)))
    (assert (= G.player.hp (if mon-can-shoot-through 90 100))))

  (check 'floor            T T)
  (check "wall"            F F)
  (check ["orc" :hp 2]     F F)
  (check "door"            F F)
  (check "exit"            T T)
  (check "wallfall trap"   T T)
  (check "magical energy shield"
                           T F)
  (check "pile of gold"    F F)
  (check "key"             F F))
    ; In IQ, keys (including magical keys) are unlike all other
    ; pickups in that the player can shoot through them. The
    ; motivation for this exception is mysterious to me, so I decided
    ; to omit it.
