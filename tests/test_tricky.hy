"Tests of finicky interactions not associated with a specific tile."


(import
  tests.lib [init assert-at assert-hp assert-player-at wk wait shoot mv-player]
  simalq.game-state [G]
  simalq.geometry [Pos at]
  simalq.quest-definition [mk-tile])
(setv  T True  F False)


(defn test-player-goes-first []
  "The player acts first on every level."

  (defn check [state-i turn-n level-n [player-hp None]]
    (assert (and
      (= G.state-i state-i)
      (= G.turn-n turn-n)
      (= G.level-n level-n)))
    (when (is-not player-hp None)
      (assert (= G.player.hp player-hp))))

  (init
    [:tiles [["orc" :hp 3] "exit"]]
    [:tiles [["orc" :hp 1]]])
  (check  0 0  1 100)
  (wk 'E 2)
  (check  2 2  1 97)
  (wk 'E 2)
  ; We're now on level 2, but the second monster won't get a hit in.
  (check  4 3  2 97)
  (assert-at 'E "orc")
  (wk 'E)
  (check  5 4  2 97)
  (assert-at 'E 'floor)

  ; The effect of a potion of speed doesn't stack with this free
  ; action.
  (init
    [:tiles ["potion of speed" 'floor "exit"]]
    [])
  (check  0 0  1)
  (wk 'E)
  (check  1 0  1)
  (wk 'E)
  (check  2 1  1)
  (assert-at 'E "exit")
  (wk 'E)
  (check  3 1  2)
  (wk 'E)
  (check  4 2  2))


(defn test-refresh-level []
  ; Levels are refreshed when you revisit them.
  (init [
    :next-level 1 :tiles [["orc" :hp 3] "key" "exit"]])

  (assert-player-at 0 0)
  (assert (= G.level-n 1))
  (wk 'E 4)
  (assert (and (= G.player.hp 97) (= G.player.keys 1)))
  (wk 'E)
  (assert-player-at 0 0)
  (assert (= G.level-n 1))
  ; The monster and the item are back now.
  (wk 'E 4)
  (assert (and (= G.player.hp 94) (= G.player.keys 2))))


(defn test-shoot-through []
  "Test which tile types can be shot through by the player or monsters."

  (defn check [tile player-can-shoot-through mon-can-shoot-through]
    (when (is (type tile) str)
      (setv tile (. tile
        (replace "(i)" "(in phase)")
        (replace "(o)" "(out of phase)"))))
    (init
      [:tiles ['floor tile 'floor ["devil" :hp 2]]])
    (assert (= G.player.hp 100))
    (shoot 'E)
    (assert-hp
      [(+ 3 mon-can-shoot-through) 0]
      (if player-can-shoot-through 1 2))
    (assert (= G.player.hp (if mon-can-shoot-through 90 100))))

  (check 'floor                  T T)
  (check "wall"                  F F)
  (check "Void"                  F F)
  (check ["orc" :hp 2]           F F)
  (check "door"                  F F)
  (check "exit"                  T T)
  (check "wallfall trap"         T T)
  (check "hole"                  T T)
  (check "poison pressure plate" T T)
    ; The IQ equivalent of this tile, the poisonous amulet, is an item
    ; and hence blocks shots.
  (check "magical energy shield" T F)
  (check "broken pillar"         T F)
  (check "open portcullis"       T T)
  (check "closed portcullis"     T T)
    ; In IQ, portculli block all shots, even when open, which seems
    ; pretty silly to me.
  (check "phasing wall (i)"      F F)
  (check "phasing wall (o)"      T F)
  (check "pile of gold"          F F)
  (check "key"                   F F))
    ; In IQ, keys (including magical keys) are unlike all other
    ; pickups in that the player can shoot through them. The
    ; motivation for this exception is mysterious to me, so I decided
    ; to omit it.


(defn test-attackability-stacks []
  "Monsters and the player can attack either other, by melee or by
  shots, even when sharing a map stack with a wall tile. Although
  obstacles like wall tiles can block shots from entering another
  square (as tested elsewhere), they don't protect other tiles at the
  same `Pos`. The motivation here is to prevent the player from
  getting asymmetrical shot-blocking by just using a wall-making wand
  on her own position."

  (defn mon-p []
    (Pos G.map (if ranged 2 1) 0))

  (for [wall-on-top [F T]  ranged [F T]]
    (init [])
    (mk-tile (mon-p) ["devil" :hp 3])
    (for [p [G.player.pos (mon-p)]]
      (mk-tile p "wall")
      (when (not wall-on-top)
        (setv (cut (at p)) (reversed (at p)))))
    (if ranged
      (shoot 'E)
      (wk 'E))
    (assert (= (. (at (mon-p)) [wall-on-top] hp) (- 3 (if ranged 1 2))))
    (assert (= G.player.hp (- 100 (if ranged 10 3))))))


(defn test-no-instack-attack []
  "Monsters on your square can't attack you."

  (init
    [:tiles ["devil"]])
  (mv-player 1 0)
  (assert (= G.player.hp 100))
  ; We wait. The devil can't attack. It doesn't move, either, since
  ; its behavior is `Approach`.
  (wait)
  (assert (= G.player.hp 100))
  (wk 'E)
  (assert (= G.player.hp 97)))
