(import
  fractions [Fraction]
  simalq.util [REALITY-BUBBLE-SIZE hurt-player DamageType]
  simalq.geometry [burst at]
  simalq.game-state [G]
  simalq.player-actions [do-action])


(defn start-quest [quest]
  (setv
    G.quest quest
    G.score 0
    G.turn-n 0
    G.player-hp quest.starting-hp
    G.poison-dose (Fraction 0)
    G.keys 0)
  (start-level 1))


(defn start-level [level-n]
  (setv
    G.level-n level-n
    level (get G.quest.levels (- G.level-n 1))
    G.map level.map
    G.player-pos level.player-start))


(defn take-turn [action]
  (do-action action)

  ; Allow actors in the reality bubble to act, in `burst`'s spiral
  ; order.
  (for [
      pos (burst G.player-pos REALITY-BUBBLE-SIZE)
      tile (at pos)
      :if (isinstance tile hy.M.simalq/tile.Actor)]
    (.maybe-act tile))

  ; Now do end-of-turn processing.

  ; Dose the player with ambient poison, and convert an accumulated
  ; dose ≥1 into damage.
  (+= G.poison-dose G.level.poison-intensity)
  (setv dose-integer (.__floor__ G.poison-dose))
  (when dose-integer
    (hurt-player dose-integer DamageType.Poison)
    (-= G.poison-dose dose-integer))

  ; Advance the turn counter last.
  (+= G.turn-n 1))
