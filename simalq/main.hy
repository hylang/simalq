(import
  simalq.util [hurt-player DamageType]
  simalq.game-state [G]
  simalq.player-actions [do-action])


(defn start-quest [quest]
  (setv
    G.quest quest
    G.score 0
    G.turn-n 0
    G.player-hp quest.starting-hp
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

  ; End-of-turn processing.
  (when (and
      (is-not G.level.poison-interval None)
      (= (% (+ G.turn-n 1) G.level.poison-interval) 0))
        ; `G.turn-n` is incremented here so that the first poison
        ; damage occurs after the poison counter has elapsed once.
    (hurt-player 1 DamageType.Poison))

  ; Advance the turn counter last.
  (+= G.turn-n 1))
