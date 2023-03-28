(import
  simalq.game-state [G])


(defn start-quest [quest]
  (setv
    G.quest quest
    G.score 0
    G.keys 0)
  (start-level 1))


(defn start-level [level-n]
  (setv
    G.level-n level-n
    level (get G.quest.levels (- G.level-n 1))
    G.map level.map
    G.player-pos level.player-start))
