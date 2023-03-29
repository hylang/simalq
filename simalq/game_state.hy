(defclass GameState []
  (setv __slots__ [
    "quest" "map" "level_n" "player_pos"
    "score" "turn_n"
    "player_hp" "keys"])
  (defn [property] level [self]
    (get self.quest.levels (- self.level-n 1))))
(setv G (GameState))
