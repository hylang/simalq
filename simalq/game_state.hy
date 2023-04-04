(eval-and-compile (setv  T True  F False))


(defclass GameState []
  (setv __slots__ [
    "rules"
    "quest" "level" "level_n" "player_pos"
    "score" "turn_n"
    "player_hp" "poison_dose" "keys"])
  (defn [property] map [self]
    self.level.map))
(setv G (GameState))


(defclass Rules []
  (do-mac
    (setv slot-defaults (dict
      ; The individual rules and their default values. All defaults
      ; are per IQ.
      :reality-bubble-size 6
        ; The reality bubble is the (Chebyshev) radius around the
        ; player in which monsters etc. get to act. It's a square
        ; spanning `reality-bubble-size` + 1 squares on each side,
        ; with the player in the center.
      :max-keys 8
        ; How many keys the player can carry at once.
      :base-player-melee-damage 2
        ; How much damage the player does with her sword normally.
      :dainty-monsters T))
        ; Whether monsters will only step on empty floor (with some
        ; exceptions, like spiders walking on webs). Otherwise,
        ; monsters obey similar rules as the player does regarding
        ; blocking tiles.
    `(setv
      __slots__ ~(list (.keys slot-defaults))
      slot-defaults ~slot-defaults)))
