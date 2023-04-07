(require
  simalq.macros [slot-defaults])
(eval-and-compile (setv  T True  F False))


(defclass GameState []
  (slot-defaults
    rules None
    quest None
    level None
    level-n None
    score 0
    turn-n 0
    player None
    player-hp 1
    keys 0)
  (defn [property] map [self]
    self.level.map))
(setv G (GameState))
(for [[k v] (.items G.slot-defaults)]
  (setattr G k v))


(defclass Rules []
  (slot-defaults
    ; The individual rules and their default values. All defaults
    ; are per IQ.
      reality-bubble-size 6
        ; The reality bubble is the (Chebyshev) radius around the
        ; player in which monsters etc. get to act. It's a square
        ; spanning `2 * reality-bubble-size + 1` squares on each side,
        ; with the player in the center.
      max-keys 8
        ; How many keys the player can carry at once.
      base-player-melee-damage 2
        ; How much damage the player does with her sword normally.
      dainty-monsters T))
        ; Whether monsters will only step on empty floor (with some
        ; exceptions, like spiders walking on webs). Otherwise,
        ; monsters obey similar rules as the player does regarding
        ; blocking tiles.
