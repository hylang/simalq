(require
  simalq.macros [slot-defaults])
(eval-and-compile (setv  T True  F False))


(defclass GameState []
  (slot-defaults
    rules None
      ; A `Rules` object.
    quest None
      ; A `Quest` object. It shouldn't be mutated, so fresh copies
      ; of each level can be retrieved from it.
    level None
      ; A `Level` object. This is mutated to represent the level
      ; changing, such as monsters moving around.
    level-n None
      ; An integer indicating the level we're currently playing.
      ; Numbered starting from 1.
    score 0
      ; How many points the player has accumulated.
    turn-n 0
      ; The number of rounds that have elapsed so far. This usually
      ; increments by 1 between successive states, but not always, due
      ; to effects that give the player extra actions.
    player None)
      ; A `Player` object.

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
