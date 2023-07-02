(require
  simalq.macros [slot-defaults])
(import
  fractions [Fraction :as f/])
(eval-and-compile (setv  T True  F False))


(defclass Global []
  "All state information for the game being played, including an undo
  history of individual `GameState`s."

  (slot-defaults
    rules None
      ; A `Rules` object. It shouldn't be mutated mid-game, or the
      ; game-state history can get desynchronized.
    quest None
      ; A `Quest` object. It shouldn't be mutated, so fresh copies
      ; of each level can be retrieved from it.
    states None
      ; A history of `GameState` objects.
    state-i None)
      ; An index of `states`, pointing to the current state. Typically
      ; its value is `(- (len states) 1)`, but it's decremented when
      ; states are undone.

  ; Attributes of `GameState` can be treated as attributes of
  ; `Global`. Such access is passed through to the current state.
  (defn __getattr__ [self name]
    (if (in name GameState.__slots__)
      (getattr (get self.states self.state-i) name)
      (object.__getattribute__ self name)))
  (defn __setattr__ [self name value]
    (if (in name GameState.__slots__)
      (setattr (get self.states self.state-i) name value)
      (object.__setattr__ self name value)))

  (defn [property] map [self]
    (. self states [self.state-i] level map)))

(setv G (Global))
  ; This instance is the active `Global` object. It should only be
  ; modified in place, not reassigned.


(defclass GameState []
  "A state in which the game is ready to receive another action from
  the player."

  (slot-defaults
    action None
      ; The player's `Action` that produced this game state from the
      ; previous one. It's `None` only for the initial state.
    level None
      ; A `Level` object. This is mutated to represent the level
      ; changing, such as monsters moving around.
    level-n None
      ; An integer indicating the level we're currently playing.
      ; Numbered starting from 1.
    each-turn []
      ; Objects on which to call `.each-turn` at the end of each turn.
    score 0
      ; How many points the player has accumulated.
    turn-n 0
      ; The number of rounds that have elapsed so far. This usually
      ; increments by 1 between successive states, but not always, due
      ; to effects that give the player extra actions.
    player None))
      ; A `Player` object.


(defclass Rules []
  (slot-defaults
    ; The individual rules and their default values. All defaults
    ; are per IQ.
      reality-bubble-size 6
        ; The reality bubble is the (Chebyshev) radius around the
        ; player in which monsters etc. get to act. It's a square
        ; spanning `2 * reality-bubble-size + 1` map squares on each
        ; side, with the player in the center.
      max-keys 8
        ; How many keys the player can carry at once.
      max-usables 3
        ; How many inventory slots the player has for wands etc.
      player-melee-damage-base 2
        ; How much damage the player does with her sword normally.
      player-melee-damage-artifact 3
        ; How much with the relevant artifact.
      player-shot-damage-base 1
        ; How much damage the player does with her bow normally.
      player-shot-damage-artifact 2
        ; How much with the relevant artifact (and no magic arrows).
      player-shot-damage-magic 3
        ; How much damage the player does with magic arrows.
      magic-arrows-pickup-size 10
        ; How many magic arrows come in a pickup.
      artifact-shield-factor (f/ 3 4)
        ; How much the artifact shield multiplies incoming damage by.
      dainty-monsters T))
        ; Whether monsters will only step on empty floor (with some
        ; exceptions, like spiders walking on webs). Otherwise,
        ; monsters obey similar rules as the player does regarding
        ; blocking tiles.
