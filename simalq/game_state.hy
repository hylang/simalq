(require
  hyrule [unless]
  simalq.macros [slot-defaults])
(import
  copy [deepcopy]
  fractions [Fraction :as f/])
(setv  T True  F False)


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
    state None
      ; The current `GameState`.
    states None
      ; A history of `GameState`s.
    state-i None)
      ; An index of `states`, pointing to the predecessor of `state`.
      ; Typically its value is just the last index of `states`, but it's
      ; decremented when states are undone.

  ; For convenience, attributes of `GameState` can be treated as
  ; attributes of `Global`. Such access is passed through to the
  ; current state.
  (defn __getattr__ [self name]
    (if (in name (+ GameState.__slots__ HydratedGameState.__slots__))
      (getattr self.state name)
      (object.__getattribute__ self name)))
  (defn __setattr__ [self name value]
    (if (in name (+ GameState.__slots__ HydratedGameState.__slots__))
      (setattr self.state name value)
      (object.__setattr__ self name value)))

  (defn [property] map [self]
    (. self state level map))

  (defn initialize-states [self]
    "Clear `GameState` attributes and start fresh. `rules` should
    already be set."

    (setv self.state (HydratedGameState :action None))
    (for [[k v] (.items HydratedGameState.slot-defaults)]
      (setattr self.state k (deepcopy v)))
    (setv self.states [])
    (setv self.state-i None))

  (defn advance-states [self]
    "Increment the state counter and save the current state to the
    history."

    (if (is self.state-i None)
      (setv self.state-i 0)
      (+= self.state-i 1))
    (defn copy-state []
      (if (% self.state-i self.rules.state-dehydration-factor)
        (DehydratedGameState :action self.state.action)
        (deepcopy self.state)))
    (cond
      (= self.state-i (len self.states))
        ; We're at the end of the undo history, so append a copy of
        ; the new state.
        (.append self.states (copy-state))
      (= (. self.states [self.state-i] action) self.state.action)
        ; We should effectively be redoing this state, since it's the
        ; same action as last time and the game is deterministic.
        ; Keep the remaining history for further redoing.
        None
      True
        ; We're branching off in a new direction. Discard the now-
        ; obsolete redo history. (We don't support a full-blown tree
        ; of states, just a timeline.)
        (setv (cut self.states self.state-i None) [(copy-state)])))

  (defn set-state-i [self target-i]
    "Undo or redo to a given index in the state history."

    (import
      simalq.main [suppress-display take-turn])

    (unless (chainc 0 <= target-i < (len self.states))
      (raise (ValueError f"Illegal state index {target-i}")))

    ; Search for the most recent hydrated game state before the target
    ; state. This search should never fail because state 0 is always
    ; hydrated.
    (setv last-hydrated-i (next (gfor
      i (range target-i -1 -1)
      :if (isinstance (get self.states i) HydratedGameState)
      i)))
    ; Run the whole game (with display turned off) from the hydrated
    ; state to the target state.
    (setv self.state-i last-hydrated-i)
    (setv self.state (deepcopy (get self.states self.state-i)))
    (with [(suppress-display)]
      (for [i (range (+ last-hydrated-i 1) (+ target-i 1))]
        (take-turn (. self.states [i] action))))))


(setv G (Global))
  ; This instance is the active `Global` object. It should only be
  ; modified in place, not reassigned.


(defclass GameState []
  "An abstract base class for a state in which the game is ready to
  receive another action from the player."

  (slot-defaults
    action None)
      ; The player's `Action` that produced this game state from the
      ; previous one. It's `None` only for the initial state.

  (defn __init__ [self action]
    (setv self.action action)))

(defclass HydratedGameState [GameState]
  "A fully elaborated game state."

  (slot-defaults
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

(defclass DehydratedGameState [GameState]
  "A lighter-weight substitute for a `HydratedGameState`."

  (setv __slots__ []))


(defclass Rules []
  (slot-defaults
    ; The individual rules and their default values. All defaults
    ; are per IQ when applicable.
      state-dehydration-factor 100
        ; State 0, and every `state-dehydration-factor` states
        ; thereafter, are stored as `HydratedGameState`s. The rest are
        ; stored as `DehydratedGameState`s.
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
