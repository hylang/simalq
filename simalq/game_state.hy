(require
  hyrule [unless]
  simalq.macros [defdataclass defmeth])
(import
  copy [deepcopy]
  fractions [Fraction :as f/])
(setv  T True  F False)


(defdataclass Global []
  "All state information for the game being played, including an undo
  history of individual `GameState`s."

  :field-defaults {
    rules None
      ; A `Rules` object. It shouldn't be mutated mid-game, or the
      ; game-state history can get desynchronized.
    quest None
      ; A `Quest` object. It shouldn't be mutated, so fresh copies
      ; of each level can be retrieved from it.
    state None
      ; The current `GameState`.
    states []
      ; A history of `GameState`s.
    state-i None}
      ; An index of `states`, pointing to the predecessor of `state`.
      ; Typically its value is just the last index of `states`, but it's
      ; decremented when states are undone.

  ; For convenience, attributes of `GameState` can be treated as
  ; attributes of `Global`. Such access is passed through to the
  ; current state.
  (defmeth __getattr__ [name]
    (if (in name (+ GameState.__slots__ HydratedGameState.__slots__))
      (getattr @state name)
      (object.__getattribute__ @ name)))
  (defmeth __setattr__ [name value]
    (if (in name (+ GameState.__slots__ HydratedGameState.__slots__))
      (setattr @state name value)
      (object.__setattr__ @ name value)))

  (defmeth [property] map []
    @state.level.map)

  (defmeth initialize-states []
    "Clear `GameState` attributes and start fresh. `rules` should
    already be set."

    (setv @state (HydratedGameState))
    (setv @states [])
    (setv @state-i None))

  (defmeth advance-states []
    "Increment the state counter and save the current state to the
    history."

    (if (is @state-i None)
      (setv @state-i 0)
      (+= @state-i 1))
    (defn copy-state []
      (if (% @state-i @rules.state-dehydration-factor)
        (GameState :action @state.action)
        (deepcopy @state)))
    (cond
      (= @state-i (len @states))
        ; We're at the end of the undo history, so append a copy of
        ; the new state.
        (.append @states (copy-state))
      (= (. @states [@state-i] action) @state.action)
        ; We should effectively be redoing this state, since it's the
        ; same action as last time and the game is deterministic.
        ; Keep the remaining history for further redoing.
        None
      True
        ; We're branching off in a new direction. Discard the now-
        ; obsolete redo history. (We don't support a full-blown tree
        ; of states, just a timeline.)
        (setv (cut @states @state-i None) [(copy-state)])))

  (defmeth set-state-i [target-i]
    "Undo or redo to a given index in the state history."

    (import
      simalq.main [suppress-display take-turn])

    (unless (chainc 0 <= target-i < (len @states))
      (raise (ValueError f"Illegal state index {target-i}")))

    ; Search for the most recent hydrated game state before the target
    ; state. This search should never fail because state 0 is always
    ; hydrated.
    (setv last-hydrated-i (next (gfor
      i (range target-i -1 -1)
      :if (isinstance (get @states i) HydratedGameState)
      i)))
    ; Run the whole game (with display turned off) from the hydrated
    ; state to the target state.
    (setv @state-i last-hydrated-i)
    (setv @state (deepcopy (get @states @state-i)))
    (with [(suppress-display)]
      (for [i (range (+ last-hydrated-i 1) (+ target-i 1))]
        (take-turn (. @states [i] action))))))


(defdataclass GameState []
  #[[A state in which the game is ready to receive another action
  from the player. Instances of this class but not `HydratedGameState`
  are "dehydrated" game states.]]

  :field-defaults {action None})
    ; The player's `Action` that produced this game state from the
    ; previous one. It's `None` only for the initial state.


(defdataclass HydratedGameState [GameState]
  "A fully elaborated game state."

  :field-defaults {
    level None
      ; A `Level` object. This is mutated to represent the level
      ; changing, such as monsters moving around.
    level-n None
      ; An integer indicating the level we're currently playing.
      ; Numbered starting from 1, so `(get G.quest.levels n)` is
      ; counted as level `(- n 1)`.
    score 0
      ; How many points the player has accumulated.
    turn-n 0
      ; The number of rounds that have elapsed so far. This usually
      ; increments by 1 between successive states, but not always, due
      ; to effects that give the player extra actions.
    time-left None
      ; The number of turns remaining on the current level's time limit
      ; (if it has one).
    player None})
      ; A `Player` object.


(defdataclass Rules []

  :field-defaults {
    ; All defaults are per IQ when applicable.
    state-dehydration-factor 100
      ; State 0, and every `state-dehydration-factor` states
      ; thereafter, are stored as `HydratedGameState`s. The rest are
      ; stored as plain `GameState`s.
    reality-bubble-size 6
      ; The reality bubble is the (Chebyshev) radius around the
      ; player in which monsters etc. get to act. It's a square
      ; spanning `(+ (* 2 reality-bubble-size) 1)` map squares on each
      ; side, with the player in the center.
    player-hp-factor (f/ 1)
      ; Multiplies the player's starting HP and healing.
    poison-factor (f/ 1)
      ; Multiplies all ambient poison rates.
    max-keys 8
      ; How many keys the player can carry at once.
    max-usables 3
      ; How many inventory slots the player has for wands etc.
    player-melee-damage-base 2
      ; How much damage the player does with her sword normally.
    player-melee-damage-artifact 3
      ; How much with the relevant artifact.
    player-melee-damage-weakness-reduction 1
      ; How much the player's sword damage is reduced by while she's
      ; weak.
    player-shot-damage-base 1
      ; How much damage the player does with her bow normally.
    player-shot-damage-artifact 2
      ; How much with the relevant artifact (and no magic arrows).
    player-shot-damage-magic 3
      ; How much damage the player does with magic arrows.
    player-poison-damage 3
      ; How much damage the player does with a poisonous aura.
    artifact-shield-factor (f/ 3 4)
      ; How much the artifact shield multiplies incoming damage by.
    paralysis-duration 3
      ; How many rounds paralysis lasts for.
    poison-emitter-damage 2
      ; Damage per turn (to the player) from poisonous fountains.
    dainty-monsters T})
      ; Whether monsters will only step on empty floor (with some
      ; exceptions, like spiders walking on webs). Otherwise, monsters
      ; can move through items and some kinds of scenery.


(setv G (Global))
  ; This instance is the active `Global` object. It should only be
  ; modified in place, not reassigned.
