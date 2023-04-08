(require
  hyrule [ecase]
  simalq.macros [defdataclass])
(import
  copy [deepcopy]
  simalq.util [ActionError]
  simalq.geometry [at]
  simalq.game-state [G]
  simalq.tile [mv-tile]
  simalq.tile.scenery [walkability])
(setv  T True  F False)


(defdataclass Action []
  "A discrete effect that the player can have on the gamestate.")
(defdataclass Wait [Action]
  "Do nothing, passing your turn (or, more precisely, passing
  one opportunity for an action).")
(defdataclass Walk [Action]
  "Try to walk one step in the given direction, or attack something
  that's in the way with your sword."
  [direction]
  :frozen T)


(defn do-action [action]
  ; Set up a new game state.
  (setv new-state (deepcopy (get G.states G.state-i)))
  (+= G.state-i 1)
  (cond
    (= G.state-i (len G.states))
      ; We're at the end of the undo history, so append the new state.
      (.append G.states new-state)
    (= (. G.states [G.state-i] action) action)
      ; We should effectively be redoing this state, since it's the
      ; same action as last time and the game is deterministic.
      ; Keep the remaining history for further redoing.
      (setv (get G.states G.state-i) new-state)
    True
      ; We're branching off in a new direction. Discard the now-
      ; obsolete redo history. (We don't support a full-blown tree
      ; of states, just a line.)
      (setv (cut G.states G.state-i None) [new-state]))
  (setv G.action action)
  (try
    (_execute-action action)
    (except [ActionError]
      ; No action occurred. Abort the new game state before bubbling
      ; the exception up.
      (del (get G.states G.state-i))
      (-= G.state-i 1)
      (raise))))

(defn _execute-action [action]
  (ecase (type action)

    Wait
      ; Nothing to do. This action should always succeed.
      None

    Walk (do
      (setv d action.direction)
      (setv [target wly] (walkability G.player.pos d :monster? F))
      (when (= wly 'out-of-bounds)
        (raise (ActionError "The border of the dungeon blocks your movement.")))
      (when (= wly 'blocked-diag)
        (raise (ActionError "That diagonal is blocked by a neighbor.")))
      (for [tile (at target)]
        (when (.hook-player-bump tile G.player.pos)
          (return)))
      (when (= wly 'bump)
        (raise (ActionError "Your way is blocked.")))
      (for [tile (at G.player.pos)]
        (.hook-player-walk-from tile target))
      (for [tile (at target)]
        (.hook-player-walk-to tile G.player.pos))
      ; No exceptions have stopped us, so go.
      (mv-tile G.player target)
      (for [tile (at target)]
        (when (.hook-player-walked-into tile)
          (return))))))
