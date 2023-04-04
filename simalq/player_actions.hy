(require
  hyrule [ecase]
  simalq.macros [defdataclass])
(import
  simalq.util [ActionError]
  simalq.geometry [at]
  simalq.game-state [G]
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
  (ecase (type action)

    Wait
      ; Nothing to do. This action should always succeed.
      None

    Walk (do
      (setv d action.direction)
      (setv [target wly] (walkability G.player-pos d :monster? F))
      (when (= wly 'out-of-bounds)
        (raise (ActionError "The border of the dungeon blocks your movement.")))
      (when (= wly 'blocked-diag)
        (raise (ActionError "That diagonal is blocked by a neighbor.")))
      (for [tile (at target)]
        (when (.hook-player-bump tile G.player-pos)
          (return)))
      (when (= wly 'bump)
        (raise (ActionError "Your way is blocked.")))
      (for [tile (at G.player-pos)]
        (.hook-player-walk-from tile target))
      (for [tile (at target)]
        (.hook-player-walk-to tile G.player-pos))
      ; No exceptions have stopped us, so go.
      (setv G.player-pos target)
      (for [tile (at target)]
        (when (.hook-player-walked-into tile)
          (return))))))
