(require
  hyrule [ecase]
  simalq.macros [defdataclass has])
(import
  simalq.util [ActionError]
  simalq.geometry [Pos GeometryError pos+ at]
  simalq.game-state [G]
  simalq.tile.scenery [Scenery])
(setv  T True  F False)


(defdataclass Action []
  "A discrete effect that the player can have on the gamestate.")
(defdataclass Walk [Action]
  "Try to walk one step in the given direction, or attack something
  that's in the way with your sword."
  [direction]
  :frozen T)


(defn do-action [action]
  (ecase (type action)
    Walk (do
      (setv d action.direction)
      (try
        (setv target (pos+ G.player-pos d))
        (except [e GeometryError]
          (raise (ActionError "The border of the dungeon blocks your movement.") :from e)))
      (when (has target Scenery it.blocks-move)
        (raise (ActionError "Your way is blocked.")))
      ; For diagonal movement, check that the two orthogonal neighbors
      ; are clear of diagonal blockers.
      (when (and
          d.x d.y
          (any (gfor
            p2 [
              (Pos target.map G.player-pos.x target.y)
              (Pos target.map target.x G.player-pos.y)]
            (has p2 Scenery it.blocks-diag))))
        (raise (ActionError "That diagonal is blocked by a neighbor.")))
      (for [tile (at G.player-pos)]
        (.hook-player-walk-from tile target))
      (for [tile (at target)]
        (when (.hook-player-walk-to tile G.player-pos)
          ; The hook returned true, meaning the player's action is
          ; over.
          (return)))
      ; No exceptions have stopped us, so go.
      (setv G.player-pos target)
      (for [tile (at target)]
        (.hook-player-walked-into tile)))))
