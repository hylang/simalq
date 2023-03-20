(require
  hyrule [ecase]
  simalq.macros [defdataclass has])
(import
  simalq.geometry [Pos GeometryError pos+ at]
  simalq.game-state [G])
(setv  T True  F False)


(defdataclass Action []
  "A discrete effect that the player can have on the gamestate.")
(defdataclass Move [Action]
  "Try to walk one step in the given direction, or attack something
  that's in the way with your sword."
  [direction]
  :frozen T)


(defn do-action [action]
  (ecase (type action)
    Move (do
      (setv d action.direction)
      (try
        (setv target (pos+ G.player-pos d))
        (except [e GeometryError]
          (raise ActionError :from e)))
      (when (has target it.blocks-move)
        (raise (ActionError "Your way is blocked.")))
      ; For diagonal movement, check that the two orthogonal neighbors
      ; are clear of diagonal blockers.
      (when (and
          d.x d.y
          (any (gfor
            p2 [
              (Pos target.map G.player-pos.x target.y)
              (Pos target.map target.x G.player-pos.y)]
            (has p2 it.blocks-diag))))
        (raise (ActionError "That diagonal is blocked by a neighbor.")))
      (setv G.player-pos target))))


(defclass ActionError [Exception])
