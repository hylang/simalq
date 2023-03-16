(require
  hyrule [ecase]
  simalq.macros [defdataclass])
(import
  simalq.geometry [GeometryError pos+]
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
    Move
      (try
        (setv G.player-pos (pos+ G.player-pos action.direction))
        (except [e GeometryError]
          (raise ActionError :from e)))))


(defclass ActionError [Exception])
