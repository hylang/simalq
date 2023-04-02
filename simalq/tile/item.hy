(import
  simalq.util [ActionError]
  simalq.game-state [G]
  simalq.tile [Tile deftile rm-tile])


(defclass Item [Tile]
  "An object the player can pick up."

  (setv
    __slots__ []
    points None)
      ; How many points the player gets for picking up the item.

  (defn hook-player-walked-into [self]
    (rm-tile self)
    (+= G.score self.points)
    (.pick-up self))
  (defn pick-up [self]))


(deftile Item "a pile of gold"
  :iq-ix 18
  :points 100
  :flavor "Ooh, shiny.")

(deftile Item "a handful of gems"
  :iq-ix 109
  :points 250
  :flavor "Ooh, shinier.")


(deftile Item "a key"
  :iq-ix 19
  :points 50

  :hook-player-walk-to (fn [self origin]
    (when (>= G.keys G.rules.max-keys)
      (raise (ActionError "Your keyring has no room for another key."))))
  :pick-up (fn [self]
    (+= G.keys 1)
    (assert (<= G.keys G.rules.max-keys)))

  :flavor "Idok uses only the worst locks and keys that money can buy. The keys are bulky and heavy, yet immediately snap into pieces on being used once, and every lock can be opened by any old key.")
