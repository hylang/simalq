(require
  simalq.macros [defdataclass])
(import
  simalq.game-state [G]
  simalq.tile [Tile deftile rm-tile])


(defdataclass Item [Tile]
  "An object the player can pick up."
  []

  (setv
    points None)
      ; How many points the player gets for picking up the item.

  (defn hook-player-walked-into [self]
    (rm-tile self)
    (+= G.score self.points)))


(deftile Item "a pile of gold"
  :iq-ix 18
  :points 100
  :flavor "Ooh, shiny.")

(deftile Item "a handful of gems"
  :iq-ix 109
  :points 250
  :flavor "Ooh, shinier.")
