(require
  simalq.macros [defdataclass])
(setv  T True  F False)


(defdataclass Tile []
  [pos]
  :frozen T
  (setv types-by-iq-ix {}))


(import ; For side-effects.
  simalq.tile.scenery
  simalq.tile.not-implemented)
