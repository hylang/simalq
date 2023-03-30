(require
  simalq.macros [defdataclass])
(setv  T True  F False)


(defdataclass Quest []
  "A scenario or campaign; a sequence of levels to play."
  [title starting-hp levels]
  :frozen T)


(defdataclass Level []
  "A map and associated data for playing it."
  [n title player-start next-level
    poison-intensity time-limit exit-speed moving-exit-start
    map]
  ; Poison intensity is a fraction.Fraction, the amount of poison to
  ; dose the player with per turn, which converts to poison damage
  ; once it gets ≥ 1.
  :frozen T)
