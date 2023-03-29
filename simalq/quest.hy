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
    poison-interval time-limit exit-speed moving-exit-start
    map]
  :frozen T)
