(require
  hyrule [unless]
  simalq.macros [defdataclass])
(import
  simalq.game-state [G Rules]
  simalq.util [StatusEffect refactor-hp GameOverException]
  simalq.tile [mv-tile]
  simalq.tile.player [Player]
  copy [deepcopy])
(setv  T True  F False)


(defdataclass Quest []
  "A scenario or campaign; a sequence of levels to play."
  [name title authors starting-hp levels]
  :frozen T)

(defn start-quest [quest [rules None]]
  (setv
    G.rules (Rules)
    G.quest quest)
  (when rules
    (for [[k v] (.items rules)]
      (setattr G.rules k v)))
  (.initialize-states G)
  (setv
     G.player (Player :pos None)
     G.player.hp (or (when quest (refactor-hp quest.starting-hp)) 1)
     (cut G.player.inventory) (* [None] G.rules.max-usables))
  (.update G.player.status-effects (dfor  x StatusEffect  x 0)))


(defdataclass Level []
  "A map and associated data for playing it."
  [n title player-start next-level
    poison-intensity time-limit exit-speed moving-exit-start
    map]
  ; Poison intensity is a fraction.Fraction, the amount of poison to
  ; dose the player with per turn, which converts to poison damage
  ; once it gets ≥ 1.
  :frozen T)

(defn start-level [level-n [show-title T]]
  (when (> level-n (len G.quest.levels))
    (raise (GameOverException 'won)))
  (setv level (get G.quest.levels (- level-n 1)))
  (when show-title
    (hy.I.simalq/main.text-screen :center T
      f"Level {level-n}\n\n{level.title}"))
  (setv
    G.level-n level-n
    G.level (deepcopy level)
      ; The default behavior of `deepcopy` is smart enough to make all
      ; the references to `G.level.map` in tiles point to the new map.
    G.time-left G.level.time-limit)
  (mv-tile G.player G.level.player-start)
  (unless G.states
    ; If we haven't saved any states yet (because the game just
    ; started), save this as the first state in the history.
    (.advance-states G)))
