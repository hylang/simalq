(require
  simalq.macros [defdataclass])
(import
  simalq.game-state [G Rules GameState]
  simalq.tile [mv-tile]
  simalq.tile.player [Player]
  copy [deepcopy])
(setv  T True  F False)


(defdataclass Quest []
  "A scenario or campaign; a sequence of levels to play."
  [title starting-hp levels]
  :frozen T)

(defn start-quest [quest [show-title T]]
  (when show-title
    (hy.M.simalq/main.text-screen quest.title))
  (setv
    G.rules (Rules)
    G.quest quest
    G.states []
    state (GameState))
  (for [thing [G.rules state] [k v] (.items thing.slot-defaults)]
    (setattr thing k v))
  (setv
    state.player (Player :pos None)
    state.player.hp quest.starting-hp
    (cut state.player.inventory) (* [None] G.rules.max-usables))
  (.append G.states state)
  (setv G.state-i 0))


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
  (setv level (get G.quest.levels (- level-n 1)))
  (when show-title
    (hy.M.simalq/main.text-screen f"Level {level-n}\n\n{level.title}"))
  (setv
    G.level-n level-n
    G.level (deepcopy level))
      ; The default behavior of `deepcopy` is smart enough to make all
      ; the references to `G.level.map` in tiles point to the new map.
  (mv-tile G.player G.level.player-start))
