"The quest and level types."


(require
  hyrule [unless]
  simalq.macros [defdataclass])
(import
  simalq.game-state [G Rules]
  simalq.util [StatusEffect refactor-hp GameOverException]
  simalq.tile.player [Player]
  copy [deepcopy])
(setv  T True  F False)


(defdataclass Quest []
  "A scenario or campaign; a sequence of levels to play."
  :fields [
    name authors title ; Textual metadata
    starting-hp        ; An integer
    levels]            ; A tuple of `Level` objects
  :frozen T)

(defn start-quest [quest [rules None] [show-title T]]
  "Initialize the global state for playing the given quest."
  (setv
    G.rules (Rules)
    G.quest quest)
  (when show-title
    (hy.I.simalq/main.text-screen :center T
      f"{quest.name}\nby {quest.authors}\n\n{quest.title}"))
  (when rules
    (for [[k v] (.items rules)]
      (setattr G.rules k v)))
  (.initialize-states G)
  (setv G.player (Player :pos None))
  (setv G.player.hp (refactor-hp quest.starting-hp)))


(defdataclass Level []
  "A map and associated data for playing it."
  :fields [
    n
      ; The level number. The first item (i.e., index 0) in
      ; `Quest.levels` must have level number 1, the next (index 1)
      ; must have level number 2, and so on.
    title
      ; Free text. It can be up to several sentences describing the
      ; level, rather than just a name.
    player-start
      ; A `Pos`.
    next-level
      ; The level number of the level to go to if the player uses a
      ; regular exit, or the time limit expires.
    poison-intensity
      ; A `fraction.Fraction`, the dose of ambient poison per turn.
    time-limit
      ; An integer (counting in turns), or `None`.
    exit-speed moving-exit-start
      ; These fields would be used to implement moving exits, but
      ; moving exits are not yet implemented, so they're ignored.
    map])
      ; A `Map`.

(defn start-level [level-n [show-title T]]
  "Begin playing the level of the given number. If `level-n` is
  greater than the number of levels in this quest, the player wins the
  game."
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
  (.move G.player G.level.player-start)
  (unless G.states
    ; If we haven't saved any states yet (because the quest just
    ; started), save this as the first state in the history.
    (.advance-states G)))
