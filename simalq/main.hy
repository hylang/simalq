(import
  copy [deepcopy]
  simalq.util [hurt-player DamageType]
  simalq.geometry [burst at]
  simalq.game-state [G Rules GameState]
  simalq.tile [Tile mv-tile]
  simalq.tile.player [Player]
  simalq.player-actions [do-action])
(setv  T True  F False)


(defn start-quest [quest]
  (setv
    G.rules (Rules)
    G.quest quest
    G.states []
    state (GameState))
  (for [thing [G.rules state] [k v] (.items thing.slot-defaults)]
    (setattr thing k v))
  (setv
    state.player (Player :pos None)
    state.player.hp quest.starting-hp)
  (.append G.states state)
  (setv G.state-i 0)
  (start-level 1))


(defn start-level [level-n]
  (setv
    G.level-n level-n
    G.level (deepcopy (get G.quest.levels (- level-n 1))))
      ; The default behavior of `deepcopy` is smart enough to make all
      ; the references to `G.level.map` in tiles point to the new map.
  (mv-tile G.player G.level.player-start))


(defn take-turn [action]
  (do-action action)

  (when G.player.just-exited
    ; If the player has changed levels, let her take another action
    ; this turn. This means she always gets the first action on a
    ; level.
    (setv G.player.just-exited F)
    (return))

  ; Allow actors in the reality bubble to act, in `burst`'s spiral
  ; order.
  (for [
      pos (burst G.player.pos G.rules.reality-bubble-size)
      tile (at pos)
      :if (isinstance tile hy.M.simalq/tile.Actor)]
    (.maybe-act tile))

  ; Now do end-of-turn processing.

  ; Dose the player with ambient poison, and convert an accumulated
  ; dose ≥1 into damage.
  (+= G.player.poison-dose G.level.poison-intensity)
  (setv dose-integer (.__floor__ G.player.poison-dose))
  (when dose-integer
    (hurt-player dose-integer DamageType.Poison)
    (-= G.player.poison-dose dose-integer))

  ; Advance the turn counter last.
  (+= G.turn-n 1))
