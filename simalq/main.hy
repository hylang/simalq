(import
  copy [deepcopy]
  fractions [Fraction]
  simalq.util [hurt-player DamageType]
  simalq.geometry [burst at]
  simalq.game-state [G Rules]
  simalq.player-actions [do-action])


(defn start-quest [quest]
  (setv
    G.rules (Rules)
    G.quest quest
    G.score 0
    G.turn-n 0
    G.player-hp quest.starting-hp
    G.poison-dose (Fraction 0)
    G.keys 0)
  (for [[k v] (.items Rules.slot-defaults)]
    (setattr G.rules k v))
  (start-level 1))


(defn start-level [level-n]
  (setv
    G.level-n level-n
    G.level (deepcopy (get G.quest.levels (- level-n 1)))
      ; The default behavior of `deepcopy` is smart enough to make all
      ; the references to `G.level.map` in tiles point to the new map.
    G.player-pos G.level.player-start))


(defn take-turn [action]
  (setv level-was G.level)
  (do-action action)

  (when (is-not G.level level-was)
    ; If the player has changed levels, let her take another action
    ; this turn. This means she always gets the first action on a
    ; level.
    (return))

  ; Allow actors in the reality bubble to act, in `burst`'s spiral
  ; order.
  (for [
      pos (burst G.player-pos G.rules.reality-bubble-size)
      tile (at pos)
      :if (isinstance tile hy.M.simalq/tile.Actor)]
    (.maybe-act tile))

  ; Now do end-of-turn processing.

  ; Dose the player with ambient poison, and convert an accumulated
  ; dose ≥1 into damage.
  (+= G.poison-dose G.level.poison-intensity)
  (setv dose-integer (.__floor__ G.poison-dose))
  (when dose-integer
    (hurt-player dose-integer DamageType.Poison)
    (-= G.poison-dose dose-integer))

  ; Advance the turn counter last.
  (+= G.turn-n 1))
