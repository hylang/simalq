(import
  copy [deepcopy]
  blessed
  simalq.util [ActionError hurt-player DamageType]
  simalq.color :as color
  simalq.geometry [burst at]
  simalq.game-state [G Rules GameState]
  simalq.tile [Tile mv-tile]
  simalq.tile.player [Player]
  simalq.un-iq [read-quest iq-quest]
  simalq.player-actions [do-action get-action]
  simalq.display [draw-map])
(setv  T True  F False)


(defn main [iq-quest-name]
  (start-quest (read-quest (iq-quest iq-quest-name)))
  (main-io-loop))


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


(defn main-io-loop []
  (setv B (blessed.Terminal))

  (with [_ (B.cbreak)  _ (B.fullscreen)  _ (B.hidden-cursor)]
    (while True

      ; Print the map.
      (print
        :flush T :sep "" :end ""
        B.home B.clear
        (.join "" (gfor
          [color-fg color-bg mapsym] (draw-map B.width B.height)
          ((B.on-color-rgb #* (get color.by-name color-bg))
            ((B.color-rgb #* (get color.by-name color-fg))
              mapsym)))))

      ; Get input.
      (while
        (try
          (while (not (setx action (get-action (B.inkey)))))
          (take-turn action)
          (break)
          (except [e ActionError]
            ; ActionErrors are silent, for now.
            (continue)))))))
