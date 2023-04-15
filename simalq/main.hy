(import
  copy [deepcopy]
  blessed
  simalq.util [CommandError hurt-player DamageType]
  simalq.color :as color
  simalq.geometry [burst at]
  simalq.game-state [G Rules GameState]
  simalq.tile [Tile mv-tile]
  simalq.tile.player [Player]
  simalq.tile.scenery [Scenery]
  simalq.tile.item [Item]
  simalq.tile.monster [Monster]
  simalq.un-iq [read-quest iq-quest]
  simalq.commands [Action get-command do-command do-action]
  simalq.display [draw-screen bless-colorstr])
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


(setv B None)

(defn io-mode [draw on-input]
  "Enter a modal interface that alternates between the callbacks
  `draw` (nullary) and `on-input` (unary, taking a key from
  `blessed.Terminal.inkey`. `on-input` can return the symbol `done`
  to exit the mode."

  (global B)
  (setv top-io-mode F)
  (when (is B None)
    (setv B (blessed.Terminal))
    (setv top-io-mode T))

  (defn f []
    (while True
      (draw)
      (when (= (on-input (B.inkey)) 'done)
        (break))))

  (if top-io-mode
    (with [_ (B.cbreak)  _ (B.fullscreen)  _ (B.hidden-cursor)]
      (f))
    (f))

  (when top-io-mode
    (del B)))


(defn main-io-loop []
  (setv message None)
  (io-mode

    :draw (fn []
      (nonlocal message)
      ; Draw the screen.
      (print-main-screen
        :focus G.player.pos
        :status-bar T
        :message message)
      ; Clear the message buffer.
      (setv message None))

    :on-input (fn [key]
      (setv cmd (get-command key))
      (when (is cmd None)
        (return))
      (try
        (if (isinstance cmd Action)
          (take-turn cmd)
          (do-command cmd))
        (except [e CommandError]
          (nonlocal message)
          (setv message (get e.args 0)))))))

(defn print-main-screen [focus status-bar [message None]]
  (print
    :flush T :sep "" :end ""
    B.home B.clear
    (bless-colorstr B (draw-screen
      B.width B.height focus status-bar message))))


(defn info-screen [t]
  "Enter an `io-mode` for showing information about the tile `t`."

  (setv x-margin (* 2 " "))
  (setv y-margin 1)
  (setv max-wrap-cols 75)

  (import
    re
    simalq.display [color-tile])

  (defn wrapped [x]
    (lfor
      line (B.wrap
        x
        (min max-wrap-cols (- B.width (len x-margin))))
      (+ x-margin line)))

  (setv lines [
    #* (* [""] y-margin)
    (+ x-margin (bless-colorstr B (color-tile t)) "  " t.full-name)
    ""
    (+ x-margin (B.bold (next (gfor
      cls [Scenery Item Monster (type t)]
      :if (isinstance t cls)
      cls.__name__))))
    ""
    #* (gfor
      bullet (.info-bullets t)
      :if bullet
      line (wrapped (re.sub r"\s+" " " (if (isinstance bullet tuple)
        (+
          (B.bold f"• {(get bullet 0)}:")
          " " (str (get bullet 1)))
        (B.bold f"• {bullet}"))))
      line)
    ""
    #* (wrapped t.flavor)])

  (setv top-line-ix 0)

  (io-mode

    :draw (fn []
      (print
        :flush T :sep "" :end ""
        B.home B.clear
        (.join "\n" (cut lines
          top-line-ix
          (+ top-line-ix B.height)))))

    :on-input (fn [key]
      ; Advance `top-line-ix` (i.e., scroll the screen), or exit if
      ; we've shown it all.
      (nonlocal top-line-ix)
      (setv new-ix (min
        (+ top-line-ix B.height)
        (max 0 (- (len lines) B.height))))
      (if (= new-ix top-line-ix)
        'done
        (setv top-line-ix new-ix)))))
