(require
  hyrule [unless]
  simalq.macros [pop-integer-part])
(import
  contextlib [contextmanager]
  simalq.util [CommandError message-queue msg hurt-player DamageType]
  simalq.color :as color
  simalq.geometry [burst at]
  simalq.game-state [G]
  simalq.tile.scenery [Scenery]
  simalq.tile.item [Item]
  simalq.tile.monster [Monster]
  simalq.quest [start-quest start-level]
  simalq.un-iq [read-quest iq-quest]
  simalq.commands [Action get-command do-command do-action]
  simalq.display [draw-screen bless-colorstr])
(setv  T True  F False)


(defn main [iq-quest-name [skip-to-level None]]
  ; `skip-to-level` is used for debugging, so for convenience, don't
  ; show titles when it's provided.
  (with [(player-io)]
    (start-quest (read-quest (iq-quest iq-quest-name))
      :show-title (not skip-to-level))
    (start-level
      :level-n (if skip-to-level (int skip-to-level) 1)
      :show-title (not skip-to-level))
    (main-io-loop)))


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
  (hurt-player :animate F
    (pop-integer-part G.player.poison-dose)
    DamageType.Poison)

  ; Advance the turn counter last.
  (+= G.turn-n 1))


(setv B None)

(defn [contextmanager] player-io []
  (global B)
  (setv B (hy.M.blessed.Terminal))
  (try
    (with [_ (B.cbreak)  _ (B.fullscreen)  _ (B.hidden-cursor)]
      (yield))
    (finally
      (setv B None))))

(defn displaying []
  (bool B))

(defn io-mode [draw on-input]
  "Enter a modal interface that alternates between the callbacks
  `draw` (nullary) and `on-input` (unary, taking a key from `inkey`).
  `on-input` can return the symbol `done` to exit the mode."

  (while True
    (draw)
    (when (= (on-input (inkey)) 'done)
      (break))))



(defn main-io-loop []
  (io-mode

    :draw (fn []
      (print-main-screen
        :focus G.player.pos
        :status-bar T
        :messages (tuple message-queue)))

    :on-input (fn [key]
      (setv (cut message-queue) #())
      (setv cmd (get-command key))
      (when (is cmd None)
        (return))
      (try
        (if (isinstance cmd Action)
          (take-turn cmd)
          (do-command cmd))
        (except [e CommandError]
          (msg (get e.args 0)))))))

(defn print-main-screen [focus status-bar [messages #()] [overmarks None]]
  (print
    :flush T :sep "" :end ""
    B.home B.clear
    (.join "\n" (map (fn [x] (bless-colorstr B x)) (draw-screen
      B.width B.height focus status-bar messages overmarks)))))


(defn inkey []
  ; Before checking for a key, flush standard input, so any keys
  ; pressed during an animation or slow processing are ignored instead
  ; of queued for input.
  (import sys termios)
  (termios.tcflush sys.stdin termios.TCIFLUSH)
  (B.inkey :esc-delay .01))


(setv max-wrap-cols 75)

(defn text-screen [text]
  (setv y-margin 2)

  (unless (displaying)
    (return))
  (io-mode
    :draw (fn []
      (print
        :flush T :sep "" :end ""
        B.home B.clear
        (* "\n" y-margin)
        (.join "\n" (lfor
          line (B.wrap
            text
            (min max-wrap-cols B.width))
          (.center line B.width)))))
    :on-input (fn [key]
      'done)))

(defn info-screen [t]
  "Enter an `io-mode` for showing information about the tile `t`."

  (setv x-margin (* 2 " "))
  (setv y-margin 1)

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
