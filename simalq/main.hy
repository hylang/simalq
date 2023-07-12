(require
  hyrule [unless ecase]
  simalq.macros [pop-integer-part])
(import
  time [sleep]
  contextlib [contextmanager]
  simalq.util [CommandError message-queue msg hurt-player DamageType player-status GameOverException menu-letters]
  simalq.color :as color
  simalq.geometry [burst at]
  simalq.game-state [G]
  simalq.tile [Tile]
  simalq.quest [start-quest start-level]
  simalq.commands [Action get-command do-command do-action]
  simalq.display [draw-screen bless-colorstr]
  simalq.save-load [get-saves-list load-game])
(setv  T True  F False)


(defn main [quest [skip-to-level None] [load-main-save F]]
  (start-quest quest)
  (with [(player-io)]
    (if (and
        load-main-save
        (setx save (next
          (gfor  item (get (get-saves-list) 0)  :if (get item "main")  item)
          None)))
      (do
        (load-game (get save "path"))
        (when skip-to-level
          (start-level
            :level-n skip-to-level
            :show-title F)))
      (do
        ; `skip-to-level` is used for debugging, so for convenience,
        ; don't show titles when it's provided.
        (unless skip-to-level
          (text-screen G.quest.title :center T))
        (start-level
          :level-n (if skip-to-level (int skip-to-level) 1)
          :show-title (not skip-to-level))))
    (main-io-loop)))


(defn take-turn [action]
  (try
    (_take-turn action)
    (except [GameOverException]
      (ecase G.player.game-over-state
        'dead (msg "You have died.")
        'won  (victory-screen))))
  (.advance-states G))

(defn _take-turn [action]
  (do-action action)

  (when G.player.just-exited
    ; If the player has changed levels, let her take another action
    ; this turn. This means she always gets the first action on a
    ; level.
    (setv G.player.just-exited F)
    (setv G.player.taking-extra-action T)
    (return))

  (when (and
      (player-status 'Fast)
      (not G.player.taking-extra-action))
    ; Let the player take a second action this turn. (Notice that
    ; this doesn't stack with the free action from using an exit.)
    (setv G.player.taking-extra-action T)
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
  (unless (player-status 'Ivln)
    (+= G.player.poison-dose G.level.poison-intensity)
    (hurt-player :animate F
      (pop-integer-part G.player.poison-dose)
      DamageType.Poison))

  ; Run each-turn hooks. Unless the object is neither on this level
  ; nor in the player's inventory, in which case, kick the object off
  ; the list.
  (for [o (list G.each-turn)]
    (unless (or (in o G.player.inventory) (and o.pos (is o.pos.map G.map)))
      (.remove G.each-turn o))
    (.each-turn o))

  ; Tick down status effects.
  (for [se (list G.player.status-effects)]
    (when (get G.player.status-effects se)
      (-= (get G.player.status-effects se) 1)))

  ; Advance the turn counter last.
  (+= G.turn-n 1)
  (setv G.player.taking-extra-action F))


(setv B None)
(setv _displaying F)

(defn [contextmanager] player-io []
  (global B _displaying)
  (setv B (hy.M.blessed.Terminal))
  (try
    (setv _displaying T)
    (with [_ (B.cbreak)  _ (B.fullscreen)  _ (B.hidden-cursor)]
      (yield))
    (finally
      (setv _displaying F))))

(defn [contextmanager] suppress-display []
  (global _displaying)
  (setv was _displaying)
  (try
    (setv _displaying F)
    (yield)
    (finally
      (setv _displaying was))))

(defn displaying []
  _displaying)

(defn io-mode [draw on-input]
  "Enter a modal interface that alternates between the callbacks
  `draw` (nullary) and `on-input` (unary, taking a key from `inkey`).
  `on-input` can return the symbol `done` to exit the mode."

  (while True
    (draw)
    (when (= (on-input (inkey)) 'done)
      (break))))

(defn inkey []
  ; Before checking for a key, flush standard input, so any keys
  ; pressed during an animation or slow processing are ignored instead
  ; of queued for input.
  (while (B.inkey :timeout 0))
  (B.inkey :esc-delay .01))


(defn main-io-loop []
  (io-mode

    :draw (fn []
      (print-main-screen
        :focus G.player.pos
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

(defn print-main-screen [focus #** kwargs]
  (print
    :flush T :sep "" :end ""
    B.home B.clear
    (.join "\n" (map (fn [x] (bless-colorstr B x)) (draw-screen
      B.width B.height focus #** kwargs)))))


(setv max-wrap-cols 75)
(setv x-margin (* 2 " "))
(setv y-margin 1)

(defn text-screen [text center]
  (unless (displaying)
    (return))
  (_scrolling-text-screen (+
    (* [""] y-margin)
    (if center
      (lfor
        line (B.wrap
          text
          (min max-wrap-cols B.width))
        (.center line B.width))
      (wrapped text)))))

(defn info-screen [t]
  "Enter an `io-mode` for showing information about the tile `t`."

  (import
    re
    simalq.display [color-tile])

  (setv lines [
    #* (* [""] y-margin)
    (+ x-margin (bless-colorstr B (color-tile t)) "  " t.full-name)
    ""
    (+ x-margin (B.bold (next (gfor
      [name superclass] (.items Tile.superclasses)
      :if (isinstance t superclass)
      name))))
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

  (_scrolling-text-screen lines))

(defn _scrolling-text-screen [lines]

  (setv top-line-ix 0)
  (setv pause-seconds 0.25)

  (io-mode

    :draw (fn []
      (print
        :flush T :sep "" :end ""
        B.home B.clear
        (.join "\n" (cut lines
          top-line-ix
          (+ top-line-ix B.height))))
      (when (= top-line-ix 0)
        ; Ignore input for a bit so the user doesn't accidentally
        ; dismiss the screen while holding a key.
        (sleep pause-seconds)))

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

(defn wrapped [text]
  (lfor
    line (B.wrap
      text
      (min max-wrap-cols (- B.width (len x-margin))))
    (+ x-margin line)))


(defn load-saved-game-screen [saves-meta]
  "Display a menu of saved games, and return the path of a saved game
  to load (or `None` for no selection)."

  (setv [saves display-lines] saves-meta)

  (setv save-ix None)
  (io-mode

    :draw (fn []
      (print
        :flush T :sep "" :end ""
        B.home B.clear
        (.join "\n" (gfor
          line (cut display-lines B.height)
          (cut line B.width)))))

    :on-input (fn [key]
      (nonlocal save-ix)
      (when (and
          (in key menu-letters)
          (< (.index menu-letters key) (len saves)))
        (setv save-ix (.index menu-letters key)))
      'done))

  (when (is-not save-ix None)
    (get saves save-ix "path")))


(defn victory-screen []
  (import zlib base64)
  (text-screen :center F (+
    (.decode :encoding "UTF-8" (zlib.decompress (base64.b64decode
      b"eNqVUkEOwCAIu+8VfSoHD7zAB/qSxaECwsxmSNRSpCUCaLWswJ8lJZfs3CqZ4E/1rES5XGNfUixnnmc/dtwU9Qjra7MdRK0vmw60YXen0kwYQpp5cTkulDzgDpqhLU6Z0LTDY7QlqKRUCbnRuJJTJszCfhQkSqF2NiijeQzR9uP0BhF5bBI=")))
        ; Modified from `toilet -f mono12`.
    "\n"
    "You've escaped the dungeon and returned home to your kingdom, safe and sound. Congratulations!\n"
    "\n"
    f"Score: {G.score :10,}\n"
    f"Turn:  {G.turn-n :10,}")))
