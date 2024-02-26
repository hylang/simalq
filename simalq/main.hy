"The most central functions for the game logic and interface."

;; --------------------------------------------------------------
;; * Imports
;; --------------------------------------------------------------

(require
  hyrule [unless ecase]
  simalq.macros [pop-integer-part])
(import
  re
  time [sleep]
  contextlib [contextmanager]
  simalq [version-string]
  simalq.strings
  simalq.util [CommandError StatusEffect message-queue msg DamageType GameOverException menu-letters burst-damage]
  simalq.color :as color
  simalq.geometry [burst at turn-and-pos-seed]
  simalq.game-state [G]
  simalq.tile [Tile Scenery]
  simalq.quest [start-quest start-level]
  simalq.commands [Action get-command do-command do-action]
  simalq.display [draw-screen bless-colorstr color-tile]
  simalq.save-load [get-saves-list load-game])
(setv  T True  F False)

;; --------------------------------------------------------------
;; * `main`
;; --------------------------------------------------------------

(defn main [quest [rules None] [skip-to-level None] [load-main-save F]]
  "Begin interactive play with the named quest. `load-main-save` is ignored if
  `skip-to-level` is true."
  (setv save (when (and load-main-save (not skip-to-level))
    (next
      (gfor
        item (get (get-saves-list quest.name) 0)
        :if (get item "main")
        item)
      None)))
  (with [(player-io)]
    ; `skip-to-level` is used for debugging, so for convenience,
    ; don't show titles when it's provided.
    (start-quest quest
      :rules (when (not save) rules)
      :show-title (not (or save skip-to-level)))
    (if save
      (load-game (get save "path"))
      (start-level
        :level-n (or skip-to-level 1)
        :show-title (not skip-to-level)))
    (main-io-loop)))

;; --------------------------------------------------------------
;; * Running turns
;; --------------------------------------------------------------

(defn take-turn [action]
  (try
    (_take-turn action)
    (except [e GameOverException]
      (setv [G.player.game-over-state] e.args)
      (ecase G.player.game-over-state
        'dead (msg
          "You have died."
          (get simalq.strings.death-messages (%
             (turn-and-pos-seed G.player.pos)
             (len simalq.strings.death-messages))))
        'won (victory-screen))))
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
      (.player-has? StatusEffect.Fast)
      (not G.player.taking-extra-action))
    ; Let the player take a second action this turn. (Notice that
    ; this doesn't stack with the free action from using an exit.)
    (setv G.player.taking-extra-action T)
    (return))

  ; Allow actors in the reality bubble to act, in `burst`'s spiral
  ; order.
  (for [
      pos (burst G.player.pos G.rules.reality-bubble-size)
      tile (list (at pos))
      :if (isinstance tile hy.I.simalq/tile.Actor)]
    (.maybe-act tile))

  ; Now do end-of-turn processing.

  ; Run `each-turn` hooks.
  (hy.I.simalq/tile.EachTurner.run-all)

  ; Dose the player with ambient poison, and convert an accumulated
  ; dose ≥1 into damage. Also, deal damage from poison emitters.
  (setv [protected extra-poison] (map any (zip #* (gfor
    pos (burst G.player.pos 1)
    tile (at pos)
    (if (isinstance tile Scenery)
      #(tile.protects-vs-poison-air tile.emits-poison-air)
      #(F F))))))
  (unless (or protected (.player-has? StatusEffect.Ivln))
    (+= G.player.poison-dose
      (* G.rules.poison-factor G.level.poison-intensity))
    (.damage G.player :animate F
      (pop-integer-part G.player.poison-dose)
      DamageType.Poison)
    (when extra-poison
      (.damage G.player G.rules.poison-emitter-damage DamageType.Poison)))

  ; The player herself deals out passive poison damage, if she's
  ; poisonous.
  (when (.player-has? StatusEffect.Pois)
    (burst-damage G.player.pos
      :damage-type DamageType.Poison
      :amount (* [G.rules.player-poison-damage] 2)
      :color 'moss-green
      :quick-flash T))
        ; This animation happens every turn the player has a poisonous
        ; aura, so make it especially fast.

  ; Tick down status effects.
  (for [se StatusEffect]
    (when (.player-has? se)
      (.add se -1)))

  ; Tick down the time limit.
  (when G.time-left
    (if (= G.time-left 1)
      ; Time's up. (We check on 1 rather than 0 so that a time limit of 3
      ; allows 3 turns.)
      (start-level G.level.next-level)
      (-= G.time-left 1)))

  ; Advance the turn counter last.
  (+= G.turn-n 1)
  (setv G.player.taking-extra-action F))

;; --------------------------------------------------------------
;; * IO
;; --------------------------------------------------------------

;; --------------------------------------------------------------
;; ** Basics
;; --------------------------------------------------------------

(setv B None)
(setv _displaying F)

(defn [contextmanager] player-io []
  (global B _displaying)
  (setv B (hy.I.blessed.Terminal))
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

;; --------------------------------------------------------------
;; ** The main loop for IO
;; --------------------------------------------------------------

(defn main-io-loop []
  (io-mode

    :draw (fn []
      (print-main-screen
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

(defn print-main-screen [#** kwargs]
  (print
    :flush T :sep "" :end ""
    B.home
    (.join "\n" (map (fn [x] (bless-colorstr B x)) (draw-screen
      B.width B.height #** kwargs)))))

;; --------------------------------------------------------------
;; ** Text screens
;; --------------------------------------------------------------

(setv max-wrap-cols 75)
(setv x-margin (* 2 " "))
(setv y-margin 1)

(defn text-screen [text center]
  "Wrap text into lines and display it interactively."
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

  (_scrolling-text-screen [
    #* (* [""] y-margin)
    ; The mapsym and name of the tile
    (+ x-margin (bless-colorstr B (color-tile t)) "  " t.full-name)
    ""
    ; The name of the tile's superclass
    (+ x-margin (B.bold (next (gfor
      [name superclass] (.items Tile.superclasses)
      :if (isinstance t superclass)
      name))))
    ""
    ; Info bullets
    #* (gfor
      bullet (.info-bullets t)
      :if bullet
      line (wrapped (re.sub r"\s+" " " (if (isinstance bullet tuple)
        (.format "{} {}"
          (B.bold f"• {(get bullet 0)}:")
          (get bullet 1))
        (B.bold f"• {bullet}"))))
      line)
    ""
    ; The flavor text
    #* (wrapped t.flavor)]))

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
    f"Turn:  {G.turn-n :10,}\n"
    "\n"
    version-string)))

(defn _scrolling-text-screen [lines]
  "A simplistic interactive mode for reading text that may be taller
  than the screen. Any key will advance the text, or end the mode once
  you've reached the end of the text."

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
      ; Advance `top-line-ix` (and hence scroll the screen), or exit
      ; if we've shown it all.
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

;; --------------------------------------------------------------
;; ** The menu for loading a saved game
;; --------------------------------------------------------------

(defn load-saved-game-screen [saves display-lines]
  "Display a menu of saved games, and return the path of a saved game
  to load (or `None` for no selection)."

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
