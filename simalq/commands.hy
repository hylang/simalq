"Interface and logic for player commands and actions."


(require
  hyrule [case ecase ebranch do-n unless]
  simalq.macros [defdataclass])
(import
  sys
  re
  copy [deepcopy]
  toolz [partition]
  metadict [MetaDict]
  simalq.color :as color
  simalq.util [CommandError DamageType StatusEffect msg player-shot-damage flash-map menu-letters]
  simalq.geometry [Direction Pos at dist]
  simalq.game-state [G]
  simalq.tile [Damageable]
  simalq.tile.scenery [Scenery walkability]
  simalq.save-load [save-game-to-slot get-saves-list load-game])
(setv  T True  F False)


(setv move-blocked-msgs (MetaDict
  :simple "Your way is blocked."
  :diag "That diagonal is blocked by a neighbor."
  :map-border "The border of the dungeon blocks your movement."))


(defdataclass Command []
  "Something the player wants to do, translated from user input.")

(defdataclass Action [Command]
  "A discrete effect that the player can have on the game state.")
(defdataclass Wait [Action]
  "Do nothing, passing your turn (or, more precisely, passing
  one opportunity for an action).")
(defdataclass Walk [Action]
  "Try to walk one step in the given direction, or attack something
  that's in the way with your sword."
  :fields [direction]
  :frozen T)
(defdataclass Shoot [Action]
  "Fire an arrow in the given direction."
  :fields [direction]
  :frozen T)
(defdataclass UseItem [Action]
  "Apply an item from your inventory."
  :fields [item-ix target-x target-y]
  :frozen T)
(defdataclass UseControllableTeleporter [Action]
  "Walk towards a controllable teleporter and use it."
  :fields [direction target-x target-y]
  :frozen T)

(defdataclass Help [Command]
  "Show this help screen.")
(defdataclass GonnaShoot [Command]
  "Fire an arrow. This key enters shooting mode; press a direction key
  to shoot or any other key to cancel.")
(defdataclass Inventory [Command]
  "Inventory display. Press the key corresponding to an item to get
  info on it.")
(defdataclass GonnaUseItem [Command]
  "Apply an item from your inventory. Some items need to be targeted
  at a square, in the same way as look mode.")
  ; Actually: show your inventory, like `Inventory`, but use the
  ; selected item instead of showing info.
(defdataclass Look [Command]
  "Look mode. Use a direction key to move the cursor, this key (or the
  wait key) to get info on a tile under the cursor, and any other key
  to exit look mode. If there's more than one tile on the selected
  square, you'll be prompted to choose one when you try to get info.")
(defdataclass ShiftHistory [Command]
  "{details}"
  ; Undo or redo.
  :fields [steps]
  :frozen T)
(defdataclass SaveGame [Command]
  "Save the game, {details}."
  :fields [kind]
  :frozen T)
(defdataclass LoadGame [Command]
  "Load a saved game.")
(defdataclass Quit [Command]
  "Quit the game. (You won't be prompted to confirm or save first.)")


(defn get-command [key]
  (import simalq.keyboard [read-dir-key command-keys])
  (when (setx v (read-dir-key key))
    (return (if (= v 'center)
      (Wait)
      (Walk v))))
  (when (setx v (.get command-keys (str key)))
    (return (if (isinstance v list)
      ((get v 0) #* (cut v 1 None))
      (v))))
  None)


(defn do-command [cmd]
  "This function is only for commands that aren't actions; see
  `do-action` for actions."
  (import
    simalq.main [io-mode text-screen print-main-screen info-screen inkey take-turn load-saved-game-screen])

  (defn menu [n-items draw]
    "Allow the user to select an item from a list. Return an index of
    the list or `None`."
    (setv ix None)
    (io-mode
      :draw draw
      :on-input (fn [key]
        (nonlocal ix)
        (when (and
            (in key menu-letters)
            (< (.index menu-letters key) n-items))
          (setv ix (.index menu-letters key)))
        'done))
    ix)

  (defn get-inventory-ix []
    (menu G.rules.max-usables :draw (fn []
      (print-main-screen :inventory T))))

  (ecase (type cmd)

    Help
      (text-screen (help-text) :center F)

    GonnaShoot (do
      ; A direction key causes Tris to shoot in that direction.
      ; Any other key just cancels out of shooting mode.
      (setv v (hy.I.simalq/keyboard.read-dir-key (inkey)))
      (when (isinstance v Direction)
        (take-turn (Shoot v))))

    Inventory
      (when (is-not (setx item-ix (get-inventory-ix)) None)
        (if (setx item (get G.player.inventory item-ix))
          (info-screen item)
          (raise (CommandError "That inventory slot is empty."))))

    GonnaUseItem
      (when (is-not (setx item-ix (get-inventory-ix)) None)
        (if (and
            (setx item (get G.player.inventory item-ix))
            item.targeted)
          (when (setx target (targeting-mode (fn [target] T)))
            (take-turn (UseItem item-ix target.x target.y)))
          (take-turn (UseItem item-ix None None))))

    Look
      (targeting-mode (fn [target]
        (when (and
            (setx stack (at target))
            (is-not None (setx tile-ix (if (= (len stack) 1)
              0
              (menu (len stack) :draw (fn []
                (print-main-screen :target target
                  :tile-list 'pickable)))))))
          (info-screen (get stack tile-ix)))))

    ShiftHistory (do
      (when (and (< cmd.steps 0) (= G.state-i 0))
        (raise (CommandError "Nothing to undo.")))
      (when (and (> cmd.steps 0) (= G.state-i (- (len G.states) 1)))
        (raise (CommandError "Nothing to redo.")))
      (.set-state-i G (max 0 (min (- (len G.states) 1)
        (+ G.state-i cmd.steps)))))

    SaveGame (do
      (try
        (save-game-to-slot (= cmd.kind 'checkpoint))
        (except [e IOError]
          (raise (CommandError f"Save failed: {e}"))))
      (msg f"Game saved ({cmd.kind !s})."))

    LoadGame (do
      (try
        (when (setx path (load-saved-game-screen #* (get-saves-list)))
          (load-game path)
          (msg "Game loaded."))
        (except [e IOError]
          (raise (CommandError f"Load failed: {e}")))))

    Quit
      (sys.exit)))


(defn do-action [action]
  (setv G.action action)

  (when G.player.game-over-state
    (raise (CommandError (.format "{}. You can undo or load a saved game."
      (ecase G.player.game-over-state
        'dead "You're dead"
        'won "You won the game")))))
  (when (and
      (.player-has? StatusEffect.Para)
      (is-not (type action) Wait))
    (raise (CommandError "You're paralyzed. You can only wait for it to pass.")))

  (ebranch (in (type action) it)

    [Wait]
      ; Nothing to do. This action should always be allowed.
      None

    [Walk UseControllableTeleporter] (do

      (defn pat [pos]
        "`at`, but excluding tiles we should ignore due to passwall."
        (gfor
          tile (at pos)
          :if (not (and
            (isinstance tile Scenery)
            tile.passwallable
            (.player-has? StatusEffect.Pass)))
          tile))

      (setv d action.direction)
      (setv [target wly] (walkability G.player.pos d :monster? F))
      (when (= wly 'out-of-bounds)
        (raise (CommandError move-blocked-msgs.map-border)))
      (when (= wly 'blocked-diag)
        (raise (CommandError move-blocked-msgs.diag)))

      (for [tile (pat target)]
        (when (.hook-player-bump tile G.player.pos)
          (return)))
      (when (= wly 'bump)
        (raise (CommandError move-blocked-msgs.simple)))

      (for [tile (pat G.player.pos)]
        (.hook-player-walk-from tile target))
      (for [tile (pat target)]
        (.hook-player-walk-to tile G.player.pos))

      ; No exceptions have stopped us, so go.
      (setv pos-was G.player.pos)
      (.move G.player target)
      (for [tile (pat pos-was)]
        (.hook-player-walked-from tile))
      (for [tile (pat target)]
        (when (.hook-player-walked-into tile)
          (return))))

    [Shoot] (do

      (setv
        d action.direction
        target G.player.pos
        targets []
        magic? F)
      (when G.player.magic-arrows
       (setv magic? T)
       (-= G.player.magic-arrows 1))

      (defn animate []
        (flash-map :flash-time-s .1
          (if magic?
            color.flash-player-shot-magic
            color.flash-player-shot-mundane)
          targets
          {}))

      (do-n G.rules.reality-bubble-size
        (setv target (+ target d))
        (unless target
          ; An arrow that hits the level border stops.
          (animate)
          (return))
        (.append targets target)
        (when (= target G.player.pos)
          ; An arrow that wraps around the map all the way back to the
          ; player stops there, rather than looping.
          (animate)
          (return))
        (setv blocked F)
        (for [tile (at target)]
          (cond
            tile.hook-player-shot (do
              ; The arrow may stop after the hook is called.
              (animate)
              (.hook-player-shot tile)
              (unless (and magic? (!= tile.pos target))
                (return)))
            (isinstance tile Damageable) (do
              ; The arrow damages the tile and may stop.
              (animate)
              (.damage tile (player-shot-damage magic?) (if magic?
                DamageType.MagicArrow
                DamageType.MundaneArrow))
              (unless (and magic? (!= tile.pos target))
                (return)))
            tile.blocks-player-shots
              ; The arrow can't leave this square, although it can
              ; affect other tiles in the square.
              (setv blocked T)))
        (when blocked
          (animate)
          (return)))
      (animate))

    [UseItem] (do
      (setv item (get G.player.inventory action.item-ix))
      (unless item
        (raise (CommandError "That inventory slot is empty.")))
      (setv target (when (is-not action.target-x None)
        (Pos G.map action.target-x action.target-y)))
      (if item.targeted
        (do
          (unless target
            (raise (CommandError "That item requires a target.")))
          (unless (<= (dist G.player.pos target) G.rules.reality-bubble-size)
            (raise (CommandError "Item targets must lie inside the reality bubble.")))
          (.use item target))
        (do
          (when target
            (raise (CommandError "That item can't use a target.")))
          (.use item)))
      ; All usable items are destroyed after one use.
      (setv (get G.player.inventory action.item-ix) None))))


(defn targeting-mode [target-callback]
  "Allow the user to select a target square with the direction keys.
  `target-callback` is called on the targeted `Pos` when the user
  presses a selection key. It should return true if targeting mode
  should then end.

  Return a selected `Pos` or `None`."

  (setv focus G.player.pos)
  (hy.I.simalq/main.io-mode
    :draw (fn []
      (hy.I.simalq/main.print-main-screen
        :target focus :tile-list 'nonpickable))
    :on-input (fn [key]
      (nonlocal focus)
      (setv dir-v (hy.I.simalq/keyboard.read-dir-key key))
      (cond
        (and dir-v (!= dir-v 'center))
          (setv focus (or (+ focus dir-v) focus))
        (or (= dir-v 'center) (= (str key) ";"))
          (when (target-callback focus)
            'done)
        True (do
          (setv focus None)
          'done))))
  focus)


(defn help-text []
  (setv controls (.join "\n" (lfor
    [k cmd] (.items hy.I.simalq/keyboard.command-keys)
    :setv [cmd arg] (if (isinstance cmd list) cmd [cmd None])
    (+ k " - " (.format
      (re.sub r"\s+" " " cmd.__doc__)
      :details (case cmd
        ShiftHistory
          (.format "{} {}."
            (if (< arg 0) "Undo the previous" "Redo the next")
            (if (> (abs arg) 1) f"{(abs arg)} actions" "action"))
        SaveGame
          (ecase arg
            'main "overwriting your main save file for this quest"
            'checkpoint "creating a new checkpoint save")))))))

  (.strip #[f[

All controls are case-sensitive. Use the numeric keypad or the vi-keys to move. Use `5` or `.` to wait, skipping a turn.

{controls}

You can rebind keys by editing the Hy source code at
  {hy.I.simalq/keyboard.__file__}

Other filepaths of interest:
- Saved games: {hy.I.simalq/util.saved-games-dir}
- IQ quest cache: {hy.I.simalq/util.cache-dir}

To delete a saved game, delete the corresponding file from the saved-game directory.]f]))
