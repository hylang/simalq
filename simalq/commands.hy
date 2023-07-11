(require
  hyrule [case ecase do-n unless]
  simalq.macros [defdataclass])
(import
  sys
  re
  copy [deepcopy]
  toolz [partition]
  simalq.color :as color
  simalq.util [CommandError DamageType msg player-shot-damage flash-map menu-letters player-status]
  simalq.geometry [Direction Pos pos+ at dist]
  simalq.game-state [G]
  simalq.tile [mv-tile damage-tile destroy-tile]
  simalq.tile.scenery [walkability]
  simalq.save-load [save-game-to-slot get-saves-list load-game])
(setv  T True  F False)


(defdataclass Command []
  "Something the player wants to do, translated from user input.")

(defdataclass Action [Command]
  "A discrete effect that the player can have on the gamestate.")
(defdataclass Wait [Action]
  "Do nothing, passing your turn (or, more precisely, passing
  one opportunity for an action).")
(defdataclass Walk [Action]
  "Try to walk one step in the given direction, or attack something
  that's in the way with your sword."
  [direction]
  :frozen T)
(defdataclass Shoot [Action]
  "Fire an arrow in the given direction."
  [direction]
  :frozen T)
(defdataclass UseItem [Action]
  "Apply an item from your inventory."
  [item-ix target-x target-y]
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
  [steps]
  :frozen T)
(defdataclass SaveGame [Command]
  "Save the game, {details}."
  [kind]
  :frozen T)
(defdataclass LoadGame [Command]
  "Load a saved game.")
(defdataclass Quit [Command]
  "Quit the game. (You won't be prompted to confirm or save first.)")


(defn get-command [key]
  (when (setx v (read-dir-key key))
    (return (if (= v 'center)
      (Wait)
      (Walk v))))
  (when (setx v (.get cmd-keys (str key)))
    (return (if (isinstance v list)
      ((get v 0) #* (cut v 1 None))
      (v))))
  None)

(defn read-dir-key [key]
  (.get _dir-keys (str key) (.get _dir-keys key.name)))
(setv _dir-keys (dfor
  :setv D Direction
  [k v] (partition 2 [
    ["7" "y" "HOME"] D.NW  ["8" "k" "UP"]     D.N      ["9" "u" "PGUP"]   D.NE
    ["4" "h" "LEFT"] D.W   ["5" "." "ESCAPE"] 'center  ["6" "l" "RIGHT"]  D.E
    ["1" "b" "END"]  D.SW  ["2" "j" "DOWN"]   D.S      ["3" "n" "PGDOWN"] D.SE])
  s k
  (if (> (len s) 1) (+ "KEY_" s) s) v))

(setv cmd-keys {
  "?" Help
  "!" Quit
  "S" [SaveGame 'main]
  "C" [SaveGame 'checkpoint]
  "L" LoadGame
  "e" [ShiftHistory  -1]  ; Undo
  "E" [ShiftHistory -10]
  "r" [ShiftHistory  +1]  ; Redo
  "R" [ShiftHistory +10]
  ";" Look
  "f" GonnaShoot
  "i" Inventory
  "a" GonnaUseItem})


(defn do-command [cmd]
  "This function is only for commands that aren't actions; see
  `do-action` for actions."
  (import
    simalq.main [io-mode text-screen print-main-screen info-screen inkey take-turn load-saved-game-screen])

  (defn targeting-mode [target-callback]
    "Allow the user to select a target square with the direction keys.
    `target-callback` is called on the targeted `Pos` when the user
    presses a selection key. It should return true if targeting mode
    should then end.

    Return a selected `Pos` or `None`."
    (setv focus G.player.pos)
    (io-mode
      :draw (fn []
        (print-main-screen focus :status-bar F :tile-list 'nonpickable))
      :on-input (fn [key]
        (nonlocal focus)
        (setv dir-v (read-dir-key key))
        (cond
          (and dir-v (!= dir-v 'center))
            (setv focus (or (pos+ focus dir-v) focus))
          (or (= dir-v 'center) (= (str key) ";"))
            (when (target-callback focus)
              'done)
          True (do
            (setv focus None)
            'done))))
    focus)

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
      (print-main-screen G.player.pos :inventory T))))

  (ecase (type cmd)

    Help
      (text-screen help-text :center F)

    GonnaShoot (do
      ; A direction key causes Tris to shoot in that direction.
      ; Any other key just cancels out of shooting mode.
      (setv v (read-dir-key (inkey)))
      (when (isinstance v Direction)
        (take-turn (Shoot v))))

    Inventory
      (when (is-not None (setx item-ix (get-inventory-ix)))
        (if (setx item (get G.player.inventory item-ix))
          (info-screen item)
          (raise (CommandError "That inventory slot is empty."))))

    GonnaUseItem
      (when (is-not None (setx item-ix (get-inventory-ix)))
        (if (and
            (setx item (get G.player.inventory item-ix))
            item.targeted)
          (do
            (setv target (targeting-mode (fn [target] T)))
            (when target
              (take-turn (UseItem item-ix target.x target.y))))
          (take-turn (UseItem item-ix None None))))

    Look
      (targeting-mode (fn [target]
        (when (and
            (setx stack (at target))
            (is-not None (setx tile-ix (if (= (len stack) 1)
              0
              (menu (len stack) :draw (fn []
                (print-main-screen target :status-bar F :tile-list 'pickable)))))))
          (info-screen (get stack tile-ix)))
        F))

    ShiftHistory (do
      (when (and (< cmd.steps 0) (= G.state-i 0))
        (raise (CommandError "Nothing to undo.")))
      (when (and (> cmd.steps 0) (= G.state-i (- (len G.states) 1)))
        (raise (CommandError "Nothing to redo.")))
      (.set-state-i G (max 0 (min (- (len G.states) 1)
        (+ G.state-i cmd.steps)))))

    SaveGame
      (try
        (save-game-to-slot (= cmd.kind 'checkpoint))
        (msg f"Game saved ({cmd.kind !s}).")
        (except [e IOError]
          (raise (CommandError (+ "Save failed: " (str e))))))

    LoadGame
      (try
        (when (setx path (load-saved-game-screen (get-saves-list)))
          (load-game path)
          (msg "Game loaded."))
        (except [e IOError]
          (raise (CommandError (+ "Load failed: " (str e))))))

    Quit
      (sys.exit)))


(defn do-action [action]
  (setv G.action action)

  (when G.player.game-over-state
    (raise (CommandError (.format "{}. You can undo or load a saved game."
      (ecase G.player.game-over-state
        'dead "You're dead"
        'won "You won the game")))))
  (when (and (player-status 'Para) (is-not (type action) Wait))
    (raise (CommandError "You're paralyzed. You can only wait for it to pass.")))

  (ecase (type action)

    Wait
      ; Nothing to do. This action should always succeed.
      None

    Walk (do
      (setv d action.direction)
      (setv [target wly] (walkability G.player.pos d :monster? F))
      (when (= wly 'out-of-bounds)
        (raise (CommandError "The border of the dungeon blocks your movement.")))
      (when (= wly 'blocked-diag)
        (raise (CommandError "That diagonal is blocked by a neighbor.")))
      (for [tile (at target)]
        (when (.hook-player-bump tile G.player.pos)
          (return)))
      (when (= wly 'bump)
        (raise (CommandError "Your way is blocked.")))
      (for [tile (at G.player.pos)]
        (.hook-player-walk-from tile target))
      (for [tile (at target)]
        (.hook-player-walk-to tile G.player.pos))
      ; No exceptions have stopped us, so go.
      (mv-tile G.player target)
      (for [tile (at target)]
        (when (.hook-player-walked-into tile)
          (return))))

    Shoot (do
      (setv
        d action.direction
        target G.player.pos
        targets []
        magic F)
      (when G.player.magic-arrows
       (setv magic T)
       (-= G.player.magic-arrows 1))
      (defn animate []
        (flash-map :flash-time-s .1
          G.player.pos
          (if magic
            color.flash-player-shot-magic
            color.flash-player-shot-mundane)
          targets
          {}))
      (do-n G.rules.reality-bubble-size
        (setv target (pos+ target d))
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
              (unless (and magic (!= tile.pos target))
                (return)))
            tile.damageable (do
              ; The arrow damages the tile and may stop.
              (animate)
              (damage-tile tile (player-shot-damage magic) (if magic
                DamageType.MagicArrow
                DamageType.MundaneArrow))
              (unless (and magic (!= tile.pos target))
                (return)))
            tile.blocks-player-shots
              ; The arrow won't be able to leave this square, although
              ; it can affect other tiles in the square.
              (setv blocked T)))
        (when blocked
          (animate)
          (return)))
      (animate))

    UseItem (do
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
      (setv (get G.player.inventory action.item-ix) None)
      (destroy-tile item))))


(setv controls (.join "\n" (lfor
  [k cmd] (.items cmd-keys)
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

(setv help-text (.strip #[f[

All controls are case-sensitive. Use the numeric keypad or the vi-keys to move. Use `5` or `.` to wait, skipping a turn.

{controls}

Paths:
- Saved games: {hy.M.simalq/util.saved-games-dir}
- IQ quest cache: {hy.M.simalq/util.cache-dir}

To delete a saved game, delete the corresponding file from the saved-game directory.]f]))
