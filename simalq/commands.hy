(require
  hyrule [ecase do-n unless]
  simalq.macros [defdataclass])
(import
  copy [deepcopy]
  simalq.color :as color
  simalq.util [CommandError DamageType save-game-path msg player-shot-damage flash-map invlets player-status]
  simalq.geometry [Direction Pos pos+ at dist]
  simalq.game-state [G save-game load-game]
  simalq.tile [mv-tile damage-tile destroy-tile]
  simalq.tile.scenery [walkability])
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

(defdataclass GonnaShoot [Command]
  "Prepare to take a direction key for shooting.")
(defdataclass GonnaUseItem [Command]
  "Show your inventory, and prepare to take a key indicating
  the item to use.")
(defdataclass Look [Command]
  "Move the cursor around the level.")
(defdataclass ShiftHistory [Command]
  "Undo or redo."
  [steps]
  :frozen T)
(defdataclass SaveGame [Command]
  "Write the global state to a file.")
(defdataclass LoadGame [Command]
  "Read a file and replace the global state with its contents.")


(defn get-command [key]
  (when (setx v (.get dir-keys (str key)))
    (return (if (= v 'center)
      (Wait)
      (Walk v))))
  (when (setx v (.get cmd-keys (str key)))
    (return (if (isinstance v list)
      ((get v 0) #* (cut v 1 None))
      (v))))
  None)

(setv dir-keys (dfor
  [k v] (.items {
    "7y" Direction.NW  "8k" Direction.N  "9u" Direction.NE
    "4h" Direction.W   "5." 'center      "6l" Direction.E
    "1b" Direction.SW  "2j" Direction.S  "3n" Direction.SE})
  char k
  char v))

(setv cmd-keys {
  "a" GonnaUseItem
  "f" GonnaShoot
  ";" Look
  "e" [ShiftHistory  -1]  ; Undo
  "E" [ShiftHistory -10]
  "r" [ShiftHistory  +1]  ; Redo
  "R" [ShiftHistory +10]
  "S" SaveGame
  "L" LoadGame})


(defn do-command [cmd]
  "This function is only for commands that aren't actions; see
  `do-action` for actions."
  (import
    simalq.main [io-mode print-main-screen info-screen inkey take-turn])

  (defn targeting-mode [target-callback]
    "Allow the user to select a target square with the direction keys.
    `target-callback` is called on the targeted `Pos` when the user
    presses a selection key. It should return true if targeting mode
    should then end.

    Return a selected `Pos` or `None`."
    (setv focus G.player.pos)
    (io-mode
      :draw (fn []
        (print-main-screen focus :status-bar F))
      :on-input (fn [key]
        (nonlocal focus)
        (setv dir-v (.get dir-keys (str key)))
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

  (ecase (type cmd)

    GonnaShoot (do
      ; A direction key causes Tris to shoot in that direction.
      ; Any other key just cancels out of shooting mode.
      (setv v (.get dir-keys (str (inkey))))
      (when (isinstance v Direction)
        (take-turn (Shoot v))))

    GonnaUseItem (do
      (setv item-ix None)
      (io-mode
        :draw (fn []
          (print-main-screen G.player.pos :inventory T))
        :on-input (fn [key]
          (nonlocal item-ix)
          (when (and
              (in key invlets)
              (< (.index invlets key) G.rules.max-usables))
            (setv item-ix (.index invlets key)))
          'done))
      (when (is-not item-ix None)
        (if (and
            (setx item (get G.player.inventory item-ix))
            item.targeted)
          (do
            (setv target (targeting-mode (fn [target] T)))
            (when target
              (take-turn (UseItem item-ix target.x target.y))))
          (take-turn (UseItem item-ix None None)))))

    Look
      (targeting-mode (fn [target]
        (when (at target)
          (info-screen (get (at target) 0)))
        F))

    ShiftHistory (do
      (when (and (< cmd.steps 0) (= G.state-i 0))
        (raise (CommandError "Nothing to undo.")))
      (when (and (> cmd.steps 0) (= G.state-i (- (len G.states) 1)))
        (raise (CommandError "Nothing to redo.")))
      (setv G.state-i (max 0 (min (- (len G.states) 1)
        (+ G.state-i cmd.steps)))))

    SaveGame
      (try
        (.parent.mkdir save-game-path :exist-ok T)
        (save-game save-game-path)
        (msg "Game saved.")
        (except [e IOError]
          (raise (CommandError (+ "Save failed: " (str e))))))

    LoadGame
      (try
        (load-game save-game-path)
        (msg "Game loaded.")
        (except [e IOError]
          (raise (CommandError (+ "Load failed: " (str e))))))))


(defn do-action [action]
  ; Set up a new game state.
  (setv new-state (deepcopy (get G.states G.state-i)))
  (+= G.state-i 1)
  (cond
    (= G.state-i (len G.states))
      ; We're at the end of the undo history, so append the new state.
      (.append G.states new-state)
    (= (. G.states [G.state-i] action) action)
      ; We should effectively be redoing this state, since it's the
      ; same action as last time and the game is deterministic.
      ; Keep the remaining history for further redoing.
      (setv (get G.states G.state-i) new-state)
    True
      ; We're branching off in a new direction. Discard the now-
      ; obsolete redo history. (We don't support a full-blown tree
      ; of states, just a line.)
      (setv (cut G.states G.state-i None) [new-state]))
  (setv G.action action)
  (try
    (_execute-action action)
    (except [CommandError]
      ; No action occurred. Abort the new game state before bubbling
      ; the exception up.
      (del (get G.states G.state-i))
      (-= G.state-i 1)
      (raise))))

(defn _execute-action [action]
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
