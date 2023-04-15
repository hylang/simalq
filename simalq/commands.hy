(require
  hyrule [ecase]
  simalq.macros [defdataclass])
(import
  copy [deepcopy]
  simalq.util [CommandError save-game-path msg]
  simalq.geometry [Direction GeometryError pos+ at]
  simalq.game-state [G save-game load-game]
  simalq.tile [mv-tile]
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

(setv dir-keys {
  "7" Direction.NW  "8" Direction.N  "9" Direction.NE
  "4" Direction.W   "5" 'center      "6" Direction.E
  "1" Direction.SW  "2" Direction.S  "3" Direction.SE})

(setv cmd-keys {
  ";" Look
  "u" [ShiftHistory -1]  ; Undo
  "r" [ShiftHistory +1]  ; Redo
  "S" SaveGame
  "L" LoadGame})


(defn do-command [cmd]
  "This function is only for commands that aren't actions; see
  `do-action` for actions."
  (import
    simalq.main [io-mode print-main-screen info-screen])

  (ecase (type cmd)

    Look (do
      (setv focus G.player.pos)
      (io-mode
        :draw (fn []
          (print-main-screen focus :status-bar F))
        :on-input (fn [key]
          (setv dir-v (.get dir-keys (str key)))
          (cond
            (and dir-v (!= dir-v 'center))
              (try
                (nonlocal focus)
                (setv focus (pos+ focus dir-v))
                (except [GeometryError]))
            (or (= dir-v 'center) (= (str key) ";"))
              (when (at focus)
                (info-screen (get (at focus) 0)))
            True
              'done))))

    ShiftHistory (do
      (setv target (+ G.state-i cmd.steps))
      (when (>= target (len G.states))
        (raise (CommandError "Nothing to redo.")))
      (when (< target 0)
        (raise (CommandError "Nothing to undo.")))
      (setv G.state-i target))

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
          (return))))))
