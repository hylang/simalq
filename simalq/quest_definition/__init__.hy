"This file provides functions for concisely defining quests and
levels. It also provides a dictionary `builtin-quests`, which gathers
quest definitions from other files in this directory."

;; --------------------------------------------------
;; * Imports
;; --------------------------------------------------

(require
  hyrule [unless])
(import
  math [ceil]
  re
  textwrap [dedent]
  fractions [Fraction]
  toolz [partition]
  simalq.game-state [G]
  simalq.geometry [Map Pos pos+ Direction]
  simalq.quest [Quest Level]
  simalq.tile [Tile add-tile])

;; --------------------------------------------------
;; * Helpers
;; --------------------------------------------------

(defn kwdict [iterable]
  (dfor
    [k v] (partition 2 iterable)
    (hy.mangle k.name) v))

(defn locate [locator]
  (cond
    (= locator 'here)
      G.player.pos
    (isinstance locator hy.models.Symbol)
      (pos+ G.player.pos (getattr Direction (str locator)))
    (isinstance locator list)
      (Pos G.map #* locator)
    (isinstance locator Pos)
      locator))

;; --------------------------------------------------
;; * `mk-quest`, `mk-level`, `mk-tile`
;; --------------------------------------------------

(defn mk-quest [
    #* levels
    [starting-hp 100]
    [name "Test Quest"]
    [title "Test Quest title"]
    [authors "Mitt Lowporch and Cire Whyhall"]]
  (Quest
    :name name
    :title title
    :authors authors
    :starting-hp starting-hp
    :levels (tuple (gfor
      [i level-args] (enumerate levels)
      (mk-level :n (+ i 1) #** (kwdict level-args))))))

(defn mk-level [
    n
    [player-start #(0 0)]
    [width 16] [height 16] [wrap-x False] [wrap-y False]
    [tiles #()]
      ; The requested tiles are placed in a line east of the player
      ; start.
    [map None]
      ; Overrides `width`, `height`, `tiles`, and possibly
      ; `player-start` if provided.
    [map-marks #()]
    [title None]
    [next-level None]
    [poison-intensity (Fraction 0)]
    [time-limit None] [exit-speed None] [moving-exit-start None]]
  (if map
    (do
      (setv map (dedent
        (re.sub r"\A( *\n)*" "" (re.sub r"( *\n)*\Z" "" map))))
      (setv height (+ 1 (.count map "\n")))
      (setv width (ceil (/
        (try (.index map "\n") (except [ValueError] (len map)))
        2)))
      (setv m (Map.make :wrap-x wrap-x :wrap-y wrap-y :width width :height height))
      (for [
          [y row] (enumerate (reversed (.split map "\n")))
          :do (when (% (len row) 2) (setv row (+ row " ")))
          [x mapsym] (enumerate (partition 2 row))]
        (setv p (Pos m x y))
        (setv mapsym (.join "" mapsym))
        (cond
          (in mapsym map-marks)
            (mk-tile p (get map-marks mapsym))
          (= mapsym "@ ")
            (setv player-start #(x y))
          (= mapsym ". ")
            None
          (= mapsym "██")
            (mk-tile p "wall")
          True (do
            (setv types (lfor
              t (.values Tile.types)
              :if (= t.mapsym mapsym)
              t))
            (unless types
              (raise (ValueError f"No match for {mapsym !r}")))
            (when (> (len types) 1)
              (raise (ValueError f"Ambiguous: {mapsym !r} - {types !r}")))
            (mk-tile p (. types [0] stem))))))
    (do
      (setv m (Map.make :wrap-x wrap-x :wrap-y wrap-y :width width :height height))
      (for [[i tile-spec] (enumerate tiles)]
        (mk-tile
          (Pos m (+ (get player-start 0) i 1) (get player-start 1))
          tile-spec))))
  (Level
    :title title
    :n n
    :next-level (or next-level (+ n 1))
    :map m
    :player-start (Pos m #* player-start)
    :poison-intensity poison-intensity
    :time-limit time-limit
    :exit-speed exit-speed
    :moving-exit-start moving-exit-start))

(defn mk-tile [locator tile-spec]
  (when (= tile-spec 'floor)
    (return))
  (if (isinstance tile-spec str)
    (setv  d {}  stem tile-spec)
    (setv  [stem #* d] tile-spec  d (kwdict d)))
  (add-tile (locate locator) stem #** d))

;; --------------------------------------------------
;; * Load built-in quests
;; --------------------------------------------------

(setv builtin-quests {})
(import simalq.quest_definition.tutorial)
