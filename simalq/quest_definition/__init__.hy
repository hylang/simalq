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
  simalq.geometry [Map Pos Direction]
  simalq.quest [Quest Level]
  simalq.tile [Tile])

;; --------------------------------------------------
;; * Helpers
;; --------------------------------------------------

(defn kwdict [iterable]
  (dfor
    [k v] (partition 2 iterable)
    (hy.mangle k.name) v))

(defn locate [locator]
  (cond
    (is locator None)
      None
    (= locator 'here)
      G.player.pos
    (isinstance locator hy.models.Symbol)
      (+ G.player.pos (getattr Direction (str locator)))
    (isinstance locator list)
      (Pos G.map #* locator)
    (isinstance locator Pos)
      locator
    True
      (raise (TypeError locator))))

;; --------------------------------------------------
;; * `mk-quest`, `mk-level`, `mk-tile`
;; --------------------------------------------------

(defn mk-quest [
  ; Make a quest.
    #* levels
    [starting-hp 100]
    [name "Test Quest"]
    [authors "Mitt Lowporch and Cire Whyhall"]
    [title "Test Quest title"]]
  (Quest
    :name name
    :authors authors
    :title title
    :starting-hp starting-hp
    :levels (tuple (gfor
      [i level-args] (enumerate levels)
      (mk-level :n (+ i 1) #** (kwdict level-args))))))

(defn mk-level [
  ; Make a level.
    [n 1]
    [player-start #(0 0)]
    [width 16] [height 16] [wrap-x False] [wrap-y False]
    [tiles #()]
      ; An iterable of stems. The requested tiles are placed in a line
      ; east of the player start.
    [map None]
      ; A string passed to `parse-text-map`. It overrides `width`,
      ; `height`, `tiles`, and `player-start` if provided.
    [map-marks #()]
    [title None]
    [next-level None]
    [poison-intensity (Fraction 0)]
    [time-limit None] [exit-speed None] [moving-exit-start None]]
  (if map
    (setv [m player-start] (parse-text-map map map-marks wrap-x wrap-y))
    (do
      (setv m (Map.make wrap-x wrap-y width height))
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

(defn parse-text-map [text [map-marks #()] [wrap-x False] [wrap-y False]]
  (setv text (dedent
    (re.sub r"\A( *\n)*" "" (re.sub r"( *\n)*\Z" "" text))))
  (setv height (+ 1 (.count text "\n")))
  (setv width (ceil (/
    (try (.index text "\n") (except [ValueError] (len text)))
    2)))
  (setv m (Map.make wrap-x wrap-y width height))
  (setv mapsyms (dfor
    [y row] (enumerate (reversed (.split text "\n")))
    :do (when (% (len row) 2) (setv row (+ row " ")))
      ; Odd-length rows are treated as ending with a space. Thus, you
      ; don't need trailing spaces in the text.
    [x mapsym] (enumerate (partition 2 row))
    #(x y) (.join "" mapsym)))
  (setv player-start #(0 0))
  (for [[[x y] mapsym] (.items mapsyms)]
    (setv p (Pos m x y))
    (cond
      (in mapsym map-marks)
        (mk-tile p (get map-marks mapsym) m mapsyms)
      (= mapsym "@ ")
        (setv player-start #(x y))
      (= mapsym ". ")
        None
      ; A few mapsyms have a default associated tile for convenience.
      (= mapsym "██")
        (mk-tile p "wall")
      (= mapsym "> ")
        (mk-tile p "exit")
      True (do
        (setv types (lfor
          t (.values Tile.types)
          :if (= t.mapsym mapsym)
          t))
        (unless types
          (raise (ValueError f"No match for {mapsym !r}")))
        (when (> (len types) 1)
          (raise (ValueError f"Ambiguous: {mapsym !r} - {types !r}")))
        (mk-tile p (. types [0] stem)))))
  #(m player-start))

(defn mk-tile [locator tile-spec [map-object None] [mapsyms None]]
  "Make a tile."
  (when (= tile-spec 'floor)
    ; Do nothing, since floor is actually the absence of tiles.
    (return))
  (if (isinstance tile-spec str)
    (setv  d {}  stem tile-spec)
    (setv  [stem #* d] tile-spec  d (kwdict d)))
  (when (isinstance (.get d "target") str)
    ; When it's a string, this attribute is treated specially: it
    ; should come in as a mapsym, and we change it to the `Pos` for
    ; that mapsym.
    (setv matches (lfor
      [xy mapsym] (.items mapsyms)
      :if (= mapsym (get d "target"))
      xy))
    (unless matches
      (raise (ValueError f"`target` not found: {mapsym}")))
    (when (> (len matches) 1)
      (raise (ValueError f"Ambiguous `target`: {mapsym}")))
    (setv (get d "target") (Pos map-object #* (get matches 0))))
  (Tile.make (locate locator) stem #** d))

;; --------------------------------------------------
;; * Load built-in quests
;; --------------------------------------------------

(setv builtin-quests (dfor
  ; Every module in this directory should have the members `name` and
  ; `quest-fn`.
  p (.iterdir (hy.I.pathlib.Path (get __path__ 0)))
  :if (not-in p.name ["__init__.hy" "__pycache__"])
  :setv m (hy.I.hyrule.import-path p)
  m.name m.quest-fn))
