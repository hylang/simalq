(require
  hyrule [do-n unless])
(import
  math [ceil]
  re
  fractions [Fraction]
  toolz [partition]
  textwrap [dedent]
  simalq.geometry [Map Pos at pos+ Direction]
  simalq.quest [Quest Level]
  simalq.un-iq [iq-quest]
  simalq.game-state [G]
  simalq.tile [Tile add-tile rm-tile mv-tile]
  simalq.commands [Walk Wait Shoot UseItem]
  simalq.quest [start-quest start-level]
  simalq.main [take-turn])


(defn kwdict [iterable]
  (dfor
    [k v] (partition 2 iterable)
    (hy.mangle k.name) v))


(defn init [quest [level-n 1] #** rules]
  (start-quest
    :quest (if (isinstance quest str)
      (iq-quest quest)
      quest)
    :rules rules)
  (start-level level-n))

(defn mk-quest [
    #* levels
    [title None]
    [starting-hp 100]]
  (Quest :title title :starting-hp starting-hp :levels (tuple (gfor
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

(defn top [locator [attribute None]]
  (setv x (get (at (locate locator)) 0))
  (if attribute
    (getattr x (hy.mangle attribute))
    x))

(defn assert-hp [locator value]
  (assert (= (top locator 'hp) value)))

(defn mk-tile [locator tile-spec]
  (when (= tile-spec 'floor)
    (return))
  (if (isinstance tile-spec str)
    (setv  d {}  stem tile-spec)
    (setv  [stem #* d] tile-spec  d (kwdict d)))
  (add-tile (locate locator) stem #** d))

(defn set-square [locator #* tile-specs]
  "Remove all tiles at the given square, then add new ones as
  requested."
  (setv p (locate locator))
  (for [tile (at p)]
    (rm-tile tile))
  (for [tile-spec (reversed tile-specs)]
    (mk-tile p tile-spec)))

(defn assert-at [locator thing]
  (setv stack (at (locate locator)))
  (if (= thing 'floor)
    (assert (= (len stack) 0))
    (do
      (unless (isinstance thing list)
        (setv thing [thing]))
      (assert (=
        (lfor
          tile stack
          (if (is tile G.player) 'player tile.stem))
        thing)))))

(defn mv-player [x y]
  (mv-tile G.player (Pos G.map x y)))

(defn assert-player-at [x y]
  (assert (= G.player.pos (Pos G.map x y))))


(defmacro cant [form msg-check]
  (setv e (hy.gensym))
  `(do
    (with [~e (hy.M.pytest.raises hy.M.simalq/util.CommandError)]
      ~form)
    (assert (= (. ~e value args [0]) ~msg-check))))


(defn wk [direction-abbr [n-times 1]]
  (do-n n-times
    (take-turn (Walk (getattr Direction (str direction-abbr))))))

(defn wait [[n-times 1]]
  (do-n n-times
    (take-turn (Wait))))

(defn shoot [direction-abbr [n-times 1]]
  (do-n n-times
    (take-turn (Shoot (getattr Direction (str direction-abbr))))))

(defn use-item [item-ix [target-x None] [target-y None]]
  (take-turn (UseItem item-ix target-x target-y)))

(defn add-usable [stem [n 1]]
  (do-n n
    (.pick-up (mk-tile None stem))))
