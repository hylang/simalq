(require
  hyrule [do-n unless])
(import
  fractions [Fraction]
  toolz [partition]
  simalq.geometry [Map Pos at pos+ Direction]
  simalq.quest [Quest Level]
  simalq.un-iq [read-quest iq-quest]
  simalq.game-state [G]
  simalq.tile [add-tile rm-tile]
  simalq.player-actions [Wait]
  simalq.main [start-quest start-level take-turn])


(defn kwdict [iterable]
  (dfor
    [k v] (partition 2 iterable)
    (hy.mangle k.name) v))


(defn init [quest [level-n 1]]
  (start-quest (if (isinstance quest str)
    (read-quest (iq-quest quest))
    quest))
  (when (!= level-n 1)
    (start-level level-n)))

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
      ; The requested tiles are placed in a line east of (0, 0).
    [title None]
    [next-level None]
    [poison-intensity (Fraction 0)]
    [time-limit None] [exit-speed None] [moving-exit-start None]]
  (setv m (Map.make :wrap-x wrap-x :wrap-y wrap-y :width width :height height))
  (for [[i tile-spec] (enumerate tiles)]
    (if (isinstance tile-spec str)
      (setv  d {}  stem tile-spec)
      (setv  [stem #* d] tile-spec  d (kwdict d)))
    (add-tile (Pos m (+ i 1) 0) stem #** d))
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
      G.player-pos
    (isinstance locator hy.models.Symbol)
      (pos+ G.player-pos (getattr Direction (str locator)))
    (isinstance locator Pos)
      locator))

(defn set-square [locator #* stems]
  "Remove all tiles at the given square, then add new ones as
  requested."
  (setv p (locate locator))
  (for [tile (at p)]
    (rm-tile tile))
  (for [stem stems]
    (add-tile p stem)))

(defn assert-at [locator thing]
  (setv stack (at (locate locator)))
  (if (= thing 'floor)
    (assert (= (len stack) 0))
    (do
      (unless (isinstance thing list)
        (setv thing [thing]))
      (assert (= (lfor  tile stack  tile.stem) thing)))))


(defmacro cant [form msg-check]
  (setv e (hy.gensym))
  `(do
    (with [~e (hy.M.pytest.raises hy.M.simalq/util.ActionError)]
      ~form)
    (assert (= (. ~e value args [0]) ~msg-check))))


(defmacro wk [direction-abbr [n-steps 1]]
  `(for [_ (range ~n-steps)]
    (hy.M.simalq/main.take-turn
      (hy.M.simalq/player-actions.Walk
        (. hy.M.simalq/geometry.Direction ~direction-abbr)))))

(defn wait [[n-times 1]]
  (do-n n-times
    (take-turn (Wait))))
