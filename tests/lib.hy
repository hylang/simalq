(require
  hyrule [do-n unless])
(import
  simalq.geometry [Pos at Direction]
  simalq.un-iq [iq-quest]
  simalq.game-state [G]
  simalq.commands [Walk Wait Shoot UseItem]
  simalq.quest [start-quest start-level]
  simalq.quest-definition [mk-quest mk-tile locate]
  simalq.main [take-turn])


(defn init [#* levels [starting-hp 100] #** rules]
  (start-quest
    :quest (mk-quest #* levels :starting-hp starting-hp)
    :rules rules)
  (start-level 1))

(defn init-boot-camp [[level-n 1]]
  (start-quest (iq-quest "Boot Camp 2"))
  (start-level level-n))


(defn top [locator [attribute None]]
  (setv x (get (at (locate locator)) 0))
  (if attribute
    (getattr x (hy.mangle attribute))
    x))

(defn assert-hp [locator value]
  (assert (= (top locator 'hp) value)))

(defn assert-full-name [locator value]
  (setv actual (top locator 'full-name))
  ; Check for string equality, except that a period in `value` can be
  ; any one character.
  (assert (= (len actual) (len value)))
  (assert (all (gfor
    [a v] (zip actual value)
    (or (= v ".") (= a v))))))

(defn set-square [locator #* tile-specs]
  "Remove all tiles at the given square, then add new ones as
  requested."
  (setv p (locate locator))
  (for [tile (at p)]
    (.rm-from-map tile))
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
  (.move G.player (Pos G.map x y)))

(defn assert-player-at [x y]
  (assert (= G.player.pos (Pos G.map x y))))


(defmacro cant [form msg-check]
  (setv e (hy.gensym))
  `(do
    (with [~e (hy.I.pytest.raises hy.I.simalq/util.CommandError)]
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
