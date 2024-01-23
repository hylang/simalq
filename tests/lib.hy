(require
  hyrule [do-n unless])
(import
  simalq.geometry [Pos at Direction]
  simalq.un-iq [iq-quest]
  simalq.game-state [G]
  simalq.commands [Walk Wait Shoot UseItem UseControllableTeleporter]
  simalq.quest [start-quest start-level]
  simalq.quest-definition [mk-quest mk-tile locate parse-text-map]
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
  (for [tile (list (at p))]
    (.rm-from-map tile))
  (for [tile-spec (reversed tile-specs)]
    (mk-tile p tile-spec)))

(defn assert-at [locator #* things]
  (setv stack (at (locate locator)))
  (if (= things #('floor))
    (assert (= (len stack) 0))
    (assert (=
      (tuple (gfor
        tile stack
        (if (is tile G.player) 'player tile.stem)))
      things))))

(defn assert-textmap [#* args #** kwargs]
  "Check that the current map looks like the given text map: it has the same
  dimensions and the same stack of stems on each square."
  (setv [model [player-x player-y]] (parse-text-map #* args #** kwargs))
  (.append
    (get model.data player-x player-y)
    (hy.I.simalq/tile.Player :pos (Pos model player-x player-y)))
  (assert (= G.map.width model.width))
  (assert (= G.map.height model.height))
  (for [[stack-actual stack-expected] (zip (sum :start #() G.map.data) (sum :start #() model.data))]
    (assert (= (len stack-actual) (len stack-expected)))
    (for [[t-actual t-expected] (zip stack-actual stack-expected)]
      (assert (= t-actual.stem t-expected.stem)))))

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

(defn use-item [thing [target-x None] [target-y None]]
  (when (isinstance thing int)
    (return (take-turn (UseItem thing target-x target-y))))
  (try
    (setv inv-was (.copy G.player.inventory))
    (setv (get G.player.inventory 0) None)
    (.pick-up (mk-tile None thing))
    (take-turn (UseItem 0 target-x target-y))
    (finally
      (setv (cut G.player.inventory) inv-was))))

(defn use-cport [direction-abbr target-x target-y]
   (take-turn (UseControllableTeleporter
     (getattr Direction (str direction-abbr)) target-x target-y)))
