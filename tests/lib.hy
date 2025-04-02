;; --------------------------------------------------------------
;; * Imports
;; --------------------------------------------------------------

(require
  hyrule [do-n unless])
(import
  toolz [concat]
  metadict [MetaDict]
  simalq.geometry [Pos at Direction]
  simalq.un-iq [iq-quest]
  simalq.game-state [G Rules]
  simalq.commands [Walk Wait Shoot UseItem UseControllableTeleporter]
  simalq.quest [start-quest start-level]
  simalq.quest-definition [mk-quest mk-tile locate parse-text-map]
  simalq.main [take-turn])

;; --------------------------------------------------------------
;; * Initialization functions
;; --------------------------------------------------------------

(defn init [
    #* levels
    #** kwargs]
  "(Re)initialize the global state."
  (setv rules (dict :player-starting-hp 100))
  ; Extract elements of `kwargs` that correspond to rules.
  (for [[k v] (list (.items kwargs))]
    (when (in k Rules.__dataclass_fields__)
      (setv (get rules k) (.pop kwargs k))))
  (when kwargs
    ; Any remaining `kwargs` should specify the only level, per
    ; `mk-level`, in place of a `levels` argument.
    (assert (not levels)))
  (unless levels
    (setv levels [kwargs]))
  (start-quest
    :quest (mk-quest #* levels #** rules))
  (start-level 1))

(defn init-boot-camp [[level-n 1]]
  (start-quest (iq-quest "Boot Camp 2"))
  (start-level level-n))

;; --------------------------------------------------------------
;; * Tiles
;; --------------------------------------------------------------

(defn top [locator [attribute None]]
  "Get the top tile at this location, and optionally an attribute thereof."
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
  "Check every tile at this square against a list of stems. Order
  matters."
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
  (for [
      [stack-actual stack-expected]
      (zip (concat G.map.data) (concat model.data))]
    (assert (= (len stack-actual) (len stack-expected)))
    (for [[t-actual t-expected] (zip stack-actual stack-expected)]
      (assert (= t-actual.stem t-expected.stem)))))

(defn mv-player [x y]
  (.move G.player (Pos G.map x y)))

(defn assert-player-at [x y]
  (assert (= G.player.pos (Pos G.map x y))))

;; --------------------------------------------------------------
;; * Player actions
;; --------------------------------------------------------------

(defmacro cant [form [msg-check None]]
  "Assert that the given action raises a `CommandError`."
  (setv e (hy.gensym))
  `(do
    (with [~e (hy.I.pytest.raises hy.I.simalq/util.CommandError)]
      ~form)
    ~@(when msg-check
      [`(assert (= (. ~e value args [0]) ~msg-check) (.format
        "Expected CommandError {}; got {}"
        (hy.repr (str ~msg-check))
        (hy.repr (. ~e value args [0]))))])))

(defn wk [direction-abbr [n-times 1]]
  "Walk."
  (do-n n-times
    (take-turn (Walk (getattr Direction (str direction-abbr))))))

(defn wait [[n-times 1]]
  (do-n n-times
    (take-turn (Wait))))

(defn shoot [direction-abbr [n-times 1]]
  (do-n n-times
    (take-turn (Shoot (getattr Direction (str direction-abbr))))))

(defn use-item [thing [locator None]]
  "`thing` can be an integer (meaning to use the inventory item with
  that index) or a string (meaning to add the item with that stem to
  the inventory and then immediately use it)."
  (setv target (if locator
    (locate locator)
    (MetaDict :x None :y None)))
  (when (isinstance thing int)
    (return (take-turn (UseItem thing target.x target.y))))
  (try
    (setv inv-was (.copy G.player.inventory))
    (setv (get G.player.inventory 0) None)
    (.pick-up (mk-tile None thing))
    (take-turn (UseItem 0 target.x target.y))
    (finally
      (setv (cut G.player.inventory) inv-was))))

(defn use-cport [direction-abbr target-x target-y]
   (take-turn (UseControllableTeleporter
     (getattr Direction (str direction-abbr)) target-x target-y)))
