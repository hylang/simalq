(require
  hyrule [unless]
  simalq.macros [slot-defaults])
(import
  copy [deepcopy]
  re
  simalq.color :as color
  simalq.game-state [G]
  simalq.util [GameOverException]
  simalq.geometry [at])
(setv  T True  F False)


(defclass Tile []
  (slot-defaults
    pos None)
      ; Generally this will actually be a `Pos`, but it could be
      ; `None` for e.g. an item in the player's inventory.
  (setv types {})
  (setv types-by-iq-ix {})

  (defn __init__ [self #** kwargs]
    ; - Each keyword argument must match a slot.
    ; - All subclasses must set `__slots__` (if only to an empty list).
    ; - No slot named may be used twice in a single inheritance chain.
    (for [[cls slot] (.all-slots self)]
       (object.__setattr__ self slot
         (if (in slot kwargs)
           (.pop kwargs slot)
           (deepcopy (get cls.slot-defaults slot)))))
    (when kwargs
      (raise (TypeError f"Illegal arguments: {(hy.repr kwargs)}")))
    (when self.each-turn
      (.append G.each-turn self)))

  (defn __setattr__ [self name value]
    (if (in name (.all-mutable-slots self))
      (object.__setattr__ self name value)
      (raise (AttributeError f"Tried to set attribute {name !r} on an instance of {(type self) !r}. Use `object.__setattr__` if you really mean it."))))

  (defn __deepcopy__ [self memo]
    ; We provide this to avoid triggering `__setattr__` in `deepcopy`.
    (setv t (.__new__ self (type self)))
    (for [[_ slot] (.all-slots self)]
      (object.__setattr__ t slot (deepcopy (getattr self slot) memo)))
    t)

  (defn __setstate__ [self state]
    ; We provide this to avoid triggering `__setattr__` in
    ; `pickle.load`.
    (setv [_ slot-dict] state)
    (for [[k v] (.items slot-dict)]
      (object.__setattr__ self k v)))

  (defn [classmethod] all-slots [cls]
    (lfor
      c cls.__mro__
      :if (is-not c object)
      s c.__slots__
      #(c s)))

  (defn [classmethod] all-mutable-slots [cls]
    (sfor
      c cls.__mro__
      s (getattr c "mutable_slots" #())
      s))

  (defn [classmethod property] name-with-article [cls]
    (+ (if cls.article (+ cls.article " ") "") cls.stem))

  (defn [property] full-name [self]
    (setv suffix-items (.items (.suffix-dict self)))
    (+ self.name-with-article (if suffix-items
      (.format " ({})" (.join ", " (gfor
        [k v] suffix-items
        f"{k} {v}")))
      "")))

  ; The below variables and methods may be overridden by subclasses.

  (setv
    mutable-slots #()
      ; Slots whose values should be freely adjustable with `setv`,
      ; `+=`, etc.
    article None
      ; "a", "the", etc.
    stem None
      ; The main part of the name.
    mapsym "� "
      ; A two-character symbol to represent the tile on screen.
      ; Space characters are treated as transparent, allowing mapsyms
      ; of below tiles to peek through.
    color color.default-fg
      ; A foreground color for the mapsym. It can be a single color
      ; or a tuple of two colors (for the two characters).
    color-bg None
      ; Similar, for the background color. `None` means allowing
      ; other background-color effects, like the reality fringe, to
      ; determine it.
    flavor None
      ; Flavor text to show in a help screen.
    iq-ix None
      ; The number that represents this tile in IQ.
    iq-ix-mapper None
      ; An alternative to `iq-ix` for many-to-one matchups from IQ to
      ; SQ tiles. It should be a list like
      ;   ["hp" {1 2  3 4  5 6}]
      ; where the first element is a slot name and the second is a
      ; dictionary mapping IQ values to values for the slot.
    points 0
      ; Points awarded for picking up an object, killing a monster,
      ; etc.
    damageable F
      ; Whether a tile of this kind can be hurt by the player's sword
      ; etc. To be overridden in subclasses, which, if they enable it,
      ; should include a slot `hp`.
    immune #()
      ; Damage types the tile ignores.
    resists #()
      ; Damage types the tile can only take 1 damage from. Immunities
      ; apply first (but you should avoid having the same damage type
      ; in each).
    weaknesses #()
      ; Damage types that can instantly destroy the tile. Immunities
      ; apply first.
    score-for-damaging F
      ; If true, you get the tile's point value per HP of damage you
      ; do to it (with no points for overkill). Otherwise, you get
      ; its point value for destroying it (or picking it up, if it's
      ; an item).
    blocks-player-shots T
      ; Whether the player's arrows are stopped by this tile.
      ; If `damageable` is true, it overrides this.
    blocks-monster-shots T
      ; Whether monsters are prevented from shooting by this tile.
    superblock F
      ; Resist all ordinary attempts to change or bypass the tile.
    each-turn None)
      ; Set this to a method to get instances added to `G.each-turn`
      ; upon creation. The method will be called in the main turn loop.

  (defn [classmethod] read-tile-extras [cls mk-pos v1 v2]
    "This method should return a dictionary of instance variables
    to set for a new instance."
    (raise (TypeError (+ "Tile extras not implemented: " cls.stem))))

  (defn suffix-dict [self]
    "Return a dictionary of things to append to the full name of the
    tile"
    {})

  (defn info-bullets [self]
    "Return a list of bulleted items for an info screen. `None`s
    in this list will be filtered out by the caller."
    [])

  (defn hook-player-bump [self origin]
    "Called when the player tries to walk towards this tile. Return
    true to end her turn."
    None)
  (defn hook-player-walk-from [self target]
    "Called when the player is about to walk from a square containing
    this tile. The hook shouldn't change the game state, but it can
    raise CommandError to halt the movement."
    None)
  (defn hook-player-walk-to [self origin]
    "Analogous to `Tile.hook-player-walk-from`."
    None)
  (defn hook-player-walked-into [self]
    "Called when the player successfully walks into this tile. Return
    true to end her turn."
    None)
  (setv hook-player-shot None)
    ; Called when the player hits this tile with an arrow. If this
    ; hook exists at all, that means an arrow can hit the tile, and
    ; further processing of this arrow against this tile will stop
    ; after the hook is called. (A magic arrow might still keep going
    ; to other tiles.)
  (defn hook-destroyed [self pos]
    "Called when the tile is destroyed by damage. The tile has already
    been removed, but its previous position is given by `pos`."
    None))


(defn deftile [superclass mapsym name #** kwargs]
  "Declare and return a new concrete and final tile type. Superclasses
  of tiles not meant to themselves be instantiated should be declared
  with `defclass`."

  (setv article None)
  (setv stem (re.sub r"\A(a|an|the|some) "
    (fn [m] (nonlocal article) (setv article (.group m 1)) "")
    name))
  (assert (or (isinstance mapsym property) (= (len mapsym) 2)))

  (setv new-attrs (lfor  k kwargs  :if (not (hasattr superclass k))  k))
  (when new-attrs
    (raise (TypeError f"Unknown attributes: {new-attrs}")))
      ; New attributes should be introduced in a superclass. Otherwise,
      ; you're probably just typoing an attribute name.
  (.setdefault kwargs "__slots__" (if (in "slot_defaults" kwargs)
    (tuple (.keys (get kwargs "slot_defaults")))
    #()))

  (setv cls (type
    stem
    #(superclass)
    (dict
      :article article
      :stem stem
      :mapsym mapsym
      #** kwargs)))

  (assert (not-in stem Tile.types))
  (setv (get Tile.types stem) cls)

  ; Also add the new class as a global variable of `simalq.tile` so
  ; `pickle` can find it.
  (setv (get (globals) stem) cls)

  (when (setx iq-ix (.get kwargs "iq_ix"))
    (assert (not-in iq-ix Tile.types-by-iq-ix))
    (setv (get Tile.types-by-iq-ix iq-ix) cls))
  (when (in "iq_ix_mapper" kwargs)
    (setv [slot d] (get kwargs "iq_ix_mapper"))
    (assert (in slot (sfor  [_ s] (.all-slots cls)  s)))
    (for [[iq-ix slot-value] (.items d)]
      (assert (not-in iq-ix Tile.types-by-iq-ix))
      (setv (get Tile.types-by-iq-ix iq-ix)
        (dict :cls cls :slot slot :value slot-value))))

  (hy.repr-register cls (fn [x]
    (.format "(<{}> {})" stem (.join " " (gfor
      [_ s] (.all-slots x)
      (.format ":{} {}" (hy.unmangle s) (hy.repr (getattr x s))))))))

  cls)


(defn add-tile [pos stem #** kwargs]
  (setv t ((get Tile.types stem) :pos pos #** kwargs))
  (when pos
    (.insert (at pos) 0 t))
  t)

(defn rm-tile [tile]
  (when (is-not tile.pos None)
    (.remove (at tile.pos) tile)
    (object.__setattr__ tile "pos" None)))

(defn destroy-tile [tile]
  (setv pos-was tile.pos)
  (rm-tile tile)
  (.hook-destroyed tile pos-was))

(defn mv-tile [tile pos]
  (rm-tile tile)
  (.insert (at pos) 0 tile)
  (object.__setattr__ tile "pos" pos))

(defn replace-tile [old new-stem]
  (setv
    (get (at old.pos) (.index (at old.pos) old))
    ((get Tile.types new-stem) :pos old.pos)))


(defn damage-tile [tile amount damage-type]
  "`amount` can be `Inf` or a nonnegative `int`. `damage-type` can be
  `None` for non-modifiable pure damage."

  (when (= amount 0)
    (return))
  (unless tile.damageable
    (raise TypeError))
  (when (in damage-type tile.immune)
    ; The tile shrugs off the attack.
    (return))
  (when (in damage-type tile.resists)
    ; The tile can't take more than 1 damage.
    (setv amount 1))
  (when (or (= amount Inf) (in damage-type tile.weaknesses))
    ; This will be a one-hit kill.
    (setv amount tile.hp))
  (-= tile.hp amount)
  (when tile.score-for-damaging
    (+= G.score (* tile.points (min amount (+ tile.hp amount)))))
      ; No extra points are awarded for overkill damage.
  (when (<= tile.hp 0)
    ; It's destroyed.
    (if (is tile G.player)
      (do
        (setv G.player.game-over-state 'dead)
        (raise GameOverException))
      (do
        (unless tile.score-for-damaging
          (+= G.score tile.points))
        (destroy-tile tile)))))


(defclass Actor [Tile]
  "A kind of tile (typically a monster) that gets to do something each
  turn that it's in the reality bubble."

  (slot-defaults
    last-acted None)
  (setv mutable-slots #("last_acted"))

  (defn maybe-act [self]
    "Act, if we haven't already acted this turn."
    (when (or (is self.last-acted None) (< self.last-acted G.turn-n))
      (.act self)
      (setv self.last-acted G.turn-n)))

  (defn act [self]
    (raise (TypeError f"No `act` method defined for actor {(type self)}"))))


(import
  ; Chiefly for side-effects: namely, filling out `Tile.types` and
  ; `Tile.types-by-iq-ix`.
  simalq.tile.player [Player]
  simalq.tile.scenery [Scenery]
  simalq.tile.item [Item]
  simalq.tile.monster [Monster]
  simalq.tile.unimplemented [UnimplementedTile])

(setv Tile.superclasses (do-mac (dfor
  c '[Scenery Item Monster Player]
  (str c) c)))
