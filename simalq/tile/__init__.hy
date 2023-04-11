(require
  hyrule [unless]
  simalq.macros [slot-defaults])
(import
  copy [deepcopy]
  re
  simalq.color :as color
  simalq.game-state [G]
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
           (get cls.slot-defaults slot))))
    (when kwargs
      (raise (TypeError f"Illegal arguments: {(hy.repr kwargs)}"))))

  (defn __setattr__ [self name value]
    (if (in name (.all-mutable-slots self))
      (object.__setattr__ self name value)
      (raise (AttributeError f"Tried to set attribute {name !r} on an instance of {(type self) !r}. Use `object.__setattr__` if you really mean it."))))

  (defn __deepcopy__ [self memo]
    ; We provide this to avoid triggering `__setattr__` in `deepcopy`.
    ((type self) #** (dfor
      [cls slot] (.all-slots self)
      slot (deepcopy (getattr self slot) memo))))

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
      ; The first character may be a space, but the first shouldn't
      ; be.
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
     ; Attributes to be overriden in subclasses.
    points 0
      ; Points awarded for picking up an object, killing a monster,
      ; etc.
    damageable F
      ; Whether a tile of this kind can be hurt by the player's sword
      ; etc. To be overridden in subclasses, which, if they enable it,
      ; should include a slot `hp`.
    immune #())
      ; Damage types the tile ignores.

  (defn [classmethod] read-tile-extras [cls v1 v2]
    "This method should return a dictionary of instance variables
    to set for a new instance."
    (raise (TypeError (+ "Tile extras not implemented: " cls.stem))))

  (defn hook-player-bump [self origin]
    "Called when the player tries to walk towards this tile. Return
    true to end her turn."
    None)
  (defn hook-player-walk-from [self target]
    "Called when the player is about to walk from a square containing
    this tile. The hook shouldn't change the game state, but it can
    raise ActionError to halt the movement."
    None)
  (defn hook-player-walk-to [self origin]
    "Analogous to `Tile.hook-player-walk-from`."
    None)
  (defn hook-player-walked-into [self]
    "Called when the player successfully walks into this tile. Return
    true to end her turn."
    None)
  (defn hook-destroyed [self pos]
    "Called when the tile is destroyed by damage. The tile has already
    been removed, but its previous position is given by `pos`."
    None))


(defn deftile [superclass mapsym name #** kwargs]
  "Declare a new concrete and final tile type. Superclasses of tiles
  not meant to themselves be instantiated should be declared with
  `defclass`."

  (setv article None)
  (setv stem (re.sub r"\A(a|an|the) "
    (fn [m] (nonlocal article) (setv article (.group m 1)) "")
    name))
  (assert (= (len mapsym) 2))

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
        (dict :cls cls :slot slot :value slot-value)))))


(defn add-tile [pos stem #** kwargs]
  (.insert (at pos) 0 ((get Tile.types stem) :pos pos #** kwargs)))

(defn rm-tile [tile]
  (when (is-not tile.pos None)
    (.remove (at tile.pos) tile)
    (object.__setattr__ tile "pos" None)))

(defn mv-tile [tile pos]
  (rm-tile tile)
  (.insert (at pos) 0 tile)
  (object.__setattr__ tile "pos" pos))

(defn replace-tile [old new-stem]
  (setv
    (get (at old.pos) (.index (at old.pos) old))
    ((get Tile.types new-stem) :pos old.pos)))


(defn damage-tile [tile amount damage-type]
  (unless tile.damageable
    (raise TypeError))
  (when (in damage-type tile.immune)
    ; The tile shrugs off the attack.
    (return))
  (-= tile.hp amount)
  (when (<= tile.hp 0)
    ; It's destroyed.
    (+= G.score tile.points)
    (setv tile-was tile.pos)
    (rm-tile tile)
    (.hook-destroyed tile tile.pos)))


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
  ; For side-effects: namely, filling out `Tile.types` and
  ; `Tile.types-by-iq-ix`.
  simalq.tile.player
  simalq.tile.scenery
  simalq.tile.item
  simalq.tile.monster
  simalq.tile.not-implemented)
