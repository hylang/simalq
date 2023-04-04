(import
  copy [deepcopy]
  re
  simalq.game-state [G]
  simalq.geometry [at])


(defclass Tile []
  (setv __slots__ ["pos"])
    ; `pos` is generally a `Pos`, but could be `None` for e.g. an item
    ; in the player's inventory.
  (setv types {})
  (setv types-by-iq-ix {})

  (defn __init__ [self #** kwargs]
    ; - The set of keyword arguments used must exactly equal the set of
    ;   available slots, minus slots initialized with `slot-init`.
    ; - All subclasses must set `__slots__` (if only to an empty list).
    ; - No slot named may be used twice in a single inheritance chain.
    (for [[cls slot] (.all-slots self)]
       (object.__setattr__ self slot
         (if (in slot (getattr cls "slot_init" {}))
           (get cls.slot-init slot)
           (.pop kwargs slot))))
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
      :if (not-in slot (getattr cls "slot_init" {}))
      slot (deepcopy (getattr self slot) memo))))

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

  ; Class variables overrriden by subclasses.

  (setv
    article None
      ; "a", "the", etc.
    stem None
      ; The main part of the name.
    flavor None
      ; Flavor text to show in a help screen.
    iq-ix None)
      ; The number that represents this tile in IQ.

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
    None))


(defn deftile [superclass name #** kwargs]
  "Declare a new concrete and final tile type. Superclasses of tiles
  not meant to themselves be instantiated should be declared with
  `defclass`."

  (setv article None)
  (setv stem (re.sub r"\A(a|an|the) "
    (fn [m] (nonlocal article) (setv article (.group m 1)) "")
    name))

  (assert (all (gfor  k kwargs  (hasattr superclass k))))
    ; New attributes should be introduced in a superclass. Otherwise,
    ; you're probably just typoing an attribute name.
  (.setdefault kwargs "__slots__" #())

  (assert (not-in stem Tile.types))
  (setv (get Tile.types stem) (type
    stem
    #(superclass)
    (dict
      :article article
      :stem stem
      #** kwargs)))

  (when (setx iq-ix (.get kwargs "iq_ix"))
    (assert (not-in iq-ix Tile.types-by-iq-ix))
    (setv (get Tile.types-by-iq-ix iq-ix) (get Tile.types stem))))


(defn add-tile [pos stem #** kwargs]
  (.append (at pos) ((get Tile.types stem) :pos pos #** kwargs)))

(defn rm-tile [tile]
  (.remove (at tile.pos) tile))

(defn mv-tile [tile pos]
  (rm-tile tile)
  (.append (at pos) tile)
  (object.__setattr__ tile "pos" pos))

(defn replace-tile [old new-stem]
  (setv
    (get (at old.pos) (.index (at old.pos) old))
    ((get Tile.types new-stem) :pos old.pos)))


(defclass Actor [Tile]
  "A kind of tile (typically a monster) that gets to do something each
  turn that it's in the reality bubble."

  (setv
    __slots__ ["last_acted"]
      ; The turn number on which the actor last got a turn.
    mutable-slots #("last_acted")
    slot-init {"last_acted" None})

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
  simalq.tile.scenery
  simalq.tile.item
  simalq.tile.monster
  simalq.tile.not-implemented)
