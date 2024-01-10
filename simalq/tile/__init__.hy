(require
  hyrule [unless]
  simalq.macros [field-defaults defmeth defmacro-kwargs])
(import
  copy [deepcopy]
  re
  simalq.color :as color
  simalq.game-state [G]
  simalq.util [player-melee-damage DamageType]
  simalq.geometry [at])
(setv  T True  F False)


(defclass Tile []
  (field-defaults
    pos None)
      ; Generally this will actually be a `Pos`, but it could be
      ; `None` for e.g. an item in the player's inventory.
  (setv types {})
  (setv types-by-iq-ix {})

  (defmeth __init__ [#** kwargs]
    ; Each keyword argument must match a field.
    (for [field (@all-fields)]
      (object.__setattr__ @ field
        (if (in field kwargs)
          (.pop kwargs field)
          (deepcopy (next (gfor
            c (. (type @) __mro__)
            :if (in field (getattr c "field_defaults" {}))
            (get c.field-defaults field)))))))
    (when kwargs
      (raise (TypeError f"Illegal arguments: {(hy.repr kwargs)}"))))

  (defmeth __setattr__ [name value]
    (if (in name (@all-mutable-fields))
      (object.__setattr__ @ name value)
      (raise (AttributeError f"Tried to set attribute {name !r} on an instance of {(type @) !r}. Use `object.__setattr__` if you really mean it."))))

  (defmeth __deepcopy__ [memo]
    ; We provide this to avoid triggering `__setattr__` in `deepcopy`.
    (setv t (@__new__ (type @)))
    (for [field (@all-fields)]
      (object.__setattr__ t field (deepcopy (getattr @ field) memo)))
    t)

  (defmeth __setstate__ [state]
    ; We provide this to avoid triggering `__setattr__` in
    ; `pickle.load`.
    (for [[k v] (.items state)]
      (object.__setattr__ @ k v)))

  (defn [classmethod] all-fields [cls]
    (sfor
      c cls.__mro__
      field (.get c.__dict__ "fields" #())
      field))

  (defn [classmethod] all-mutable-fields [cls]
    (sfor
      c cls.__mro__
      field (.get c.__dict__ "mutable_fields" #())
      field))

  (defn [classmethod property] name-with-article [cls]
    (+ (if cls.article (+ cls.article " ") "") cls.stem))

  (defmeth [property] full-name []
    (setv suffix-items (.items (@suffix-dict)))
    (+ @name-with-article (if suffix-items
      (.format " ({})" (.join ", " (gfor
        [k v] suffix-items
        f"{k} {v}")))
      "")))

  (defmeth [classmethod] make [pos stem [stack-ix 0] #** kwargs]
    "Create a new tile."
    (setv t ((get Tile.types stem) :pos pos #** kwargs))
    (when pos
      (.insert (at pos) stack-ix t))
    t)

  (defmeth rm-from-map []
    "If this tile is on a map, remove it from the map. Otherwise, do
    nothing."
    (when (is-not @pos None)
      (.remove (at @pos) @)
      (object.__setattr__ @ "pos" None)))

  (defmeth move [pos]
    (@rm-from-map)
    (.insert (at pos) 0 @)
    (object.__setattr__ @ "pos" pos))

  (defmeth replace [new-stem]
    (setv
      (get (at @pos) (.index (at @pos) @))
      ((get Tile.types new-stem) :pos @pos)))

  ; The below variables and methods may be overridden by subclasses.

  (setv
    mutable-fields #()
      ; Fields whose values should be freely adjustable with `setv`,
      ; `+=`, etc.
    declare #()
      ; New class variables to introduce in a `deftile`.
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
      ; The number that represents this tile in IQ. It can also be a
      ; tuple, in which case all included numbers will be translated to
      ; this tile.
    iq-ix-mapper None
      ; An alternative to `iq-ix` for many-to-one matchups from IQ to
      ; SQ tiles. It should be a list like
      ;   ["hp" {1 2  3 4  5 6}]
      ; where the first element is a field name and the second is a
      ; dictionary mapping IQ values to values for the field.
    blocks-player-shots T
      ; Whether the player's arrows are stopped by this tile.
    blocks-monster-shots T
      ; Whether monsters are prevented from shooting by this tile.
    superblock F)
      ; Resist all ordinary attempts to change or bypass the tile.

  (defn [classmethod] read-tile-extras [cls mk-pos v1 v2]
    "This method should return a dictionary of instance variables
    to set for a new instance."
    (raise (TypeError (+ "Tile extras not implemented: " cls.stem))))

  (defmeth suffix-dict []
    "Return a dictionary of things to append to the full name of the
    tile"
    {})

  (defmeth info-bullets [#* bullets]
    "Return a list of bulleted items for an info screen. `None`s
    in this list will be filtered out by the caller."
    bullets)

  (defmeth hook-player-bump [origin]
    "Called when the player tries to walk towards this tile. Return
    true to end her turn."
    None)
  (defmeth hook-player-walk-from [target]
    "Called when the player is about to walk from a square containing
    this tile. The hook shouldn't change the game state, but it can
    raise CommandError to halt the movement."
    None)
  (defmeth hook-player-walk-to [origin]
    "Analogous to `Tile.hook-player-walk-from`."
    None)
  (defmeth hook-player-walked-into []
    "Called when the player successfully walks into this tile. Return
    true to end her turn."
    None)
  (setv hook-player-shot None))
    ; Called when the player hits this tile with an arrow. If this
    ; hook exists at all, that means an arrow can hit the tile, and
    ; further processing of this arrow against this tile will stop
    ; after the hook is called. (A magic arrow might still keep going
    ; to other tiles.)


(defmacro-kwargs deftile [mapsym name superc #** kwargs]
  "Declare and return a new concrete and final tile type. Superclasses
  of tiles not meant to themselves be instantiated should be declared
  with `defclass`."

  (setv superc (if (isinstance superc hy.models.List)
    superc
    `[~superc]))

  (setv (get kwargs "article") None)
  (setv (get kwargs "stem") (re.sub r"\A(a|an|the|some) "
    (fn [m] (setv (get kwargs "article") (.group m 1)) "")
    name))

  (defn un! [x]
    (.removeprefix x "hyx_Xexclamation_markX"))

  `(defclass
    [hy.I.simalq/tile.tiletype]
    ~(hy.models.Symbol (+ "TileType_" (.join "" (gfor
      c (get kwargs "stem")
      (if (or (= c " ") (in c hy.reader.HyReader.NON_IDENT)) "_" c)))))
    ~superc
    ; Attributes whose names begin with a exclamation point, as in
    ; `:!length 5`, are considered to be newly declared and so
    ; exempted from the unknown-attributes check.
    (setv declare ~(tuple (gfor
      k (.keys kwargs)
      :if (!= k (un! k))
      (un! k))))
    ~@(gfor
      [k v] (.items (dict :mapsym mapsym #** kwargs))
      ; Treat `(meth …)` and `(property-meth …)` forms specially.
      (cond
        (and (isinstance v hy.models.Expression) (= (get v 0) 'meth))
          `(hy.R.simalq/macros.defmeth ~(hy.models.Symbol k)
            ~@(cut v 1 None))
        (and (isinstance v hy.models.Expression) (= (get v 0) 'property-meth))
          `(hy.R.simalq/macros.defmeth [property] ~(hy.models.Symbol k)
            ~@(cut v 1 None))
        True
          `(setv ~(hy.models.Symbol (un! k)) ~v)))))


(defn tiletype [cls]
  "A decorator used by `deftile`."

  (assert (or (isinstance cls.mapsym property) (= (len cls.mapsym) 2)))

  (setv unknown-attrs (lfor
    k (dir cls)
    :if (not-in k cls.declare)
    :if (not (any (gfor
      c cls.__mro__
      :if (is-not c cls)
      (in k c.__dict__))))
    k))
  (when unknown-attrs
    (raise (TypeError f"Unknown attributes: {unknown-attrs}")))
      ; New attributes should be introduced in a superclass or declared with
      ; an exclamation point.

  (when (and (in "field_defaults" cls.__dict__) (not-in "fields" cls.__dict__))
    (setv cls.fields (tuple (.keys cls.field-defaults))))

  (assert (not-in cls.stem Tile.types))
  (setv (get Tile.types cls.stem) cls)

  (when (setx iq-ix cls.iq-ix)
    (for [i (if (isinstance iq-ix int) [iq-ix] iq-ix)]
      (assert (not-in i Tile.types-by-iq-ix))
      (setv (get Tile.types-by-iq-ix i) cls)))
  (when cls.iq-ix-mapper
    (setv [field d] cls.iq-ix-mapper)
    (assert (in field (.all-fields cls)))
    (for [[iq-ix field-value] (.items d)]
      (assert (not-in iq-ix Tile.types-by-iq-ix))
      (setv (get Tile.types-by-iq-ix iq-ix)
        (dict :cls cls :field field :value field-value))))

  (hy.repr-register cls (fn [x]
    (.format "(<{}> {})" stem (.join " " (gfor
      s (.all-fields x)
      (.format ":{} {}" (hy.unmangle s) (hy.repr (getattr x s))))))))

  cls)


(defclass Actor [Tile]
  "A kind of tile (typically a monster) that gets to do something each
  turn that it's in the reality bubble."

  (field-defaults
    last-acted None)
  (setv mutable-fields #("last_acted"))

  (defmeth maybe-act []
    "Act, if we haven't already acted this turn."
    (when (or (is @last-acted None) (< @last-acted G.turn-n))
      (@act)
      (setv @last-acted G.turn-n)))

  (defmeth act []
    (raise NotImplementedError)))


(defclass EachTurner [Tile]
  "Like `Actor`, but the method can still fire outside the reality
  bubble, and it goes after all actors."

  (defmeth __init__ [#** kwargs]
    (.__init__ (super) #** kwargs)
    (.append G.each-turn @))

  (defmeth [classmethod] run-all []
    "Run all currently registered hooks. Unless an object is neither
    on this level nor in the player's inventory, in which case, kick
    the object off the list."
    (for [o (list G.each-turn)]
      (unless (or (in o G.player.inventory) (and o.pos (is o.pos.map G.map)))
        (.remove G.each-turn o))
      (.each-turn o)))

  (defmeth each-turn []
    (raise NotImplementedError)))


(defclass Damageable [Tile]
  "Tiles that can take damage, like from being attacked by the player
  or a monster, and track hit points.

  Instances ignore the attribute `Tile.blocks-player-shots`."

  (field-defaults
    hp 1)
      ; The tile's number of hit points (HP). When its HP hits 0,
      ; it's destroyed.

  (setv
    mutable-fields #("hp")
    immune #()
      ; Damage types the tile ignores.
    resists #()
      ; Damage types the tile can only take 1 damage from. Immunities
      ; apply first (but you should avoid having the same damage type
      ; in each).
    weaknesses #()
      ; Damage types that can instantly destroy the tile. Immunities
      ; apply first.
    destruction-points 0
      ; Points awarded for destroying the tile.
    score-for-damaging F)
      ; If true, you get the tile's point value per HP of damage you
      ; do to it (with no points for overkill). Otherwise, you get
      ; its point value for destroying it (or picking it up, if it's
      ; an item).

  (defmeth hook-player-bump [origin]
    "You attack the tile with your sword."
    (@damage (player-melee-damage) DamageType.PlayerMelee)
    True)

  (defmeth damage [amount damage-type]
    "Take some damage. `amount` can be `Inf` or a nonnegative `int`.
    `damage-type` can be `None` for non-modifiable pure damage."

    (when (= amount 0)
      (return))
    (when (in damage-type @immune)
      (return))
    (when (in damage-type @resists)
      (setv amount 1))
    (when (or (= amount Inf) (in damage-type @weaknesses))
      ; This will be a one-hit kill.
      (setv amount @hp))
    (-= @hp amount)
    (when @score-for-damaging
      (+= G.score (* @destruction-points (min amount (+ @hp amount)))))
        ; No extra points are awarded for overkill damage.
    (when (<= @hp 0)
      (@destroy)))

  (defmeth destroy []
    (unless @score-for-damaging
      (+= G.score @destruction-points))
    (@rm-from-map))

  (defmeth suffix-dict []
    (dict :HP @hp))

  (defmeth info-bullets [#* extra]
    (.info-bullets (super)
      #("Hit points" @hp)
      (if @immune
        #("Immune to" (.join ", " (gfor  x @immune  x.value)))
        "No immunities")
      (when @resists
        #("Takes no more than 1 damage from" (.join ", " (gfor  x @resists  x.value))))
      (when @weaknesses
        #("Instantly destroyed by" (.join ", " (gfor  x @weaknesses  x.value))))
      #* extra
      #("Point value" (.format "{:,}{}"
        @destruction-points
        (if @score-for-damaging " (scored per HP lost)" ""))))))


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
