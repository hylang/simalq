(import
  re
  simalq.geometry [at])


(defclass Tile []
  (setv __slots__ ["pos"])
  (setv types {})
  (setv types-by-iq-ix {})

  (defn __init__ [self * pos]
    (object.__setattr__ self "pos" pos))

  (defn __setattr__ [self name value]
    (raise (AttributeError f"Tried to set attribute {name !r} on an instance of {(type self) !r}. Attributes of tiles should be set with properties, or `object.__setattr__` if you really mean it.")))

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

  (defn hook-player-walk-from [self target])
  (defn hook-player-walk-to [self origin])
  (defn hook-player-walked-into [self]))


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


(defn add-tile [pos stem]
  (.append (at pos) ((get Tile.types stem) :pos pos)))

(defn rm-tile [tile]
  (.remove (at tile.pos) tile))

(defn replace-tile [old new-stem]
  (setv
    (get (at old.pos) (.index (at old.pos) old))
    ((get Tile.types new-stem) :pos old.pos)))


(import
  ; For side-effects: namely, filling out `Tile.types` and
  ; `Tile.types-by-iq-ix`.
  simalq.tile.scenery
  simalq.tile.item
  simalq.tile.not-implemented)
