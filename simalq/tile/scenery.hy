(require
  simalq.macros [defdataclass]
  hyrule [unless])
(import
  re
  dataclasses [dataclass]
  simalq.geometry [NORTH EAST SOUTH WEST GeometryError pos+]
  simalq.tile [Tile]
  simalq.player-actions [ActionError])
(setv  T True  F False)


(defdataclass Scenery [Tile]
  []
  (setv types {})

  (setv
    article None
      ; "a", "the", etc.
    stem None
      ; The main part of the name.
    flavor None
      ; Flavor text to show in a help screen.
    iq-ix None
      ; The number that represents this tile in IQ.
    blocks-move F
      ; Block movement.
    blocks-diag F))
      ; Block diagonal movement between orthogonally adjacent squares.

(defn defscenery [name [superclass Scenery] #** kwargs]

  (setv article None)
  (setv stem (re.sub r"\A(a|an|the) "
    (fn [m] (nonlocal article) (setv article (.group m 1)) "")
    name))

  (assert (all (gfor  k kwargs  (hasattr superclass k))))

  (assert (not-in stem Scenery.types))
  (setv (get Scenery.types stem) ((dataclass :frozen T) (type
    stem
    #(superclass)
    (dict
      :article article
      :stem stem
      #** kwargs))))

  (when (setx iq-ix (.get kwargs "iq_ix"))
    (assert (not-in iq-ix Tile.types-by-iq-ix))
    (setv (get Tile.types-by-iq-ix iq-ix) (get Scenery.types stem))))


(defscenery "a wall"
  :iq-ix 2
  :blocks-move T :blocks-diag T
  :flavor (.join "\n" [
    "Among the most numerous and persistent of the obstacles that stand in the way of your inevitable victory."
    ""
    "  This man, with lime and rough-cast, doth present"
    "  Wall, that vile Wall which did these lovers sunder;"
    "  And through Wall's chink, poor souls, they are content"
    "  To whisper, at the which let no man wonder."]))

(defscenery "a pillar"
  :iq-ix 12
  :blocks-move T
  :flavor "A structure of vaguely Roman style.")

(defscenery "a door"
  :iq-ix 5
  :flavor "Unlocked, but it just won't stay open. Maybe that's for the best, since monsters are too dumb to operate it.")

((fn []

  (defn safe-pos+ [pos direction]
    (try
      (pos+ pos direction)
      (except [GeometryError])))

  (defdataclass OneWayDoor [Scenery]
    []

    (setv direction None)

    (defn hook-player-walk-to [self origin]
      (unless (= (safe-pos+ origin self.direction) self.pos)
        (setv op (get {NORTH SOUTH  EAST WEST  SOUTH NORTH  WEST EAST} self.direction))
        (raise (ActionError f"That one-way door must be entered from the {op.name}."))))
    (defn hook-player-walk-from [self target]
      (unless (= (safe-pos+ self.pos self.direction) target)
        (raise (ActionError f"You can only go {self.direction.name} from this one-way door."))))

    (setv flavor "My way or the highway!"))

  (for [[direction iq-ix] [[NORTH 8] [EAST 11] [SOUTH 9] [WEST 10]]]
    (defscenery f"a one-way door ({direction.name})" OneWayDoor
      :iq-ix iq-ix
      :direction direction))))
