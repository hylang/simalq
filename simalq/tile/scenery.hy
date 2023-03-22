(require
  simalq.macros [defdataclass])
(import
  re
  dataclasses [dataclass]
  simalq.tile [Tile])
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
