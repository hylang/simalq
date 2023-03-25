(require
  simalq.macros [defdataclass])
(import
  re
  dataclasses [dataclass])
(setv  T True  F False)


(defdataclass Tile []
  [pos]
  :frozen T
  (setv types {})
  (setv types-by-iq-ix {})

  (setv
    ; Class variables overrriden by subclasses.
    article None
      ; "a", "the", etc.
    stem None
      ; The main part of the name.
    flavor None
      ; Flavor text to show in a help screen.
    iq-ix None)
      ; The number that represents this tile in IQ.

  (defn hook-player-walk-to [self origin])
  (defn hook-player-walk-from [self target]))


(defn deftile [superclass name #** kwargs]

  (setv article None)
  (setv stem (re.sub r"\A(a|an|the) "
    (fn [m] (nonlocal article) (setv article (.group m 1)) "")
    name))

  (assert (all (gfor  k kwargs  (hasattr superclass k))))

  (assert (not-in stem Tile.types))
  (setv (get Tile.types stem) ((dataclass :frozen T) (type
    stem
    #(superclass)
    (dict
      :article article
      :stem stem
      #** kwargs))))

  (when (setx iq-ix (.get kwargs "iq_ix"))
    (assert (not-in iq-ix Tile.types-by-iq-ix))
    (setv (get Tile.types-by-iq-ix iq-ix) (get Tile.types stem))))


(import ; For side-effects.
  simalq.tile.scenery
  simalq.tile.item
  simalq.tile.not-implemented)
