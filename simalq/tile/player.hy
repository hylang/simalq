(import
  fractions [Fraction]
  simalq.tile [Tile deftile])

(setv character-name "Princess Triskaidecagonn XIII")

(deftile Tile character-name
  ; A type representing the player-character.

  :slot-defaults (dict
    :poison-dose (Fraction 0)
      ; The current amount of ambient poison breathed in. Amounts
      ; ≥ 1 get converted to damage.
    :keys 0)
      ; How many (regular, and not yet used) keys you're carrying.
  :mutable-slots #("poison_dose" "keys")

  :flavor "People who've met Tris and Argonn separately are sometimes surprised to learn that they're siblings. They don't look much alike.")

(setv Player (get Tile.types character-name))
