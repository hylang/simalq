(import
  fractions [Fraction]
  simalq.util [GameOverException]
  simalq.tile [Tile deftile])
(setv  T True  F False)


(setv character-name "Princess Triskaidecagonn XIII")

(deftile Tile "@ " character-name
  ; A type representing the player-character.

  :slot-defaults (dict
    :hp 1
      ; How many hit points (HP) you have. When you run out, you die.
    :poison-dose (Fraction 0)
      ; The current amount of ambient poison breathed in. Amounts
      ; ≥ 1 get converted to damage.
    :just-exited F
      ; Did you just use an exit?
    :keys 0)
      ; How many (regular, and not yet used) keys you're carrying.
  :mutable-slots #("hp" "poison_dose" "just_exited" "keys")

  :damageable T
  :hook-destroyed (fn [self pos]
    (raise (GameOverException 'dead)))

  :flavor "People who've met Tris and Argonn separately are sometimes surprised to learn that they're siblings. They don't look much alike.")

(setv Player (get Tile.types character-name))
