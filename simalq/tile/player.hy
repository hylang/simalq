(require
  hyrule [unless]
  simalq.tile [deftile])
(import
  fractions [Fraction]
  simalq.game-state [G]
  simalq.color :as colors
  simalq.util [GameOverException DamageType msg refactor-hp hp-warning-threshold player-status flash-map]
  simalq.geometry [ray dir-to dist]
  simalq.tile [Tile Damageable])
(setv  T True  F False)


(deftile "@ " "Princess Triskaidecagonn XIII" Damageable
  ; A type representing the player-character.

  :field-defaults (dict
    :game-over-state None
      ; Set to a symbol when the game ends to indicate that the game
      ; has ended and how.
    :poison-dose (Fraction 0)
      ; The current amount of ambient poison breathed in. Amounts
      ; ≥ 1 get converted to damage.
    :just-exited F
      ; Did you just use an exit?
    :taking-extra-action F
      ; Are you currently taking a second (or third, or further)
      ; action for this turn? This is allowed by e.g.
      ; `StatusEffect.Fast`.
    :status-effects {}
      ; Time remaining for each `StatusEffect` (0 for not having the
      ; effect).
    :keys 0
      ; How many (regular, and not yet used) keys you're carrying.
    :magic-arrows 0
      ; How many magic arrows you have.
    :inventory []
      ; Single-use (`Usable`) items being carried.
    :artifacts {
      ; Boolean flags indicating which artifacts have been obtained.
        "Magic Shield" F
        "Elven Bow" F
        "Holy Sword" F}
    :floater-disturbance (Fraction 0))
      ; A measure of how much time you've spent adjacent to floaters.
  :mutable-fields (tuple (map hy.mangle '(game-over-state hp poison-dose just-exited taking-extra-action keys magic-arrows floater-disturbance)))
    ; `inventory` and `artifacts` should be mutated directly rather
    ; reassigned.

  :damage (meth [amount damage-type [animate T] [attacker None]]
    (unless amount
      (return))

    (when (and
        (get @artifacts "Magic Shield")
        (in damage-type #(DamageType.MonsterMelee DamageType.MonsterShot)))
      (setv amount (int (.__ceil__
        (* G.rules.artifact-shield-factor amount)))))

    (when animate
      (flash-map
        @pos
        colors.flash-player-damaged
        (+
          (if (and attacker attacker.pos)
            (ray @pos
              (dir-to @pos attacker.pos)
              (dist @pos attacker.pos))
            #())
          (if (player-status 'Ivln)
            #()
            #(@pos)))
        {@pos (if (> amount 99) "OW" (format amount "2"))}
        :flash-time-s .2))

    (setv hp-was @hp)
    (unless (player-status 'Ivln)
      (Damageable.damage @ amount damage-type))
    (when (chainc
           @hp
        <= (refactor-hp hp-warning-threshold)
        <  hp-was)
      (msg "Princess needs food badly!")))

  :destroy (meth []
    (raise (GameOverException 'dead)))

  :flavor "People who've met Tris and Argonn separately are sometimes surprised to learn that they're siblings. They don't look much alike.")

(setv Player (get Tile.types "Princess Triskaidecagonn XIII"))
