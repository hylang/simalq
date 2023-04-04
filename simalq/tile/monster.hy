(require
  hyrule [unless]
  simalq.macros [defdataclass])
(import
  enum [Enum]
  simalq.util [player-melee-damage DamageType hurt-player next-in-cycle]
  simalq.geometry [Direction adjacent? dir-to]
  simalq.game-state [G]
  simalq.tile [Actor deftile rm-tile mv-tile]
  simalq.tile.scenery [walkability])
(setv  T True  F False)


(setv AI (Enum "AI" ["Approach"]))

(defclass Monster [Actor]
  "A non-player character, typically out to kill the player."

  (setv
    __slots__ [
      "hp"
        ; The monster's number of hit points (HP). When a monster's
        ; HP hits 0, it dies.
      "movement_state"]
        ; A bit of memory or plan that the monster uses to choose
        ; its movements. Its meaning depends on `ai`.
    mutable-slots #("hp" "movement_state")
    slot-init {"movement_state" None}
    points None
      ; How many points the player gets for killing the monster (or
      ; just for damaging it, if it's a generated monster).
    ai AI.Approach
      ; The monster's basic artificial intelligence for deciding what
      ; to do on its turn.
    immune #()
      ; Damage types the monster ignores.
    damage-melee None
      ; How much damage the monster does with its basic melee attack.
    damage-shot None)
      ; Likewise for shots.

  (defn hook-player-bump [self origin]
    "Attack the monster in melee."
    (hurt-monster self (player-melee-damage) DamageType.PlayerMelee)
    True)

  (defn act [self]
    (when (adjacent? self.pos G.player-pos)
      ; We're in melee range of the player, so bonk her.
        (hurt-player self.damage-melee DamageType.MonsterMelee)
        ; That uses up our action.
        (return))
    (when (!= self.ai AI.Approach)
      (raise (ValueError "Other AIs are not yet implemented.")))

    ; Try to get closer to the player.
    (setv d (dir-to self.pos G.player-pos))
    (when (is d None)
      ; The player is in our square. Just give up.
      (return))

    (setv [target wly] (walkability self.pos d :monster? T))
    (unless (= wly 'walk)
      ; We can't go that way. Try a different direction.
      ; Use a non-random equivalent of IQ's `ApproachHero`.
      (setv self.movement-state
        (next-in-cycle Direction.orths self.movement-state))
      (setv d (tuple (gfor c ["x" "y"]
        (if (getattr self.movement-state c)
          (if (getattr d c)
            0
            (getattr self.movement-state c))
          (getattr d c)))))
      (unless (= d #(0 0))
        (setv d (get Direction.from-coords d))
        (setv [target wly] (walkability self.pos d :monster? T)))
      (unless (= wly 'walk)
        ; Per IQ, we make only one attempt to find a new direction.
        ; Give up.
        (return)))

    ; We're clear to move.
    (mv-tile self target)))

(defn hurt-monster [monster amount damage-type]
  (when (in damage-type monster.immune)
    ; The monster shrugs off the attack.
    (return))
  (-= monster.hp amount)
  (when (<= monster.hp 0)
    ; It's dead.
    (+= G.score monster.points)
    (rm-tile monster)))


(defclass NonGen [Monster]
  "A monster that isn't produced by a generator."

  (setv __slots__ [])

  (defn [classmethod] read-tile-extras [cls v1 v2]
    ; I have no clue what v1 means for most monsters. It's probably
    ; junk bits.
      (dict :hp v2)))


(deftile NonGen "a Dark Knight"
  :iq-ix 53
  :points 75

  :damage-melee 12

  :flavor "This dread warrior wears ink-black armor and carries a heavy chain mace. His devotion to the powers of evil (not to mention his willingness, nay, eagerness to kill you) make his appropriation of Batman's epithet questionable at best. When you get down to it, he's just trying to distract you from the fact that he's the most basic enemy in the whole dungeon.")
