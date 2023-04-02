(require
  simalq.macros [defdataclass])
(import
  enum [Enum]
  simalq.util [player-melee-damage DamageType hurt-player]
  simalq.geometry [adjacent?]
  simalq.game-state [G]
  simalq.tile [Actor deftile rm-tile])


(setv AI (Enum "AI" ["Approach"]))

(defclass Monster [Actor]
  "A non-player character, typically out to kill the player."

  (setv
    __slots__ ["hp"]
      ; The monster's number of hit points (HP). When a monster's
      ; HP hits 0, it dies.
    mutable-slots #("hp")
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

  (defn hook-player-walk-to [self origin]
    "Attack the monster in melee."
    (hurt-monster self (player-melee-damage) DamageType.PlayerMelee)
    True)

  (defn act [self]
    (when (adjacent? self.pos G.player-pos)
      ; We're in melee range of the player, so bonk her.
        (hurt-player self.damage-melee DamageType.MonsterMelee)
        ; That uses up our action.
        (return))
    (raise (ValueError "Other monster actions are not yet implemented."))))

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
