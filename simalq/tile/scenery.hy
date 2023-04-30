(require
  hyrule [unless]
  simalq.macros [has defn-dd])
(import
  simalq.color :as color
  simalq.util [CommandError GameOverException player-melee-damage DamageType]
  simalq.geometry [Pos Direction GeometryError pos+ at]
  simalq.tile [Tile deftile replace-tile damage-tile destroy-tile]
  simalq.game-state [G])
(setv  T True  F False)


(defclass Scenery [Tile]
  "A mostly static part of the level, such as a wall or trap."

  (setv
    __slots__ []
    blocks-move F
      ; Block player and monster movement.
    blocks-diag F
      ; Block diagonal movement between orthogonally adjacent squares.
    blocks-monster F
      ; Block monster movement, even if `blocks-move` is false.
    superblock F)
      ; Resist all ordinary attempts to change or bypass the tile.

  (defn dod [self prefix attr-sym]
    (setv a (hy.mangle attr-sym))
    (when (is-not (getattr (type self) a) (getattr Tile a))
      #(prefix (or
        (. (getattr self a) __doc__)
        ((. (getattr self a) dynadoc) self)))))

  (defn info-bullets [self]
    (setv blocks-monster (or self.blocks-monster self.blocks-move))
    [
      (when self.damageable
        #("Hit points" self.hp))
      (when self.damageable
        (if self.immune
          #("Immune to" self.immune)
          "No immunities"))
      (when self.blocks-move
        "Blocks all movement")
      (when (and (not self.blocks-move) (or self.blocks-monster G.dainty-monsters))
        "Blocks monster movement")
      (when self.blocks-diag
        "Blocks diagonal movement around itself")
      (cond
        (and self.blocks-player-shots self.blocks-monster-shots)
          "Blocks all shots"
        self.blocks-player-shots
          "Blocks your shots, but not monsters' shots"
        self.blocks-monster-shots
          "Blocks monsters' shots, but not your shots")
      (when self.superblock
        "Not subject to magical transformation or passage")
      (.dod self "Effect when bumped" 'hook-player-bump)
      (.dod self "Effect when trying to enter" 'hook-player-walk-to)
      (.dod self "Effect when stepped onto" 'hook-player-walked-into)
      (.dod self "Effect when trying to exit" 'hook-player-walk-from)]))


(defn walkability [p direction monster?]
  "Can an actor at `p` walk in `direction`, or at least bump something
  there (e.g., attacking a monster), considering geometry and scenery?
  `monster?` should be true for a monster and false for the player.

  Return a 2-tuple. The first element is the target position (`None`
  if it's out of bounds) and the second is a symbol:

  - 'out-of-bounds
  - 'blocked-diag
  - 'bump (you can bump something there, but not go there)
  - 'walk (you can walk there)"

  (setv target (try
    (pos+ p direction)
    (except [e GeometryError]
      (return #(None 'out-of-bounds)))))
  #(target (cond
    (and
        direction.x direction.y
        (any (gfor
          p2 [
            (Pos target.map p.x target.y)
            (Pos target.map target.x p.y)]
          (has p2 Scenery it.blocks-diag))))
      'blocked-diag
    (or
        (and monster? G.rules.dainty-monsters (at target))
        (has target Scenery it.blocks-move)
        (has target hy.M.simalq/tile/monster.Monster T))
      'bump
    True
      'walk)))


(deftile Scenery "██" "a wall"
  :iq-ix 2
  :blocks-move T :blocks-diag T
  :flavor (.join "\n" [
    "Among the most numerous and persistent of the obstacles that stand in the way of your inevitable victory."
    ""
    "  This man, with lime and rough-cast, doth present"
    "  Wall, that vile Wall which did these lovers sunder;"
    "  And through Wall's chink, poor souls, they are content"
    "  To whisper, at the which let no man wonder."]))

(deftile Scenery "██" "the Void"
  :color color.void
  :iq-ix 17
  :blocks-move T :blocks-diag T
  :superblock T
  :flavor "Unshaped matter of the realm outside time and space. Mortal magic can't so much as make a dent in it.")

(deftile Scenery "| " "a pillar"
  :iq-ix 12
  :blocks-move T
  :flavor "A structure of vaguely Roman style.")

(deftile Scenery "++" "a door"
  :color 'brown
  :iq-ix 5
  :blocks-monster T
  :flavor "Unlocked, but it just won't stay open. Maybe that's for the best, since monsters are too dumb to operate it.")

(defclass LockedDoor [Scenery]
  (setv
    __slots__ []
    result-when-opened None
    blocks-monster T)

  (defn-dd hook-player-bump [self origin]
    (doc (+ "Consumes one key to "
      (if it.result-when-opened
        f"replace the tile with {(hy.repr it.result-when-opened)}."
        "destroy the tile.")))

    (unless G.player.keys
      (raise (CommandError "It's locked, and you're keyless at the moment.")))
    (-= G.player.keys 1)
    (if self.result-when-opened
      (replace-tile self self.result-when-opened)
      (destroy-tile self))
    True))

(deftile LockedDoor "++" "a locked door"
  :color 'navy
  :iq-ix 6
  :result-when-opened "door"
  :flavor "Fortunately, Tris knows how to pick locks. Unfortunately, she was wearing her hair down when she got whisked away to the dungeon, so she doesn't have any hairpins. You may have to use a key.")

(deftile LockedDoor "++" "a locked disappearing door"
  :color 'steel-blue
  :iq-ix 81
  :result-when-opened None
  :flavor "This advanced door destroys not only the key used to unlock it, but also itself. A true marvel of engineering.")

((fn []

  (defn safe-pos+ [pos direction]
    (try
      (pos+ pos direction)
      (except [GeometryError])))

  (defclass OneWayDoor [Scenery]

    (setv
      __slots__ []
      blocks-monster T
      direction None
      color #('brown 'red))

    (defn-dd hook-player-walk-from [self target]
      (doc f"Only allows you to walk {it.direction.name}.")
      (unless (= (safe-pos+ self.pos self.direction) target)
        (raise (CommandError f"You can only go {self.direction.name} from this one-way door."))))
    (defn-dd hook-player-walk-to [self origin]
      (doc f"Only allows you to enter from the
        {it.direction.opposite.name}.")
      (unless (= (safe-pos+ origin self.direction) self.pos)
        (raise (CommandError (.format "That one-way door must be entered from the {}."
          self.direction.opposite.name)))))

    (setv flavor "My way or the highway!"))

  (for [[direction iq-ix] [
      [Direction.N 8] [Direction.E 11]
      [Direction.S 9] [Direction.W 10]]]
    (setv c (get Direction.arrows direction))
    (deftile OneWayDoor f"+{c}" f"a one-way door ({direction.name})"
      :iq-ix iq-ix
      :direction direction))))

(deftile Scenery "> " "the exit"
  :color-bg 'lime
  :iq-ix 7
  :blocks-monster T

  :hook-player-walked-into (fn [self]
    "Takes you to the next dungeon level. If there is no such level,
    you win the quest."

    (when (> G.level.next-level (len G.quest.levels))
      (raise (GameOverException 'won)))
    (hy.M.simalq/main.start-level G.level.next-level)
    (setv G.player.just-exited T)
    True)

  :flavor "Get me outta here.")


(deftile Scenery "##" "a cracked wall"
  :slot-defaults (dict
    :hp 2)
  :mutable-slots #("hp")
  :iq-ix-mapper ["hp"
    {3 4  4 2  15 6}]

  :blocks-move T :blocks-diag T :blocks-player-shots F
  :damageable T
  :hook-player-bump (fn [self origin]
    "You attack the wall with your sword."
    (damage-tile self (player-melee-damage) DamageType.PlayerMelee)
    True)

  :flavor "I think this dungeon might not be up to code.")
