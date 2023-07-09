(require
  hyrule [unless]
  simalq.macros [has defn-dd fn-dd])
(import
  simalq.color :as color
  simalq.util [CommandError GameOverException player-melee-damage DamageType next-in-cycle StatusEffect hurt-player]
  simalq.geometry [Pos Direction pos+ at burst dist dir-to]
  simalq.tile [Tile deftile replace-tile damage-tile mv-tile destroy-tile]
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
    destructible-by-passwall-wand F)

  (defn dod [self prefix attr-sym]
    (setv m (getattr (type self) (hy.mangle attr-sym)))
    (setv r None)
    (when (is-not m (getattr Tile (hy.mangle attr-sym)))
      (setv r (if (hasattr m "dynadoc")
        (.dynadoc m self)
        m.__doc__)))
    (when r
      #(prefix r)))

  (defn info-bullets [self #* extra]
    (setv blocks-monster (or self.blocks-monster self.blocks-move))
    [
      (when self.damageable
        #("Hit points" self.hp))
      (when self.damageable
        (if self.immune
          #("Immune to" (.join ", " (gfor  x self.immune  x.value)))
          "No immunities"))
      (when self.blocks-move
        "Blocks all movement")
      (when (and
          (not self.blocks-move)
          (or self.blocks-monster G.rules.dainty-monsters))
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
      (when self.destructible-by-passwall-wand
        "Destructible by a wand of passwall")
      (when self.superblock
        "Not subject to magical transformation or passage")
      #* extra
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

  (setv target (pos+ p direction))
  (unless target
    (return #(None 'out-of-bounds)))
  #(target (cond
    (and
        direction.x direction.y
        (any (gfor
          p2 [
            (Pos target.map p.x target.y)
            (Pos target.map target.x p.y)]
          (has p2 Scenery it.blocks-diag))))
      'blocked-diag
    (nogo? target monster?)
      'bump
    True
      'walk)))

(defn nogo? [pos monster?]
  (or
    (and monster? G.rules.dainty-monsters (at pos))
    (and monster? (has pos Scenery it.blocks-monster))
    (has pos Scenery it.blocks-move)
    (has pos hy.M.simalq/tile.Monster T)))


(deftile Scenery "██" "a wall"
  :iq-ix 2
  :blocks-move T :blocks-diag T
  :destructible-by-passwall-wand T
  :flavor "Among the most numerous and persistent of the obstacles that stand in the way of your inevitable victory.\n\n    This man, with lime and rough-cast, doth present\n    Wall, that vile Wall which did these lovers sunder;\n    And through Wall's chink, poor souls, they are content\n    To whisper, at the which let no man wonder.")

(deftile Scenery "██" "the Void"
  :color color.void
  :iq-ix 17
  :blocks-move T :blocks-diag T
  :superblock T
  :flavor "Unshaped matter of the realm outside time and space. Mortal magic can't so much as make a dent in it.")

(deftile Scenery "| " "a pillar"
  :iq-ix 12
  :blocks-move T
  :destructible-by-passwall-wand T
  :flavor "A structure of vaguely Roman style.")

(deftile Scenery "╷ " "a broken pillar"
  :iq-ix 82
  :blocks-move T :blocks-player-shots F
  :destructible-by-passwall-wand T
  :flavor "It's just a chest-high half of a pillar now. Good thing it wasn't load-bearing, huh? It makes for good cover against enemy shots.")

(deftile Scenery "◀▶" "a hole"
  :color 'dark-gray
  :iq-ix 107  ; crevasse
  :blocks-move T :blocks-player-shots F :blocks-monster-shots F
  :flavor "Watch your step.")

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

(deftile LockedDoor " +" "a treasure chest"
  ; This unusual mapsym, having a space on the left but not the right,
  ; allows one character of the contained item to be visible.
  :color 'steel-blue
  :result-when-opened None
  :flavor "This locked strongbox is too tough to smash apart, but its boards are so warped that you can peek (or even shoot) at what's inside before you decide to spend a key on it.\n\n    We'll dig up the box.\n    We know it's full of precious booty.\n    Burst open the locks.\n    And then we'll say \"Hooray!\"")
(setv (get Tile.types-by-iq-ix 16) (fn [pos te-v1 te-v2]
  ; We represent an IQ treasure chest as two tiles: the treasure chest
  ; on top of the item contained within.
  [
    ((get Tile.types "treasure chest") :pos pos)
    #* (if (= te-v1 21) ; An unknown potion
         ((get Tile.types-by-iq-ix te-v1) pos None te-v2)
         [((get Tile.types-by-iq-ix te-v1) :pos pos)])]))


((fn []

  (defclass OneWayDoor [Scenery]

    (setv
      __slots__ []
      blocks-monster T
      direction None
      color #('brown 'red))

    (defn-dd hook-player-walk-from [self target]
      (doc f"Only allows you to walk {it.direction.name}.")
      (unless (= (pos+ self.pos self.direction) target)
        (raise (CommandError f"You can only go {self.direction.name} from this one-way door."))))
    (defn-dd hook-player-walk-to [self origin]
      (doc f"Only allows you to enter from the
        {it.direction.opposite.name}.")
      (unless (= (pos+ origin self.direction) self.pos)
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
  :blocks-player-shots F :blocks-monster-shots F

  :hook-player-walked-into (fn [self]
    "Takes you to the next dungeon level. If there is no such level,
    you win the quest."

    (when (> G.level.next-level (len G.quest.levels))
      (setv G.player.game-over-state 'won)
      (raise GameOverException))
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

  :suffix-dict (fn [self]
    (dict :HP self.hp))

  :blocks-move T :blocks-diag T :blocks-player-shots F
  :destructible-by-passwall-wand T
  :damageable T
  :immune #(DamageType.Poison DamageType.Fire DamageType.DeathMagic)
  :hook-player-bump (fn [self origin]
    "You attack the wall with your sword."
    (damage-tile self (player-melee-damage) DamageType.PlayerMelee)
    True)

  :flavor "I think this dungeon might not be up to code.")


(deftile Scenery "■ " "a pushblock"
  :iq-ix 22
    ; Called a "moveable wall" in IQ. I think "wall" is misleading
    ; because it's not a diagonal blocker.

  :blocks-monster T
  :destructible-by-passwall-wand T
  :hook-player-walk-to (fn [self origin]
    (setv target (pos+ self.pos (dir-to origin self.pos)))
    (when (or (not target) (at target))
      (raise (CommandError "There's no room to push the block there."))))

  :hook-player-walked-into (fn [self]
    "You push the block in the same direction that you entered the square. The destination square must be empty, or else you won't be able to step on the original square."
    (setv target (pos+ self.pos G.action.direction))
    (mv-tile self target)
    F)

  :flavor "Where do video games get all their crates from? There must be entire warehouses full of 'em, am I right?")


(deftile Scenery "{}" "a gate"
  :color 'purple
  :slot-defaults (dict
    :target None)
  :iq-ix 24

  :read-tile-extras (classmethod (fn [cls mk-pos v1 v2]
    (dict :target (mk-pos #(v1 v2)))))

  :suffix-dict (fn [self]
    (dict :dest self.target))
  :hook-player-walked-into (fn-dd [self]
    (doc f"Teleports you to {it.target}. Anything already there is unaffected.")
    (mv-tile G.player self.target)
    T)

  :flavor "A small stone arch containing a rippling, sparkling sheet of violet light. It functions as a magic portal that can send you elsewhere on this level. Sadly, arrows in flight won't survive the trip.")


(deftile Scenery "┣┫" "a teleporter"
  :color 'purple
  :slot-defaults (dict
    :times-entered 0
    :output-dir None)
  :mutable-slots #("times_entered" "output_dir")
  :iq-ix 23
  :blocks-diag T :blocks-monster T

  :hook-player-walked-into (fn [self]
    "Teleports you to a free square adjacent to the nearest other teleporter in the reality bubble. If there is no eligible destination teleporter, no teleportation occurs; if there are several tied for the nearest, you'll cycle through them when you re-enter this teleporter. Squares are considered to be free even if they contain monsters, which are slain instantly if you teleport into them. The free square that you appear on is cycled each time you re-exit a teleporter."

    (import simalq.tile [Monster])

    ; Find a set of teleporters we can go to and their free adjacent
    ; positions.
    (setv candidate-dist Inf)
    (setv candidates [])
    (for [p (burst G.player.pos G.rules.reality-bubble-size :exclude-center T)]
      (when (> (dist p self.pos) candidate-dist)
        ; When multiple teleporters are in range, we consider only
        ; the subset that's as close as possible.
        (break))
      (for [tile (at p)]
        (when (= tile.stem "teleporter")
          ; This position can be a candidate if it has at least one
          ; free adjacent position.
          (when (setx neighbors (tuple (gfor
              n-p (burst p 1 :exclude-center T)
              :if (all (gfor
                n-tile (at n-p)
                (isinstance n-tile Monster)))
              n-p)))
            (.append candidates #(tile neighbors))
            (setv candidate-dist (dist p self.pos)))
          (break))))

    ; Choose the other teleporter to use.
    (unless candidates
      (return F))
    (setv [other-porter neighbors] (get
      candidates
      (% self.times-entered (len candidates))))
    (+= self.times-entered 1)
    ; Choose the specific target square.
    (while True
      (setv other-porter.output-dir
        (next-in-cycle Direction.all other-porter.output-dir))
      (when (in
          (setx target (pos+ other-porter.pos other-porter.output-dir))
          neighbors)
        (break)))

    ; At the target, tele-frag all tiles, which we already know must
    ; be monsters.
    (for [tile (at target)]
      (damage-tile tile Inf None))

    ; Now actually move the player.
    (mv-tile G.player target)

    T)

  :info-bullets (fn [self #* extra]
    (Scenery.info-bullets self
      #("Times entered" self.times-entered)
      #("Output direction" self.output-dir)))

  :flavor "A bulky cubic device representing an early attempt at teleportation technology. Its operation is a bit convoluted. The fun part is, you can tele-frag with it.")


(defclass Trap [Scenery]
  (setv
    __slots__ []
    blocks-move F
    blocks-player-shots F
    blocks-monster-shots F))

(deftile Trap :name "a wallfall trap"
  :color 'dark-yellow
  :slot-defaults (dict
    :wallnum 1)
  :iq-ix-mapper ["wallnum" (do
    ; These are just called "traps" in IQ.
    (setv x [115 13 75 76 77 111 112 113 114])
    (dict (zip x (range (len x)))))]

  :mapsym (property (fn [self]
    (+ "<" (if (< self.wallnum 10) (str self.wallnum) "^"))))
  :suffix-dict (fn [self]
    (dict :type self.wallnum))
  :hook-player-walked-into (fn-dd [self]
    (doc (if (= it.wallnum 0)
      "Destroys all trapped walls and other wallfall traps on the level, regardless of type."
      f"Destroys all trapped walls on the level of type {it.wallnum} or 0, along with all other wallfall traps of type {it.wallnum}."))
    (for [col G.map.data  stack col  tile stack]
      (when (or
          (and (= tile.stem "trapped wall")
            (or (= self.wallnum 0) (in tile.wallnum [0 self.wallnum])))
          (and (= tile.stem "wallfall trap")
            (or (= self.wallnum 0) (= tile.wallnum self.wallnum))))
        (destroy-tile tile))))

  :flavor #[[Easy there, Admiral Ackbar. This kind of trap isn't necessarily dangerous. Well, admittedly, the key word here is "necessarily".]])

(deftile Scenery :name "a trapped wall"
  :color 'dark-yellow
  :slot-defaults (dict
    :wallnum 1)
  :iq-ix-mapper ["wallnum" (do
    (setv x [120 14 78 79 80 116 117 118 119])
    (dict (zip x (range (len x)))))]

  :mapsym (property (fn [self]
    (+ "█" (if (< self.wallnum 10) (str self.wallnum) "^"))))
  :suffix-dict (fn [self]
    (dict :type self.wallnum))
  :blocks-move T :blocks-diag T
  :destructible-by-passwall-wand T
  :info-bullets (fn [self #* extra]
    (Scenery.info-bullets self
      #("Wallfall type" self.wallnum)))

  :flavor "The special thing about this wall is that it can be destroyed by wallfall traps of the corresponding type.\n\nWhat's the deal with monster closets? Monsters are proud of who they are, am I right? I'll be here all week.")

(deftile Trap "<>" "a fixed damaging trap"
  :color 'red
  :iq-ix 35  ; damage-causing trap

  :hook-player-walked-into (fn-dd [self]
    (doc f"Does {trap-damage} damage.")
    (hurt-player trap-damage DamageType.Trap))

  :flavor "A dense assortment of pointy, painful objects that can penetrate the toughest footwear.")
(setv trap-damage 5)

(deftile Trap "<>" "a paralysis trap"
  :color 'purple
  :iq-ix 36

  :hook-player-walked-into (fn-dd [self]
    (doc f"Paralyzes you for {paralysis-duration} turns. While you're paralyzed, waiting is the only action you can take.")
    (+= (get G.player.status-effects StatusEffect.Para) paralysis-duration))

  :flavor "A magical field that causes you to vividly remember something embarrassing that you did as a teenager, forcing you to briefly freeze in horror. The problem with being royalty is that awkward adolescent moments all too easily become international incidents.")
(setv paralysis-duration 3)


(deftile Scenery "()" "a magical energy shield"
  :color 'dark-orange
  :slot-defaults (dict
    :time-remaining 12)
  :mutable-slots #("time_remaining")
  :iq-ix None
    ; In IQ, tiles of this type can only be created mid-game.

  :blocks-move F :blocks-monster T
  :blocks-player-shots F :blocks-monster-shots T

  :each-turn (fn [self]
    (-= self.time-remaining 1)
    (unless self.time-remaining
      (destroy-tile self)))

  :info-bullets (fn [self #* extra]
    (Scenery.info-bullets self
      #("Turns remaining" self.time-remaining)))

  :flavor "These glittering barriers of orange plasma offer you plenty of protection and monsters none at all. Enjoy 'em while they last.")
