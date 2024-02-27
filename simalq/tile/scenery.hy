;; --------------------------------------------------------------
;; * Imports
;; --------------------------------------------------------------

(require
  hyrule [unless]
  simalq.macros [defmeth]
  simalq.tile [deftile])
(import
  fractions [Fraction :as f/]
  simalq.color :as color
  simalq.util [CommandError DamageType next-in-cycle StatusEffect]
  simalq.geometry [Pos Direction at burst dist dir-to ray]
  simalq.tile [Tile Actor PosHooked EachTurner Damageable annihilate]
  simalq.game-state [G])
(setv  T True  F False)

;; --------------------------------------------------------------
;; * The parent class
;; --------------------------------------------------------------

(defclass Scenery [Tile]
  "A mostly static part of the level, such as a wall or trap."

  (setv
    blocks-move F
      ; Block player and monster movement.
    blocks-diag F
      ; Block diagonal movement between orthogonally adjacent squares.
    blocks-monster F
      ; Block monster movement, even if `blocks-move` is false.
    passwallable F
    wand-destructible F
    protects-vs-poison-air F
    emits-poison-air F)

  (defmeth hook-remote-action []
    "Called when the player tries to use a wand of remote action on
    this tile. Return true to end her turn (which you should do if and
    only if there was an effect on the game state)."
    (@hook-player-bump None))

  (defmeth info-bullets [#* extra]
    (setv blocks-monster (or @blocks-monster @blocks-move))
    (.info-bullets (super)
      (when @blocks-move
        "Blocks all movement")
      (when (and
          (not @blocks-move)
          (or @blocks-monster G.rules.dainty-monsters))
        "Blocks monster movement")
      (when @blocks-diag
        "Blocks diagonal movement around itself")
      (cond
        (and @blocks-player-shots @blocks-monster-shots)
          "Blocks all shots"
        @blocks-player-shots
          "Blocks your shots, but not monsters' shots"
        @blocks-monster-shots
          "Blocks monsters' shots, but not your shots")
      (when @passwallable
        "Permeable with a passwall amulet")
      (when @wand-destructible
        "Destructible with a wall-destroying wand")
      (when @superblock
        "Not subject to magical transformation or passage")
      #* extra
      (@dod "Effect when bumped" 'hook-player-bump)
      (@dod "Effect when trying to enter" 'hook-player-walk-to)
      (@dod "Effect when stepped onto" 'hook-player-walked-into)
      (@dod "Effect when trying to exit" 'hook-player-walk-from)
      (@dod "Effect when stepped off of" 'hook-player-walked-from)
      (@dod "Effect when you shoot it" 'hook-player-shot)
      (when @protects-vs-poison-air
        #("Special effect" "If you end your turn within 1 square of this tile, you'll take no damage from ambient poison or poisonous fountains. Other sources of poison damage are unaffected."))
      (when @emits-poison-air
        #("Special effect" f"If you end your turn within 1 square of this tile, you'll take {G.rules.poison-emitter-damage} poison damage. This effect applies no more once per turn."))
      (@dod "Behavior" 'act))))

;; --------------------------------------------------------------
;; * Helpers
;; --------------------------------------------------------------

(defn walkability [p direction monster? [ethereal-to #()]]
  "Can an actor at `p` walk in `direction`, or at least bump something
  there (e.g., attacking a monster), considering geometry and scenery?
  `monster?` should be true for a monster and false for the player.
  `ethereal-to` can be a list or tuple of tile types (specified by
  stem) that the actor is specially allowed to ignore.

  Return a 2-tuple. The first element is the target position (`None`
  if it's out of bounds) and the second is a symbol:

  - 'out-of-bounds
  - 'blocked-diag
  - 'bump (you can bump something there, but not go there)
  - 'walk (you can walk there)"

  (setv target (+ p direction))
  (unless target
    (return #(None 'out-of-bounds)))
  #(target (cond
    ; First, check for diagonal blockers.
    (and
        direction.x direction.y
        (any (gfor
          p2 [
            (Pos target.map p.x target.y)
            (Pos target.map target.x p.y)]
          tile (at p2)
          (and
            (isinstance tile Scenery)
            tile.blocks-diag
            (not-in tile.stem ethereal-to)
            (not (and tile.passwallable (.player-has? StatusEffect.Pass)))))))
      'blocked-diag
    ; If there are no diagonal blockers, but the actor can't actually
    ; occupy the square, it can still bump things on the square.
    (not (can-occupy? target monster? ethereal-to))
      'bump
    ; Otherwise, the actor can walk there.
    True
      'walk)))

(defn can-occupy? [pos monster? ethereal-to]
  "Similar to `walkability`, but all that is considered is whether the
  player or monster could occupy `pos`, and the return value is just a
  `bool`."
  (all (gfor
    t (at pos)
    (or (in t.stem ethereal-to) (not (or
      (and monster? G.rules.dainty-monsters)
      (isinstance t hy.I.simalq/tile.Monster)
      (and (isinstance t Scenery) (or
        (and monster? t.blocks-monster)
        (and t.blocks-move (or monster?
          (not (and t.passwallable (.player-has? StatusEffect.Pass)))))))))))))

(defclass Wallish [Scenery]
  (setv
    blocks-move T blocks-diag T
    passwallable T
    wand-destructible T))

;; --------------------------------------------------------------
;; * Basic scenery
;; --------------------------------------------------------------

(deftile "██" "a wall" Wallish
  :iq-ix #(
    2    ; wall
    131) ; invisible wall
      ; Replacing invisible walls with walls is not entirely cosmetic:
      ; IQ's invisible walls are specifically immune to passwall
      ; amulets, albeit not wall-destroying wands.
  :flavor "Among the most numerous and persistent of the obstacles that stand in the way of your inevitable victory.\n\n    This man, with lime and rough-cast, doth present\n    Wall, that vile Wall which did these lovers sunder;\n    And through Wall's chink, poor souls, they are content\n    To whisper, at the which let no man wonder.")

(deftile "██" "the Void" Scenery
  :color color.void
  :iq-ix 17
  :blocks-move T :blocks-diag T
  :superblock T
  :flavor "Unshaped matter of the realm outside time and space. Mortal magic can't so much as make a dent in it.")

(deftile "| " "a pillar" Scenery
  :iq-ix #(
    12   ; pillar
    146) ; fire fountain
      ; Replacing fire fountains with pillars is not entirely
      ; cosmetic. For example, IQ's fire fountains aren't affected by
      ; passwall wands (i.e., wall-destroying wands).
  :blocks-move T
  :passwallable T
  :wand-destructible T
  :flavor "A structure of vaguely Roman style.")

(deftile "╷ " "a broken pillar" Scenery
  :iq-ix 82
  :blocks-move T :blocks-player-shots F
  :passwallable T
  :wand-destructible T
  :flavor "It's just a chest-high half of a pillar now. Good thing it wasn't load-bearing, huh? It makes for good cover against enemy shots.")

(deftile "◀▶" "a hole" Scenery
  :color 'dark-gray
  :iq-ix 107  ; crevasse
  :blocks-move T :blocks-player-shots F :blocks-monster-shots F
  :flavor "Watch your step.")

;; --------------------------------------------------------------
;; * Doors
;; --------------------------------------------------------------

(deftile "++" "a door" Scenery
  :color 'brown
  :iq-ix 5
  :blocks-monster T
  :flavor "It's unlocked, but it just won't stay open. Maybe that's for the best, since monsters are too dumb to operate it.")

;; --------------------------------------------------------------
;; ** Locked doors
;; --------------------------------------------------------------

(defclass LockedDoor [Scenery]
  (setv
    result-when-opened None
    blocks-monster T)

  (defmeth hook-player-bump [origin]
    (doc (+ "Consumes one key to "
      (if @result-when-opened
        f"replace the tile with {(. Tile types [@result-when-opened] name-with-article)}."
        "destroy the tile.")))

    (unless (.player-has? StatusEffect.MKey)
      (unless G.player.keys
        (raise (CommandError "It's locked, and you're keyless at the moment.")))
      (-= G.player.keys 1))
    (if @result-when-opened
      (@replace @result-when-opened)
      (@rm-from-map))
    True))

(deftile "++" "a locked door" LockedDoor
  :color 'navy
  :iq-ix 6
  :passwallable T
  :result-when-opened "door"
  :flavor "Fortunately, Tris knows how to pick locks. Unfortunately, she was wearing her hair down when she got whisked away to the dungeon, so she doesn't have any hairpins. You may have to use a key.")

(deftile "++" "a locked disappearing door" LockedDoor
  :color 'steel-blue
  :iq-ix 81
  :passwallable T
  :result-when-opened None
  :flavor "This advanced door destroys not only the key used to unlock it, but also itself. A true marvel of engineering.")

(deftile "##" "a closed portcullis" LockedDoor
  :color 'navy
  :iq-ix 103
  :blocks-player-shots F :blocks-monster-shots F
  :passwallable T
  :result-when-opened "open portcullis"
  :flavor "Finally, a kind of door that can be locked more than once. The keys are still really fragile, though.")

(deftile #[[""]] "an open portcullis" Scenery
  :color 'navy
  :iq-ix 102
  :blocks-player-shots F :blocks-monster-shots F

  :hook-player-walked-from (meth []
    "The portcullis closes."
    (@replace "closed portcullis"))

  :flavor "Open the gate! Close the gate! Open the gate a little!")

(deftile " +" "a treasure chest" LockedDoor
  ; This unusual mapsym, having a space on the left but not the right,
  ; allows one character of the contained item to be visible.
  :color 'steel-blue
  :result-when-opened None
  :flavor "This locked strongbox is too tough to smash apart, but its boards are so warped that you can peek (or even shoot) at what's inside before you decide to spend a key on it.\n\n    We'll dig up the box.\n    We know it's full of precious booty.\n    Burst open the locks,\n    And then we'll say \"Hooray!\"")
(setv (get Tile.types-by-iq-ix 16) (fn [pos te-v1 te-v2]
  ; We represent an IQ treasure chest as two tiles: the treasure chest
  ; on top of the item contained within.
  [
    ((get Tile.types "treasure chest") :pos pos)
    #* (if (= te-v1 21) ; An unknown potion
         ((get Tile.types-by-iq-ix te-v1) pos None te-v2)
         [((get Tile.types-by-iq-ix te-v1) :pos pos)])]))

;; --------------------------------------------------------------
;; ** Metal doors
;; --------------------------------------------------------------

(deftile "++" "a metal door" Scenery
  :iq-ix 167

  :blocks-move T

  :hook-player-bump (meth [origin]
    (when (.player-has? StatusEffect.MKey)
      (@rm-from-map)
      T))

  :flavor "This massive slab of steel will certainly not be opened with a sad little bargain-basement skeleton key. Your best bet is looking for a remote switch of some kind.")

(deftile "+|" "a metal-door control" Scenery
  :iq-ix 168
  :field-defaults (dict
    :target None)
  :fields #("target")

  :blocks-move T

  :read-tile-extras (classmethod (fn [cls mk-pos v1 v2]
    (dict :target (mk-pos #(v1 v2)))))
  :suffix-dict (meth []
    (dict :target @target))

  :hook-player-bump (meth [origin]
    (doc f"If there's a metal door at {@target}, it's destroyed. Otherwise, everything on the square is destroyed and a metal door is created there.")
    (for [t (list (at @target))  :if (= t.stem "metal door")]
      (.rm-from-map t)
      (break)
      (else
        (annihilate @target)
        (Tile.make @target "metal door")))
    T)

  :flavor "A switch (too awkwardly designed to be flipped with an arrow from Tris's bow) that can open a metal door somewhere in the dungeon. It can also close the door, in case you want to bring half a ton of steel hurtling down on somebody's head.")

;; --------------------------------------------------------------
;; ** One-way doors
;; --------------------------------------------------------------

(defclass OneWayDoor [Scenery]

  (setv
    blocks-monster T
    passwallable T
    direction None
    color #('brown 'red))

  (defmeth hook-player-bump [origin]
    (when (.player-has? StatusEffect.MKey)
      (@replace "door")
      T))

  (defmeth hook-player-walk-from [target]
    (doc f"Only allows you to walk {@direction.name}.")
    (unless (= (+ @pos @direction) target)
      (raise (CommandError f"You can only go {@direction.name} from this one-way door."))))
  (defmeth hook-player-walk-to [origin]
    (doc f"Only allows you to enter from the
      {@direction.opposite.name}.")
    (unless (= (+ origin @direction) @pos)
      (raise (CommandError (.format "That one-way door must be entered from the {}."
        @direction.opposite.name)))))

  (setv flavor "My way or the highway!"))

(do-mac
  (import simalq.geometry [Direction])
  `(do ~@(lfor
    [direction iq-ix] [
      [Direction.N 8] [Direction.E 11]
      [Direction.S 9] [Direction.W 10]]
    :setv c (get Direction.arrows direction)
    `(deftile ~f"+{c}" ~f"a one-way door ({direction.name})" OneWayDoor
      :iq-ix ~iq-ix
      :direction (. Direction ~(hy.models.Symbol direction.abbr))))))

;; --------------------------------------------------------------
;; * Exits
;; --------------------------------------------------------------

(deftile :name "an exit" :superc Scenery
  :field-defaults (dict
    :level-n None)
  :color-bg 'lime
  :iq-ix [
    7   ; normal exit
    94] ; special exit

  :mapsym (property-meth []
    (setv n (- (@effective-level-n) G.level-n))
    (cond
      (< n -1) "<<"
      (= n -1) "< "
      (= n 0)  "><"
      (= n 1)  "> "
      True     ">>"))

  :suffix-dict (meth []
    {
      (.format "{}to" (if (<= (@effective-level-n) G.level-n)
        "back "
        ""))
      (if (> (@effective-level-n) (len G.quest.levels))
        "victory"
        f"level {(@effective-level-n)}")})

  :blocks-monster T
  :blocks-player-shots F :blocks-monster-shots F

  :read-tile-extras (classmethod (fn [cls mk-pos v1 v2]
    (dict :level-n v2)))

  :!effective-level-n (meth []
    (if (is @level-n None) G.level.next-level @level-n))

  :hook-player-walked-into (meth []
    (doc f"Takes you to dungeon level {(@effective-level-n)}. If you're already on that level, it's refreshed. If there is no such level, you win the quest.")

    (hy.I.simalq/quest.start-level (@effective-level-n))
    (setv G.player.just-exited T)
    True)

  :flavor "Get me outta here.")

;; --------------------------------------------------------------
;; * Disappearing walls
;; --------------------------------------------------------------

(deftile "##" "a cracked wall" [Wallish Damageable]
  :field-defaults (dict
    :hp 2)
  :iq-ix-mapper ["hp"
    {3 4  4 2  15 6}]

  :blocks-player-shots F
  :immune #(DamageType.Poison DamageType.Fire DamageType.DeathMagic)

  :hook-remote-action (meth []
    None)

  :flavor "I think this dungeon might not be up to code.")


(defclass BreakableWall [Wallish]

  (setv
    color 'white
    color-bg 'black)

  (setv directions None)

  (defmeth hook-player-bump [origin]
    (doc f"Destroys the wall, along with all other walls of the same
    type that are either on the same square, adjacent to the {(. self
    directions [0] name)}, or adjacent to the {(. self directions [1]
    name)}. Those walls can themselves destroy further walls,
    propagating a chain reaction.")
    (@breakdown)
    True)
  (defmeth hook-remote-action []
    None)

  (defmeth hook-player-shot []
    "As when bumped."
    (@breakdown))

  (defmeth breakdown []
    (for [
        group [
          [@pos]
          #* (gfor  direction @directions  (ray @pos direction Inf))]
        p group]
      (if (setx to-remove (lfor  t (at p)  :if (is (type t) (type @))  t))
        (for [t to-remove]
          (.rm-from-map t))
        (break))))

  (setv flavor "This dungeon is coming down like a house of cards."))

(deftile "↑↓" "a breakable wall (meridional)" BreakableWall
  :iq-ix 160
  :directions #(Direction.N Direction.S))

(deftile "←→" "a breakable wall (zonal)" BreakableWall
  :iq-ix 161
  :directions #(Direction.W Direction.E))


(deftile "^█" "a fading wall" [Wallish Actor]
  :color #('white None)
  :color-bg #('black None)
  :iq-ix 188

  :!radius 3

  :act (meth []
    (doc f"Fade — If you're within {@radius} squares, the tile is destroyed.")
    (when (<= (dist @pos G.player.pos) @radius)
      (@rm-from-map)))

  :flavor "A polite and bashful sort of wall that will opt to disappear into thin air rather than stand in the way of Her Royal Highness Princess Triskaidecagonn XIII. If only more of the dungeon was so mindful of your station.")


(defclass PhasingWall [Scenery PosHooked]

  (setv map-attr "phasing_walls")
  (setv phase-replace None)

  (defmeth phase-shift []
    (@replace @phase-replace))

  (setv poshooked-callback phase-shift))

(deftile "☯█" "a phasing wall (in phase)" [PhasingWall Wallish]
  :color #('white None)
  :color-bg #('black None)
  :iq-ix 139

  :phase-replace "phasing wall (out of phase)"

  :flavor "An all but completely ordinary wall. It can be temporarily warped out of existence by various devices, such as a phase trigger.")

(deftile "☯ " "a phasing wall (out of phase)" PhasingWall
  :color 'light-gray
  :iq-ix 140

  :blocks-player-shots F
  ; Per IQ, a phasing wall is unaffected by a wall-destroying wand while
  ; out of phase.

  :phase-replace "phasing wall (in phase)"

  :flavor "A bit of gray fog betokening the ghost of a wall that was, and may be again. You can move and shoot through it freely. Monsters, on the other hand, can be a bit superstitious about it.")

(deftile "☯|" "a phase trigger" Scenery
  :iq-ix 142

  :blocks-move T

  :hook-player-bump (meth [origin]
    (doc f"Phase-shifts all phasing walls on the level.")
    (PhasingWall.run-all)
    True)
  :hook-player-shot (meth []
    "As when bumped."
    (PhasingWall.run-all))

  :flavor "An immobile switch that toggles phasing walls. Tris's expertise in target shooting allows her to trigger one of these with a single arrow.")

;; --------------------------------------------------------------
;; * Pushblocks
;; --------------------------------------------------------------

(deftile :name "a pushblock" :superc Scenery
  :field-defaults (dict
    :n-pushes None)
  :mutable-fields #("n_pushes")
  :iq-ix-mapper ["n_pushes"
    ; Called a "moveable wall" in IQ. I think "wall" is misleading
    ; because it's not a diagonal blocker.
    {22 None  144 1  145 2}]

  :mapsym (property-meth []
    (+ "■" (cond
      (is @n-pushes None) " "
      (< @n-pushes 10)    (str @n-pushes)
      True                "^")))
  :suffix-dict (meth []
    (if (is @n-pushes None) {} {"pushes left =" @n-pushes}))

  :blocks-monster T
  :wand-destructible T
  :hook-player-walk-to (meth [origin]
    (setv target (+ @pos (dir-to origin @pos)))
    (when (or (not target) (at target))
      (raise (CommandError "There's no room to push the block there."))))

  :hook-player-walked-into (meth []
    (doc (+
      "You push the block in the same direction that you entered the square. The destination square must be empty, or else you won't be able to step on the original square."
      (cond
        (is @n-pushes None)
          ""
        (= @n-pushes 1)
          " The block then transforms into a normal wall."
        True
          f" After a total of {@n-pushes} pushes, the block will transform into a normal wall.")))
    (@move (+ @pos G.action.direction))
    (unless (is @n-pushes None)
      (-= @n-pushes 1)
      (unless @n-pushes
        (@replace "wall")))
    F)

  :flavor "Where do video games get all their crates from? There must be entire warehouses full of 'em, am I right?")

;; --------------------------------------------------------------
;; * Wall generators
;; --------------------------------------------------------------

(defclass WallGenerator [Scenery]
  (setv
    blocks-move T
    ; Surprisingly, but per IQ, wall generators aren't diagonal
    ; blockers.
    direction None)

  (defmeth hook-player-bump [origin]
    (doc f"Creates a line of walls to the {@direction.name}, stopping at the first nonempty square. The wall generator itself then reverts to a normal wall.")
    (@generate)
    True)
  (defmeth hook-player-shot []
    "As when bumped."
    (@generate))

  (defmeth generate []
    (for [p (ray @pos @direction Inf)]
      (when (at p)
        (break))
      (Tile.make p "wall"))
    (@replace "wall"))

  (setv flavor "Ever wondered where all those walls come from?"))

(do-mac
  (import simalq.geometry [Direction])
  `(do ~@(lfor
    [direction iq-ix] [
      [Direction.N 190] [Direction.E 193]
      [Direction.S 191] [Direction.W 192]]
    :setv c (get Direction.arrows direction)
    `(deftile ~f"{c}|" ~f"a wall generator ({direction.name})" WallGenerator
      :iq-ix ~iq-ix
      :direction (. Direction ~(hy.models.Symbol direction.abbr))))))

;; --------------------------------------------------------------
;; * Fountains
;; --------------------------------------------------------------

(deftile "{ " "a water fountain" Scenery
  :color 'steel-blue
  :iq-ix 108

  :blocks-move T
  :protects-vs-poison-air T

  :flavor "An ornate decorative fountain featuring a statue of a particularly homely goblin. The fountain's spray fills the air about it with a refreshing mist.")

(deftile "{ " "a poisonous fountain" Scenery
  :color 'dark-green
  :iq-ix 137

  :blocks-move T
  :emits-poison-air T

  :flavor "An ornate decorative fountain featuring a statue of Death himself. The fountain's spray fills the air about it with a suffocating miasma.")

;; --------------------------------------------------------------
;; * Teleportation
;; --------------------------------------------------------------

;; --------------------------------------------------------------
;; ** Gates
;; --------------------------------------------------------------

(defclass Gate [Scenery]
  (setv field-defaults (dict
    :target None))
  (setv fields #("target"))

  (setv blocks-monster T)
  (setv one-shot? F)

  (defn [classmethod] read-tile-extras [cls mk-pos v1 v2]
    (dict :target (mk-pos #(v1 v2))))

  (defmeth suffix-dict []
    (dict :dest @target))
  (defmeth hook-player-walked-into []
    (doc (+
      f"Teleports you to {@target}. Anything already there is unaffected."
      (if @one-shot? " The gate is then destroyed." "")))
    (.move G.player @target)
    (when @one-shot?
      (@rm-from-map))
    T)

  (setv flavor "A small stone arch containing a rippling, sparkling sheet of colored light. It functions as a magic portal that can send you elsewhere on this level. Sadly, arrows in flight won't survive the trip."))

(deftile "{}" "a gate" Gate
  :color 'purple
  :iq-ix 24)

(deftile "{}" "a one-shot gate" Gate
  :color 'red
  :iq-ix 162
  :one-shot? T)

;; --------------------------------------------------------------
;; ** Teleporters
;; --------------------------------------------------------------

(deftile "┣┫" "a teleporter" Scenery
  :color 'purple
  :field-defaults (dict
    :times-used 0
    :output-dir None)
  :mutable-fields #("times_used" "output_dir")
  :iq-ix 23
  :blocks-diag T :blocks-monster T

  :hook-player-walked-into (meth []
    "Teleports you to a free square adjacent to the nearest other teleporter in the reality bubble. If there is no eligible destination teleporter, no teleportation occurs; if there are several tied for the nearest, you'll cycle through them when you re-enter this teleporter. Squares are considered to be free even if they contain monsters, which are slain instantly if you teleport into them. The free square that you appear on is cycled each time you re-exit a teleporter."

    (import simalq.tile [Monster])

    ; Find a set of teleporters we can go to and their free adjacent
    ; positions.
    (setv candidate-dist Inf)
    (setv candidates [])
    (for [p (burst G.player.pos G.rules.reality-bubble-size :exclude-center T)]
      (when (> (dist p @pos) candidate-dist)
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
            (setv candidate-dist (dist p @pos)))
          (break))))

    ; Choose the other teleporter to use.
    (unless candidates
      (return F))
    (setv [other-porter neighbors] (get
      candidates
      (% @times-used (len candidates))))
    (+= @times-used 1)
    ; Choose the specific target square.
    (while True
      (setv other-porter.output-dir
        (next-in-cycle Direction.all other-porter.output-dir))
      (when (in
          (setx target (+ other-porter.pos other-porter.output-dir))
          neighbors)
        (break)))

    ; At the target, tele-frag all tiles, which we already know must
    ; be monsters.
    (annihilate target)

    ; Now actually move the player.
    (.move G.player target)

    T)

  :info-bullets (meth []
    (.info-bullets (super)
      #("Times used" @times-used)
      #("Output direction" @output-dir)))

  :flavor "A bulky cubic device representing an early attempt at teleportation technology. Its operation is a bit convoluted. The fun part is, you can tele-frag with it.")

(deftile "┣┫" "a controllable teleporter" Scenery
  :color 'dark-green
  :iq-ix 133
  :blocks-diag T :blocks-monster T

  :hook-player-walked-into (meth []
    "Teleports you to an empty square of your choice within the reality bubble. You may choose to cancel this and not teleport."
    (import simalq.commands [UseControllableTeleporter targeting-mode])
    (defn ok [p]
      (and
        (not (at p))
        (<= (dist @pos p) G.rules.reality-bubble-size)))
    (unless (is (type G.action) UseControllableTeleporter)
      (setv G.action (UseControllableTeleporter G.action.direction #* (.
        (or
          (targeting-mode (fn [target]
            (or (= target G.player.pos) (ok target))))
          G.player.pos)
        xy))))
    (setv target (Pos G.map G.action.target-x G.action.target-y))
    (when (ok target)
      (.move G.player target)
      T))

  :flavor "Go anywhere your heart desires! Restrictions apply.")

;; --------------------------------------------------------------
;; * Traps
;; --------------------------------------------------------------

(defclass Trap [Scenery]
  (setv
    blocks-move F
    blocks-player-shots F
    blocks-monster-shots F))

;; --------------------------------------------------------------
;; ** Wallfall traps
;; --------------------------------------------------------------

(deftile :name "a wallfall trap"  :superc Trap
  :color 'dark-yellow
  :field-defaults (dict
    :wallnum 1)
  :iq-ix-mapper ["wallnum" (do-mac
    ; These are just called "traps" in IQ.
    (setv x [115 13 75 76 77 111 112 113 114])
    (dict (zip x (range (len x)))))]

  :mapsym (property-meth []
    (+ "<" (if (< @wallnum 10) (str @wallnum) "^")))
  :suffix-dict (meth []
    (dict :type @wallnum))
  :hook-player-walked-into (meth []
    (doc (if (= @wallnum 0)
      "Destroys all trapped walls and other wallfall traps on the level, regardless of type."
      f"Destroys all trapped walls on the level of type {@wallnum} or 0, along with all other wallfall traps of type {@wallnum}."))
    (for [col G.map.data  stack col  tile (list stack)]
      (when (or
          (and (= tile.stem "trapped wall")
            (or (= @wallnum 0) (in tile.wallnum [0 @wallnum])))
          (and (= tile.stem "wallfall trap")
            (or (= @wallnum 0) (= tile.wallnum @wallnum))))
        (.rm-from-map tile))))

  :flavor #[[Easy there, Admiral Ackbar. This kind of trap isn't necessarily dangerous. Well, admittedly, the key word here is "necessarily".]])

(deftile :name "a trapped wall" :superc Wallish
  :color #('black 'pale-yellow)
  :color-bg #(None 'black)
  :field-defaults (dict
    :wallnum 1)
  :iq-ix-mapper ["wallnum" (do-mac
    (setv x [120 14 78 79 80 116 117 118 119])
    (dict (zip x (range (len x)))))]

  :mapsym (property-meth []
    (+ "█" (if (< @wallnum 10) (str @wallnum) "^")))
  :suffix-dict (meth []
    (dict :type @wallnum))
  :info-bullets (meth []
    (.info-bullets (super)
      #("Wallfall type" @wallnum)))

  :flavor "The special thing about this wall is that it can be destroyed by wallfall traps.\n\nWhat's the deal with monster closets? Monsters are proud of who they are, am I right? I'll be here all week.")

;; --------------------------------------------------------------
;; ** Damaging traps
;; --------------------------------------------------------------

(defclass DamagingTrap [Trap]

  (setv trap-damage 5)
  (setv one-shot? F)

  (defmeth hook-player-walked-into []
    (doc (+ f"Does {@trap-damage} damage." (if @one-shot?
      " Then, the tile is destroyed."
      "")))
    (.damage G.player @trap-damage DamageType.Trap)
    (when @one-shot?
      (@rm-from-map))))

(deftile "<>" "a fixed damaging trap" DamagingTrap
  :color 'red
  :iq-ix 35  ; damage-causing trap

  :flavor "A dense assortment of pointy, painful objects that can penetrate the toughest footwear.")

(deftile "<>" "a one-shot damaging trap" DamagingTrap
  :color 'dark-orange
  :iq-ix 199  ; random-damage trap

  :one-shot? T
  :flavor "A tiny hole in the floor that shoots a ball bearing in your eye with uncanny accuracy. Fortunately, it has only one shot.")

;; --------------------------------------------------------------
;; ** Status-effect traps
;; --------------------------------------------------------------

(deftile "<>" "a paralysis trap" Trap
  :color 'purple
  :iq-ix 36

  :hook-player-walked-into (meth []
    (doc f"Paralyzes you for {G.rules.paralysis-duration} turns. While you're paralyzed, waiting is the only action you can take.")
    (.add StatusEffect.Para G.rules.paralysis-duration))

  :flavor "A magical field that causes you to vividly remember something embarrassing that you did as a teenager, forcing you to briefly freeze in horror. The problem with being royalty is that awkward adolescent moments all too easily become international incidents.")

(deftile "##" "a web" Scenery
  :color 'dark-gray
  :iq-ix 136

  :blocks-move F :blocks-monster T
  :hook-player-walked-into (meth []
    (doc f"The tile is destroyed, but you're paralyzed for {G.rules.paralysis-duration} turns.")
    (@rm-from-map)
    (.add StatusEffect.Para G.rules.paralysis-duration))

  :flavor "This spiderweb is the size of a really big spiderweb. Until Idok cleans up the dungeon properly, you'll have to tediously carve your way through the webs with your sword. Got any recommendations for a good smitemaster?")

(deftile "<>" "a weakness trap" Trap
  :color 'magenta
  :iq-ix 201

  :!duration 12
  :hook-player-walked-into (meth []
    (doc f"Makes you weak for {@duration} more turns, reducing the damage of your sword strikes by {G.rules.player-melee-damage-weakness-reduction}.")
    (.add StatusEffect.Weak @duration))

  :flavor "This complex gadget swiftly adorns Tris with impractical hyperfeminine attire, including a frilly pink dress and stiletto heels. It will take more than that to extinguish Tris's indomitable tomboyish fighting spirit; still, the new duds make fighting a bit awkward. Fortunately, these are all disposable fast-fashion items made solely for the 'Gram #ootd, and will disintegrate in moments.")

(deftile "<>" "an anti-magic trap" Trap
  :color 'blue
  :iq-ix 169

  :hook-player-walked-into (meth []
    (doc (.format "Disenchants you, removing the first beneficial status effect that you have from the following list: {}."
      (.join ", " (gfor  e (StatusEffect.disenchantable)  e.name))))
    (StatusEffect.disenchant-player))

  :flavor "May I take your cloak?")

;; --------------------------------------------------------------
;; ** Poison plates
;; --------------------------------------------------------------

(defclass PoisonPlate [Trap]
  "A replacement for IQ's poisonous amulets and counterpoison amulets.
  I've made them into traps so that it makes more sense that their
  effect is level-limited rather than time-limited. The way the
  numbers work are also pretty different for better compatibility with
  arbitrary nonnegative poison intensities."

  (setv poison-multiplier None)
  (defmeth hook-player-walked-into []
    (doc f"Multiplies this level's ambient poison factor by {@poison-multiplier} and destroys the tile.")
    (*= G.level.poison-intensity @poison-multiplier)
    (@rm-from-map))

  (setv flavor "Finally, some controls for the poison vents. But it's not very precise, and it can only be triggered once."))

(deftile "<>" "a poison pressure plate" PoisonPlate
  :color 'dark-green
  :iq-ix 150
  :poison-multiplier (f/ 2))

(deftile "<>" "a poison-protecting pressure plate" PoisonPlate
  :color 'navy
  :iq-ix 149
  :poison-multiplier (f/ 1 2))

;; --------------------------------------------------------------
;; ** Other traps
;; --------------------------------------------------------------

(deftile "<>" "an arrow trap" Trap
  :color 'dark-gray
  :iq-ix 194

  :blocks-player-shots T

  :flavor #[[You'd think that something called an "arrow trap" would shoot arrows at you. But no, this thing traps arrows. A ghostly hand pops out of the occult sigil on the floor to grab your arrow midflight with unerring accuracy. Then the hand tears the arrow into bits, just so you can't recover it. How petty.]])

(deftile "<>" "a phase trap" Trap
  :color 'brown
  :iq-ix 141

  :hook-player-walked-into (meth []
    (doc f"Phase-shifts all phasing walls on the level. Then, the tile is destroyed.")
    (PhasingWall.run-all)
    (@rm-from-map))

  :flavor "A pressure plate faintly inscribed with a ying-yang symbol.")

(deftile ", " "a broken trap" Trap
  :iq-ix #(
    ; Removed trap-like tiles that stick around after stepping on them.
     93   ; false exit
    189)  ; confusion trap

  :flavor "This once-vicious trap has decayed from neglect and is now pointlessly collecting dust. It was probably implemented in Flash or something.")

(deftile ", " "a pile of debris" Trap
  :color 'navy
  :iq-ix #(
    ; Removed trap-like tiles that are destroyed when you step on them.
    122   ; dimness trap
    148)  ; darkness trap

  :hook-player-walked-into (meth []
    "The tile is destroyed."
    (@rm-from-map))

  :flavor "Dungeon trash like sawdust, loose stones, pebbles, greasy chicken bones left over from goblin feasts, broken wands, and maybe a dead body, all bunched together into a small mound. Running through it will knock it over and get your boots really gross.")

;; --------------------------------------------------------------
;; * Plasma
;; --------------------------------------------------------------

(defclass MagicalBarrier [Scenery]
  ; Without barrier generators having (yet?) been implemented, these
  ; have essentially no interesting properties.

  (setv
    color 'dark-green
    blocks-move T)

  (setv directions None)

  ; IQ's effect of damaging the player when he walks into it isn't
  ; implemented, because so far as I can tell, there's never a reason
  ; to walk into a barrier—you can't even construct a puzzle to
  ; require it.

  (setv flavor "It's some kind of forcefield."))

(deftile "||" "a magical barrier (meridional)" MagicalBarrier
  :iq-ix 180
  :directions #(Direction.N Direction.S))

(deftile "==" "a magical barrier (zonal)" MagicalBarrier
  :iq-ix 179
  :directions #(Direction.W Direction.E))


(deftile "()" "a magical energy shield" [Scenery EachTurner]
  :color 'dark-orange
  :field-defaults (dict
    :time-remaining 12)
  :mutable-fields #("time_remaining")
  :iq-ix None
    ; In IQ, tiles of this type can only be created mid-game.

  :blocks-move F :blocks-monster T
  :blocks-player-shots F :blocks-monster-shots T

  :poshooked-callback (meth []
    (-= @time-remaining 1)
    (unless @time-remaining
      (@rm-from-map)))

  :info-bullets (meth []
    (.info-bullets (super)
      #("Turns remaining" @time-remaining)))

  :flavor "These glittering barriers of orange plasma offer you plenty of protection and monsters none at all. Enjoy 'em while they last.")
