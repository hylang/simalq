(require
  hyrule [unless do-n]
  simalq.macros [defdataclass slot-defaults pop-integer-part])
(import
  re
  fractions [Fraction :as f/]
  enum [Enum]
  simalq.util [player-melee-damage DamageType hurt-player next-in-cycle]
  simalq.geometry [Direction GeometryError pos+ at adjacent? dir-to]
  simalq.game-state [G]
  simalq.tile [Actor deftile mv-tile add-tile damage-tile destroy-tile]
  simalq.tile.scenery [walkability])
(setv  T True  F False)


(setv undead-immunities #(DamageType.Poison DamageType.DeathMagic))

(setv AI (Enum "AI" ["Approach"]))

(defclass Monster [Actor]
  "A non-player character, typically out to kill the player."

  (slot-defaults
    hp 1
        ; The monster's number of hit points (HP). When a monster's
        ; HP hits 0, it dies.
    movement-state None)
        ; A bit of memory or plan that the monster uses to choose
        ; its movements. Its meaning depends on `ai`.
  (setv
    mutable-slots #("hp" "movement_state")
    damageable T
    ai AI.Approach
      ; The monster's basic artificial intelligence for deciding what
      ; to do on its turn.
    damage-melee None
      ; How much damage the monster does with its basic melee attack.
      ; This can be `None`, one number, or a tuple of numbers, with
      ; element 0 giving damage when the monster is at 1 HP, element 1
      ; at 2 HP, etc. (the last element being implicitly repeated as
      ; required).
    damage-shot None
      ; Likewise for shots.
    kamikaze False)
      ; If true, the monster kills itself upon attacking.

  (defn hook-player-bump [self origin]
    "Attack the monster in melee."
    (damage-tile self (player-melee-damage) DamageType.PlayerMelee)
    True)

  (defn act [self]
    "Approach — If the monster is adjacent to you, it makes a melee attack. Otherwise, if it can shoot you, it does. Otherwise, it tries to get closer to you in a straight line. If its path to you is blocked, it will try to adjust its direction according to its movement state. If it can't move that way, it wastes its turn, and its movement state advances to the next cardinal direction."
    (when (!= self.ai AI.Approach)
      (raise (ValueError "Other AIs are not yet implemented.")))

    ; Try to get closer to the player.
    (setv d (dir-to self.pos G.player.pos))
    (when (is d None)
      ; The player is in our square. Just give up.
      (return))
    (setv [target wly] (walkability self.pos d :monster? T))

    (when (and (= target G.player.pos) (in wly ['bump 'walk]))
      ; We're in melee range of the player, so bonk her.
        (hurt-player
          (damage-by-hp self self.damage-melee)
          DamageType.MonsterMelee)
        (when self.kamikaze
          (destroy-tile self))
        ; That uses up our action.
        (return))

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
    (mv-tile self target))

  (defn info-bullets [self #* extra]
    (defn damage-array [damage]
      (if (isinstance damage tuple)
        (.join " / " (gfor
          d damage
          (if (= d (damage-by-hp self damage))
            f"[{d}]"
            (str d))))
        damage))

    [
      #("Hit points" self.hp)
      (if self.immune
        #("Immune to" self.immune)
        "No immunities")
      (if self.damage-melee
        #("Melee damage" (damage-array self.damage-melee))
        "No melee attack")
      (if self.damage-shot
        #("Shot damage" (damage-array self.damage-shot))
        "No ranged attack")
      (when self.kamikaze
        #("Kamikaze" "When the monster attacks, it dies. You get no points for this."))
      #* extra
      #("Point value" (.format "{}{}"
        self.points
        (if self.score-for-damaging " (scored per HP lost)" "")))
      #("Behavior" self.act.__doc__)
      #("Movement state" self.movement-state)]))

(defn damage-by-hp [monster damage]
  (if (isinstance damage tuple)
    (get damage (- (min monster.hp (len damage)) 1))
    damage))

(defn summon [pos stem #** kwargs]
  (add-tile pos stem #** kwargs)
  ; Newly created monsters don't get to act on the turn they come
  ; into being.
  (setv (. (at pos) [-1] last-acted) G.turn-n))


(defclass Generated [Monster]
  "A monster that can be produced by a generator."

  (setv
    __slots__ []
    score-for-damaging T))

(defclass Generator [Monster]
  "An immobile structure that creates monsters nearby."

  (slot-defaults
    generate-frequency (f/ 1 4)
      ; How often a monster is generated.
    generate-hp 1
      ; How many hit points each monster will be generated with.
    generation-power (f/ 0))
      ; A per-turn accumulator of `generate-frequency`.
  (setv
    mutable-slots ["generation_power"]
    score-for-damaging T
    immune #(DamageType.Poison)
    generate-class None)
      ; The stem of the monster type to generate.

  (defn [classmethod] read-tile-extras [cls _ v2]
    (dict
      :generate-hp (>> v2 5)
      :generate-frequency (get
        #(1 (f/ 1 2) (f/ 1 3) (f/ 1 4) (f/ 1 5) (f/ 1 6) (f/ 2 5) (f/ 1 10) (f/ 3 5) (+ 1 (f/ 1 3)) (+ 1 (f/ 1 2)) (+ 1 (f/ 2 3)) 2 (f/ 2 3) (f/ 3 4) (f/ 4 5) (f/ 5 6) (f/ 9 10))
          ; These come from IQ's `SetGenFreq`.
        (- (& v2 0b1111) 1))))

  (defn info-bullets [self #* extra]
    (.info-bullets (super)
      #("Generation frequency" self.generate-frequency)
      #("Generation power" self.generation-power)
      #("Hit points of generated monsters" self.generate-hp)
      #* extra))

  (defn act [self]
    "Generate — The generator adds its generation frequency to its generation power. If the total is more than 1, the integer part is removed and a corresponding number of monsters are generated in adjacent empty squares. If there are no adjacent empty squares, the expended generation power is wasted. The square that the generator attempts to target rotates through the compass with each generation or failed attempt."

    (+= self.generation-power self.generate-frequency)
    (do-n (pop-integer-part self.generation-power)
      ; Find an empty square to place the new monster.
      (do-n (len Direction.all)
        (setv self.movement-state
          (next-in-cycle Direction.all self.movement-state))
        (setv target (try
          (pos+ self.pos self.movement-state)
          (except [GeometryError]
            (continue))))
        (when (= (at target) [])
          (break))
        (else
          ; We couldn't find anywhere to place this monster. Just
          ; end generation, wasting the consumed generation power.
          (return)))
      ; We have a target. Place the monster.
      (summon target self.generate-class
        :hp self.generate-hp))))

(defn defgenerated [
    mapsym name *
    iq-ix-mon iq-ix-gen
    points-mon points-gen
    flavor-mon flavor-gen
    [immune #()]
    #** kwargs]
  "Shorthand for defining both a generated monster and its generator."

  (deftile Generated mapsym name
    :iq-ix-mapper ["hp"
      (dict (zip iq-ix-mon [1 2 3]))]
    :immune immune
    :points points-mon
    :flavor flavor-mon
    #** kwargs)
  (deftile Generator (+ "☉" (get mapsym 0)) (+ name " generator")
    :iq-ix-mapper ["hp"
      (dict (zip iq-ix-gen [1 2 3]))]
    :generate-class (re.sub r"\S+\s+" "" name)
    :immune (+ Generator.immune immune)
    :points points-gen
    :flavor flavor-gen))


(defclass NonGen [Monster]
  "A monster that isn't produced by a generator."

  (setv __slots__ [])

  (defn [classmethod] read-tile-extras [cls _ v2]
      (dict :hp v2)))


(defgenerated "o " "an orc"
  :iq-ix-mon [39 59 60] :iq-ix-gen [40 61 62]
  :points-mon 3 :points-gen 12

  :damage-melee #(3 6 9)

  :flavor-mon "A green-skinned, muscle-bound, porcine humanoid with a pointy spear and a bad attitude."
  :flavor-gen "A sort of orcish clown car, facetiously called a village.")

(defgenerated "g " "a goblin"
  :iq-ix-mon [95 96 97] :iq-ix-gen [98 99 100]
  :points-mon 2 :points-gen 8

  :damage-melee #(2 4 6)

  :flavor-mon "Goblins are a smaller, uglier, smellier, and worse-equipped cousin of orcs that try to make up for it with even more sadistic malice. It almost works."
  :flavor-gen "Oops, somebody gave the goblins a bath. Now there's a lot more of them, and they still stink.")

(defgenerated "G " "a ghost"
  :iq-ix-mon [37 55 56] :iq-ix-gen [38 57 58]
  :points-mon 5 :points-gen 25

  :immune undead-immunities
  :damage-melee #(5 10 15)
  :kamikaze T

  :flavor-mon "A spooky apparition bearing a striking resemblance to a man with a sheet draped over him. Giggle at your peril: it can discharge the negative energy that animates it to bring you closer to the grave yourself.\n\n    Lemme tell ya something: bustin' makes me feel good!"
  :flavor-gen "This big heap of human bones raises several questions, but sadly it appears you must treat the dead with even less respect in order to get rid of those ghosts.")


(deftile NonGen "K " "a Dark Knight"
  :iq-ix 53
  :points 75

  :damage-melee 12

  :flavor "This dread warrior wears ink-black armor and carries a heavy chain mace. His devotion to the powers of evil (not to mention his willingness, nay, eagerness to kill you) makes his appropriation of Batman's epithet questionable at best. When you get down to it, he's just trying to distract you from the fact that he's the most basic enemy in the whole dungeon.")
