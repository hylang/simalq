(require
  hyrule [unless do-n]
  simalq.macros [has defdataclass slot-defaults pop-integer-part fn-dd])
(import
  re
  fractions [Fraction :as f/]
  enum [Enum]
  toolz [unique]
  simalq.util [player-melee-damage DamageType hurt-player next-in-cycle mixed-number player-status]
  simalq.geometry [Direction pos+ at dist adjacent? dir-to pos-seed ray]
  simalq.game-state [G]
  simalq.tile [Tile Actor deftile mv-tile add-tile damage-tile destroy-tile]
  simalq.tile.scenery [Scenery walkability nogo?])
(setv  T True  F False)


((fn [] (for [dt DamageType]
  (setv (get (globals) dt.name) dt))))

(setv undead-immunities #(Poison DeathMagic))


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
    damage-melee None
      ; How much damage the monster does with its basic melee attack.
      ; This can be `None`, one number, or a tuple of numbers, with
      ; element 0 giving damage when the monster is at 1 HP, element 1
      ; at 2 HP, etc. (the last element being implicitly repeated as
      ; required).
    damage-shot None
      ; Likewise for shots.
    shot-range None
      ; If set to an integer, it limits the distance at which the
      ; monster can shoot.
    kamikaze F
      ; If true, the monster kills itself upon attacking.
    sees-invisible F
      ; If true, the monster is unaffected by the player being invisible.
    summon-frequency (f/ 1 4))
      ; How often the monster summons other monsters (if it can).

  (defn hook-player-bump [self origin]
    "Attack the monster in melee."
    (damage-tile self (player-melee-damage) PlayerMelee)
    True)

  (setv act 'approach)

  (defn suffix-dict [self]
    (dict :HP self.hp))

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
        #("Immune to" (.join ", " (gfor  x self.immune  x.value)))
        "No immunities")
      (when self.resists
        #("Takes no more than 1 damage from" (.join ", " (gfor  x self.resists  x.value))))
      (when self.weaknesses
        #("Instantly destroyed by" (.join ", " (gfor  x self.weaknesses  x.value))))
      (if self.damage-melee
        #("Melee damage" (damage-array self.damage-melee))
        "No melee attack")
      (if self.damage-shot
        #("Shot damage" (damage-array self.damage-shot))
        "No ranged attack")
      (when self.shot-range
        #("Shot range" self.shot-range))
      (when self.kamikaze
        #("Kamikaze" "When the monster attacks, it dies. You get no points for this."))
      (when self.sees-invisible
        #("Invisibility detection" "The monster is unaffected by you being invisible."))
      (unless (= self.hook-destroyed.__doc__ Tile.hook-destroyed.__doc__)
        #("Effect on death" self.hook-destroyed.__doc__))
      #* extra
      #("Point value" (.format "{:,}{}"
        self.points
        (if self.score-for-damaging " (scored per HP lost)" "")))
      #("Behavior" (or
        self.act.__doc__
        (self.act.dynadoc self)))
      #("Movement state" self.movement-state)]))

(defn damage-by-hp [monster damage]
  (if (isinstance damage tuple)
    (get damage (- (min monster.hp (len damage)) 1))
    damage))

(defn make-monster [pos stem #** kwargs]
  (add-tile pos stem #** kwargs)
  ; Newly created monsters don't get to act on the turn they come
  ; into being.
  (setv (. (at pos) [-1] last-acted) G.turn-n))

(defn summon [mon stem hp [direction-slot "summon_direction"]]
  "Increment summon power. Then, try to generate one or more monsters
  in adjacent spaces. Return true if power was expended."

  (+= mon.summon-power mon.summon-frequency)
  (when (< mon.summon-power 1)
    (return F))
  (do-n (pop-integer-part mon.summon-power)
    ; Find an empty square to place the new monster.
    (do-n (len Direction.all)
      (setattr mon direction-slot
        (next-in-cycle Direction.all (getattr mon direction-slot)))
      (setv target (pos+ mon.pos (getattr mon direction-slot)))
      (unless target
        (continue))
      (when (= (at target) [])
        (break))
      (else
        ; We couldn't find anywhere to place this monster. Just
        ; end summoning, wasting the consumed summon power.
        (return)))
    ; We have a target. Place the monster.
    (make-monster target stem :hp hp))
  T)

(defn player-invisible-to [mon [mon-pos None]]
  (and
    (player-status 'Ivis)
    (not (adjacent? (or mon-pos mon.pos) G.player.pos))
    (not mon.sees-invisible)))

(defn try-to-attack-player [mon [dry-run F] [shots-ignore-obstacles F] [from-pos None]]
  "Try to melee or shoot the player, if the monster can. Return true
  if it succeeded. If `dry-run` is true, the attack isn't actually
  made."

  (setv p (or from-pos mon.pos))
  (setv d (dir-to p G.player.pos))
  (setv attack None)

  (when (= p G.player.pos)
    ; If we're on the player's square, we can't attack her.
    (return F))

  (when (player-invisible-to mon p)
    (return F))

  ; Try a melee attack first.
  (when (and
      mon.damage-melee
      (adjacent? p G.player.pos)
      (in (get (walkability p d :monster? T) 1)
        ['bump 'walk]))
    (setv attack 'melee))

  ; Otherwise, try a ranged attack.
  (when (and (not attack) mon.damage-shot)
    (for [target (ray :pos p :direction d :length
        (min (or mon.shot-range Inf) G.rules.reality-bubble-size))]
      (when (= target G.player.pos)
        (setv attack 'shot)
        (break))
      (when (any (gfor
          tile (at target)
          (or tile.superblock (and
            tile.blocks-monster-shots
            (not shots-ignore-obstacles)))))
        (break))))

  ; If we can't attack, bail out.
  (unless attack
    (return F))

  ; Execute the attack.
  (when dry-run
    (return T))
  (hurt-player :attacker mon
    (damage-by-hp mon (if (= attack 'shot) mon.damage-shot mon.damage-melee))
    (if (= attack 'shot) MonsterShot MonsterMelee))
  (when (and mon.kamikaze mon.pos)
    ; We check `mon.pos` so as not to call `destroy-tile` when we're
    ; already being called by it.
    (destroy-tile mon))
  T)

(defn stationary [mon]
  "Stationary — The monster attacks if it can, but is otherwise immobile."
  (try-to-attack-player mon))

(defn approach [mon
    [implicit-attack T]
    [advance-movement-state T]
    [reverse F]
    [jump F]]
  "Approach — If the monster can attack, it does. Otherwise, it tries to get closer to you in a straight line. If its path to you is blocked, it will try to adjust its direction according to its movement state. If it can't move that way, it wastes its turn, and its movement state advances to the next cardinal direction."
  ; Return true if we successfully moved or attacked; false otherwise.

  (when (and implicit-attack (try-to-attack-player mon))
    (return T))
  (when (player-invisible-to mon)
    (return F))

  ; Try to get closer to the player.
  (setv d (dir-to mon.pos G.player.pos))
  (when (is d None)
    ; The player is in our square. Just give up.
    (return F))
  (when reverse
    (setv d d.opposite))

  (setv target None)
  (defn ok-target []
    (nonlocal target)
    (if jump
      ; In jump mode, we move two squares, mostly ignoring tiles on
      ; the intermediate square, and ignoring all diagonal blocking.
      (and
        (setx intermediate (pos+ mon.pos d))
        (not (has intermediate Scenery it.superblock))
        (setx target (pos+ intermediate d))
        (not (nogo? target :monster? T)))
      (do
        (setv [target wly] (walkability mon.pos d :monster? T))
        (= wly 'walk))))

  (unless (ok-target)
    ; We can't go that way. Try a different direction.
    ; Use a non-random equivalent of IQ's `ApproachHero`.
    (setv movement-state
      (next-in-cycle Direction.orths mon.movement-state))
    (when advance-movement-state
      (setv mon.movement-state movement-state))
    (setv d (tuple (gfor c ["x" "y"]
      (if (getattr movement-state c)
        (if (getattr d c)
          0
          (getattr movement-state c))
        (getattr d c)))))
    ; Per IQ, we make only one attempt to find a new direction.
    ; So if this fails, give up.
    (when (= d #(0 0))
      (return F))
    (setv d (get Direction.from-coords d))
    (unless (ok-target)
      (return F)))

  ; We're clear to move.
  (mv-tile mon target)
  (return T))
(setv Monster.act approach)

(defn wander [mon [state-slot "movement_state"] [implicit-attack T]]
  "Wander — If the monster can attack, it does. Otherwise, it chooses a direction (or, with equal odds as any given direction, nothing) with a simplistic psuedorandom number generator. It walks in the chosen direction if it can and the target square is inside the reality bubble."

  (when (and implicit-attack (try-to-attack-player mon))
    (return))

  ; Use a linear congruential generator. Each seed should have a
  ; decent period coprime to the number of options (9)—long enough
  ; to look randomish, but not long.
  ; https://en.wikipedia.org/w/index.php?title=Linear_congruential_generator&oldid=1140372972#c_%E2%89%A0_0
  (setv  m (** 8 3)  c 1  a (+ 2 1))
  (when (= (getattr mon state-slot) None)
    ; Seed the RNG.
    (setattr mon state-slot (% (pos-seed mon.pos) m)))
  (setv options (+ Direction.all #(None)))
  (setv d (get options (% (getattr mon state-slot) (len options))))
  (setattr mon state-slot (% (+ (* a (getattr mon state-slot)) c) m))
  (unless d
    (return))
  (setv [target wly] (walkability mon.pos d :monster? T))
  (unless (= wly 'walk)
    (return))
  (when (> (dist G.player.pos target) G.rules.reality-bubble-size)
    (return))
  (mv-tile mon target))


(defclass Generated [Monster]
  "A monster that can be produced by a generator."

  (setv
    __slots__ []
    score-for-damaging T))

(defclass Generator [Monster]
  "An immobile structure that creates monsters nearby."

  (slot-defaults
    summon-frequency (f/ 1 4)
    summon-hp 1
      ; How many hit points each monster will be summoned with.
    summon-power (f/ 0))
      ; A per-turn accumulator of `summon-frequency`.
  (setv
    mutable-slots ["summon_power"]
    score-for-damaging T
    immune #(Poison)
    summon-class None)
      ; The stem of the monster type to generate.

  (defn [classmethod] read-tile-extras [cls mk-pos v1 v2]
    (dict
      :summon-hp (>> v2 5)
      :summon-frequency (get
        #(1 (f/ 1 2) (f/ 1 3) (f/ 1 4) (f/ 1 5) (f/ 1 6) (f/ 2 5) (f/ 1 10) (f/ 3 5) (+ 1 (f/ 1 3)) (+ 1 (f/ 1 2)) (+ 1 (f/ 2 3)) 2 (f/ 2 3) (f/ 3 4) (f/ 4 5) (f/ 5 6) (f/ 9 10))
          ; These come from IQ's `SetGenFreq`.
        (- (& v2 0b1111) 1))))

  (defn suffix-dict [self]
    (dict
      :HP self.hp
      :pw (mixed-number self.summon-power)
      :freq (mixed-number self.summon-frequency)
      :sHP self.summon-hp))
  (defn info-bullets [self #* extra]
    (.info-bullets (super)
      #("Summoning power" (mixed-number self.summon-power))
      #("Summoning frequency" (mixed-number self.summon-frequency))
      #("Hit points of summoned monsters" self.summon-hp)
      #* extra))

  (defn act [self]
    "Generate — The generator adds its summon frequency to its summon power. If the total is more than 1, the integer part is removed and a corresponding number of monsters are generated in adjacent empty squares. If there are no adjacent empty squares, the expended summon power is wasted. The square that the generator attempts to target rotates through the compass with each summon or failed attempt."
    (summon self self.summon-class self.summon-hp "movement_state")))


(defn defgenerated [
    mapsym name *
    iq-ix-mon iq-ix-gen
    points-mon points-gen
    flavor-mon flavor-gen
    [immune #()]
    #** kwargs]
  "Shorthand for defining both a generated monster and its generator."

  (setv mon-cls (deftile Generated mapsym name
    :iq-ix-mapper ["hp"
      (dict (zip iq-ix-mon [1 2 3]))]
    :immune immune
    :points points-mon
    :flavor flavor-mon
    #** kwargs))
  (deftile Generator (+ "☉" (get mapsym 0)) (+ name " generator")
    :iq-ix-mapper ["hp"
      (dict (zip iq-ix-gen [1 2 3]))]
    :summon-class mon-cls.stem
    :immune (tuple (unique (+ Generator.immune immune)))
    :points points-gen
    :flavor flavor-gen))


(defclass NonGen [Monster]
  "A monster that isn't produced by a generator."

  (setv __slots__ [])

  (defn [classmethod] read-tile-extras [cls mk-pos v1 v2]
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

(defgenerated "b " "a bat"
  :iq-ix-mon [45 71 72] :iq-ix-gen [46 73 74]
  :points-mon 1 :points-gen 3

  :damage-melee #(1 2 3)
  :act wander

  :flavor-mon "Dusk! With a creepy, tingling sensation, you hear the fluttering of leathery wings! Bats! With glowing red eyes and glistening fangs, these unspeakable giant bugs drop onto… wait. These aren't my lecture notes."
  :flavor-gen #[[A faint singing echoes out of the depths of this cave. They sound like they're saying "na na na".]])

(defgenerated "B " "a giant bee"
  :iq-ix-mon [123 124 125] :iq-ix-gen [126 127 128]
  :points-mon 5 :points-gen 15

  :damage-melee #(5 7 9)
  :act wander

  :flavor-mon "Bees bafflingly being bigger'n bats. This is the kind that can survive stinging you. You might not be so lucky."
  :flavor-gen #[[The ancients call this place "the Plounge".]])

(defgenerated "d " "a devil"
  :iq-ix-mon [41 63 64] :iq-ix-gen [42 65 66]
  :points-mon 5 :points-gen 25

  :damage-melee #(3 6 9)
  :damage-shot 10

  :flavor-mon "A crimson-skinned, vaguely humanoid monster. Its eyes glow with the malevolent fires of hell, which it can hurl at you from a distance. Its claws are sharp, but don't hurt quite as much as getting roasted. To its enduring shame, it has no protection whatsoever against fire damage."
  :flavor-gen "A tunnel that goes all the way down to the Bad Place. It stinks of sulfur and invites the innumerable ill-spirited inhabitants of the inferno to ruin your day.")

(defgenerated "w " "a wizard"
  :iq-ix-mon [87 88 89] :iq-ix-gen [90 91 92]
  :points-mon 5 :points-gen 25

  :damage-melee 4
  :damage-shot #(4 8 12)

  :flavor-mon "This fresh-faced would-be scholar has finished sewing the stars onto his robe and is starting to grow a beard. Idok has told the whole class that whoever kills you gets tenure. Considering what the rest of the academic job market is like, the offer has proven irresistible to many."
  :flavor-gen "The Pigpimples Institute of Thaumaturgy and Dweomercraft: a shameless diploma mill that happily takes students' money to teach them one spell, then sends them on a suicide mission against a much smarter and tougher opponent.")

(defgenerated "s " "a shade"
  :iq-ix-mon [171 172 173] :iq-ix-gen [174 175 176]
  :points-mon 6 :points-gen 24

  :immune #(MundaneArrow #* undead-immunities)
  :damage-melee #(3 5 7)

  :flavor-mon #[[A dark spirit with mastery of its semi-corporeal form, allowing ordinary arrows to pass right through it. As it approaches, it hisses "Death!"]]
  :flavor-gen "Oh dear. Considering what's been done to this grave, destroying it would be a mercy.")

(defgenerated "i " "an imp"
  :iq-ix-mon [43 67 68] :iq-ix-gen [44 69 70]
  :points-mon 4 :points-gen 15

  :slot-defaults (dict
    :shot-power (f/ 0)
    :wander-state None)
  :mutable-slots #("shot_power" "wander_state")

  :damage-shot #(1 2 3)
  :act (fn-dd [self]
    (doc f"Coward — If the monster is within {imp-flee-range} squares of you, it flees (per `Approach` in reverse). Otherwise, if it has line of sight to you (ignoring all obstacles) it adds {imp-shot-charge} to its shot power. If this is ≥1, it subtracts 1 to shoot you. Otherwise, it wanders (per `Wander`).")

    (when (and
        (<= (dist G.player.pos self.pos) imp-flee-range)
        (not (player-invisible-to self)))
      (return (approach self :reverse T :implicit-attack F)))
    (when (try-to-attack-player self :dry-run T :shots-ignore-obstacles T)
      (+= self.shot-power imp-shot-charge)
      (when (pop-integer-part self.shot-power)
        (try-to-attack-player self :shots-ignore-obstacles T)
        (return)))
    (wander self "wander_state" :implicit-attack F))

  :info-bullets (fn [self #* extra]
    (Generated.info-bullets self
      #("Shot power" self.shot-power)
      #("Wandering state" self.wander-state)))

  :flavor-mon #[[Weak but incredibly annoying, this snickering little fiend is called a "lobber" in the tongue of the ancients. It throws hellstones, cursed missiles that can pierce most any obstacle. In close quarters, it resorts to cowering helplessly and begging for mercy, but, being a literal demon, it has no compunctions about getting right back to firing at you the moment it feels safe.]]
  :flavor-gen "They don't make ziggurats like they used to.")
(setv imp-flee-range 2)
(setv imp-shot-charge (f/ 4 5))


(deftile NonGen "T " "a thorn tree"
  :iq-ix 51
  :points 10

  :immune #(MundaneArrow MagicArrow Poison)
    ; We follow IQ in making thorn trees immune to poison, although
    ; the IQ manual suggests otherwise.
  :weaknesses #(Fire)
  :damage-melee 4
  :act stationary

  :flavor "From a distance, you can safely giggle at the ghostly. Up close, this arboreal abomination will rake you with its twisted, spiny boughs. Arrows snag in its branches and glance off its gnarled bark, so an intimate encounter may be unavoidable. On the other hand, it's rather flammable. Remember, only you can start forest fires.")

(deftile NonGen "K " "a Dark Knight"
  :iq-ix 53
  :points 75

  :damage-melee 12

  :flavor "This dread warrior wears ink-black armor and carries a heavy chain mace. His devotion to the powers of evil (not to mention his willingness, nay, eagerness to kill you) makes his appropriation of Batman's epithet questionable at best. When you get down to it, he's just trying to distract you from the fact that he's the most basic enemy in the whole dungeon.")

(deftile NonGen "t " "a Tricorn"
  :iq-ix 54
  :points 10

  :damage-melee 5
  :damage-shot 6
  :shot-range 3

  :flavor "Named not for a hat, but for the three horns projecting from their equine heads, Tricorns spend decades mediating while cocooned in woolen blankets. Their richly cultivated spirituality allows them to unleash a spark of static electricity from a fair distance, albeit still not as far as your arrows can fly. Up close, they can poke you with their horns for slightly less damage.")

(deftile NonGen "D " "Death"
  :iq-ix 49
  :points 200

  :immune  #(MundaneArrow Fire #* undead-immunities)
  :resists #(MagicArrow)
  :damage-melee 20

  :flavor "A shadowy hooded figure bearing a wicked scythe who speaks in all capital letters. It can be destroyed, but don't expect that to be easy.")

(deftile NonGen "N " "a negaton"
  :iq-ix 52
  :points 50

  :immune #(PlayerMelee MundaneArrow Fire Poison DeathMagic)
     ; The immunity to death magic is an addition to IQ.
  :damage-melee 25
  :kamikaze T

  :flavor "A quantum of negative energy motivated only by a hatred of princess-based life forms. It can expend its entire payload in a single attack, and, being essentially mindless, it has no qualms about doing so. Magic arrows are pretty much the only thing strong enough to hurt it.")

(deftile NonGen "f " "a floater"
  :iq-ix 47
  :points 2

  :slot-defaults (dict :kamikazed F)
  :mutable-slots #("kamikazed")

  :damage-shot 10
  :shot-range 1
  :kamikaze T

  :act (fn-dd [self]
    (doc f"Float — If you're adjacent, increases your floater disturbance by {floater-disturbance-increment}. If your floater disturbance reaches 1, it's cleared and the monster attacks. Otherwise, the monster wanders per `Wander`.")
    (when (adjacent? self.pos G.player.pos)
      (+= G.player.floater-disturbance floater-disturbance-increment)
      (when (pop-integer-part G.player.floater-disturbance)
        (setv self.kamikazed T)
        (return (try-to-attack-player self))))
    (wander self :implicit-attack F))

  :hook-destroyed (fn [self pos]
    "The monster can attempt a free attack, unless it killed itself by kamikaze."
    (unless self.kamikazed
      (try-to-attack-player self :from-pos pos)))

  :flavor "A giant aerial jellyfish, kept aloft by a foul-smelling and highly reactive gas. It doesn't fly so much as float about in the dungeon drafts. If disturbed, it readily explodes, and its explosions have the remarkable property of harming you and nobody else.")
(setv floater-disturbance-increment (f/ 1 5))


(deftile NonGen "O " "a blob"
  :iq-ix 48
  :points 0

  :slot-defaults (dict :summon-direction None :summon-power (f/ 0))
  :mutable-slots #("summon_direction" "summon_power")

  :immune #(MundaneArrow MagicArrow)
  :damage-melee 6
  :summon-frequency (f/ 1 10)

  :info-bullets (fn [self #* extra]
    (NonGen.info-bullets self
      #("Summoning power" (mixed-number self.summon-power))
      #("Summoning direction" self.summon-direction)
      #* extra))

  :act (fn-dd [self]
    (doc f"If the monster can attack, it does. Otherwise, if it has more than 1 HP, it builds up {it.summon-frequency} summoning power per turn. With enough power, it can split (per `Generate`) into two blobs with half HP (in case of odd HP, the original gets the leftover hit point). If it lacks the HP or summoning power for splitting, it wanders per `Wander`.")
    (when (try-to-attack-player self)
      (return))
    (when (and
        (> self.hp 1)
        (summon self "blob" (// self.hp 2)))
      (-= self.hp (// self.hp 2))
      (return))
    (wander self :implicit-attack F))

  :flavor "What looks like a big mobile puddle of slime is actually a man-sized amoeba. It retains the ability to divide (but not, fortunately, to grow), and its lack of distinct internal anatomy makes arrows pretty useless. It has just enough intelligence to notice that you're standing next to it and try to envelop you in its gloppy bulk.")

(deftile NonGen "S " "a specter"
  :iq-ix 50
  :points 100

  :immune #(MundaneArrow #* undead-immunities)
  :damage-melee 15
  :sees-invisible T

  :act (fn [self]
     "Try to attack or approach per `Approach`. If that fails, try moving with a variation of `Approach` that allows skipping one intermediate tile."
     (or
       (approach self :advance-movement-state F)
       (approach self :implicit-attack F :jump T)))

  :flavor "Yet another evil undead phantasm. This one's a real piece of work: it has a powerful heat-drain attack and the ability to teleport past obstacles.")
