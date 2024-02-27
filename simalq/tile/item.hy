;; --------------------------------------------------------------
;; * Imports
;; --------------------------------------------------------------

(require
  hyrule [branch]
  simalq.macros [unless defmeth]
  simalq.tile [deftile])
(import
  metadict [MetaDict]
  simalq.strings
  simalq.util [CommandError]
  simalq.game-state [G]
  simalq.geometry [pos-seed turn-and-pos-seed burst burst-size at]
  simalq.tile [Tile Damageable annihilate]
  simalq.tile.scenery [Scenery]
  simalq.util [CommandError DamageType StatusEffect msg burst-damage refactor-hp])
(setv  T True  F False)

;; --------------------------------------------------------------
;; * The parent class
;; --------------------------------------------------------------

(defclass Item [Tile]
  "An object the player can pick up."

  (setv
    bold T
      ; A lot of items have small, spindly mapsyms, and we generally
      ; want them to stand out from scenery, so we boldface all of them.
    acquirement-points 0)

  (defmeth hook-player-walked-into []
    (+= G.score @acquirement-points)
    (@pick-up)
    (@rm-from-map))
  (defmeth pick-up [])

  (defmeth hook-remote-action []
    ; Check if we can take the item.
    (@hook-player-walk-to None)
    ; Then actually take it.
    (@hook-player-walked-into)
    T)

  (defmeth info-bullets [#* extra] [
    (@dod "Pickup effect" 'pick-up)
    #* extra
    (@dod "Effect when you shoot it" 'hook-player-shot)
    #("Point value" (format @acquirement-points ","))]))

;; --------------------------------------------------------------
;; * Treasure
;; --------------------------------------------------------------

(deftile "$ " "a lump of fool's gold" Item
  :color 'orange
  :iq-ix 110
    ; The candle, which doen't take an inventory slot and is worth no
    ; points.
  :acquirement-points 0
  :flavor "Whoa! This is worthless. But no less than worthless, at least.")

(deftile "$ " "a pile of silver" Item
  :color 'dark-gray
  :iq-ix 153
    ; The amulet of sight, which doesn't take an inventory slot and is
    ; worth 50 points.
  :acquirement-points 50
  :flavor "Ooh, not quite as shiny.")

(deftile "$ " "a pile of gold" Item
  :color 'dark-yellow
  :iq-ix 18
  :acquirement-points 100
  :flavor "Ooh, shiny.")

(deftile "$ " "a handful of gems" Item
  :color 'red
  :iq-ix 109
  :acquirement-points 250
  :flavor "Ooh, shinier.")

;; --------------------------------------------------------------
;; * Food
;; --------------------------------------------------------------

(defclass Food [Item]
  (setv
    hp-effect None
      ; How much you're healed (or damaged, for negative values)
      ; by the food.
    hp-set-min None
      ; If set, the food sets your HP to at least this value, and
      ; `hp-effect` is ignored.
    eat-messages #()
      ; Messages that can print when you eat the food. A pseudorandom
      ; one is chosen.
    waste-message None)
      ; A message for when you eat the food but it has no effect.

  (defmeth hook-player-shot []
    "The item is destroyed."
    (@rm-from-map)
    (msg "Someone shot the food."))

  (defmeth pick-up []
    (doc (cond
      @hp-set-min
        f"Sets your hit points to at least {(refactor-hp @hp-set-min)}. If you already have that many, the effect is wasted."
      (< @hp-effect 0)
        f"Deals {(- @hp-effect)} poison damage to you."
      T
        f"Grants {(refactor-hp @hp-effect)} hit points."))
    (msg (if
      (and
        @hp-set-min
        (>= G.player.hp (refactor-hp @hp-set-min)))
      @waste-message
      (get @eat-messages (%
        ; Food at the same position on the same level number will have
        ; the same message. Nearby pieces of food will typically have
        ; different messages.
        (pos-seed @pos)
        (len @eat-messages)))))
    (cond
      @hp-set-min
        (setv G.player.hp (max (refactor-hp @hp-set-min) G.player.hp))
      (< @hp-effect 0)
        (.damage G.player (- @hp-effect) DamageType.Poison)
      T
        (+= G.player.hp (refactor-hp @hp-effect)))))

(setv (get Tile.types-by-iq-ix 21) (fn [pos _ te-v2]
  ; IQ's three types of unknown potion are mapped to items with fixed
  ; HP effects equal to the mean HP effect of the given type.
  [(
    (get Tile.types
      (get ["meal" "empty platter" "rotten food"] (- te-v2 1)))
    :pos pos)]))

(deftile "% " "a meal" Food
  :color 'red
  :iq-ix 20  ; healing potion
  :acquirement-points 0

  :hp-effect 100
  :eat-messages simalq.strings.meal-messages

  :flavor "Food, glorious food!")

(deftile "% " "a snack" Food
  :color 'navy
  :iq-ix 83  ; healing salve
  :acquirement-points 0

  :hp-effect 25
  :eat-messages simalq.strings.snack-messages

  :flavor "A little something to tide you over.")

(deftile "☠ " "some rotten food" Food
  :color 'red
  :iq-ix None  ; subtype of unknown potion
  :acquirement-points 0

  :hp-effect -100
  :eat-messages simalq.strings.rotten-food-messages

  :flavor #[[Idok was hoping to fool you with this, but he forgot to take off the label that says "POISON: Do Not Eat".]])

(deftile "% " "an empty platter" Food
  :color 'black
  :iq-ix None  ; subtype of unknown potion
  :acquirement-points 0

  :hp-effect 0
  :eat-messages #("There's nothing to eat here. Rats.")

  :flavor "It looks like one of the monsters got to this food first.")

(deftile "% " "some dessert" Food
  :color 'rose
  :iq-ix 155  ; super-healing potion
  :acquirement-points 0

  :hp-set-min 500
  :eat-messages simalq.strings.dessert-messages
  :waste-message "You don't have room for dessert. It goes to waste."

  :flavor "In the gustatory tradition of Tris's kingdom, saving room for dessert became so highly valued that there arose a practice of eating dessert as the first course.")

(deftile "☠ " "a jar of poison" Food
  :color 'dark-green
  :iq-ix 86
  :acquirement-points 0

  :hp-effect -50
  :eat-messages #("You drink a jar of poison. It tastes pretty bad.")

  ; Define the shot effect in terms of poison-gas bombs.
  :!pb (meth []
    (get Tile.types "poison-gas bomb"))
  :hook-player-shot (meth []
    (doc (.dynadoc (. (@pb) use) (@pb)))
    (.use (@pb) ((@pb)) @pos)
    (@rm-from-map))

  :flavor "I think you're not supposed to drink this.")

;; --------------------------------------------------------------
;; * Miscellany
;; --------------------------------------------------------------

(deftile "⚷ " "a key" Item
  :iq-ix 19
  :acquirement-points 50

  :hook-player-walk-to (meth [origin]
    (when (>= G.player.keys G.rules.max-keys)
      (raise (CommandError "Your keyring has no room for another key."))))

  :pick-up (meth []
    (doc f"Adds to your count of keys. If you're at the maximum number
      of keys ({G.rules.max-keys}), you can't step on its square.")
    (+= G.player.keys 1))

  :flavor "Idok uses only the worst locks and keys that money can buy. The keys are bulky and heavy, yet immediately snap into pieces on being used once, and every lock can be opened by any old key.")

(deftile "↑ " "some magic arrows" Item
  :color 'purple
  :iq-ix 27
  :acquirement-points 100

  :!n-arrows 10
  :pick-up (meth []
    (doc f"Gives you {@n-arrows} magic arrows. Magic arrows are fired in place of regular arrows. They do {G.rules.player-shot-damage-magic} damage, and hurt some monsters that are immune to mundane arrows. If a magic arrow destroys a monster or object, it continues on its path and can keep doing damage.")
    (+= G.player.magic-arrows @n-arrows))

  :flavor "Now we're talkin'! These missiles engraved with mystical runes hit hard and keep on going. Sadly, Tris is so excited to use them that she can't shoot mundane arrows until she's used them all up.")

(deftile "⏲ " "a clock" Item
  :iq-ix 138
    ; IQ uses an hourglass. We use a clock instead because it's hard
    ; to get a non-emoji hourglass.
  :acquirement-points 0

  :!time-bonus 25
  :pick-up (meth []
    (doc f"Adds {@time-bonus} turns to the current time limit.")
    (+= G.time-left @time-bonus))

  :flavor "Oh dear! Oh dear! I shall be late!")

;; --------------------------------------------------------------
;; * Status-effect items
;; --------------------------------------------------------------

(defclass StatusEffectItem [Item]
  (setv
    effect None
    duration None)

  (defmeth help [])

  (defmeth pick-up []
    (doc (@help))
    (.add @effect @duration)))

(deftile "! " "an amulet of invulnerability" StatusEffectItem
  :color 'dark-yellow
  :iq-ix 26
  :acquirement-points 100

  :effect StatusEffect.Ivln
  :duration 20

  :help (meth []
    f"Makes you invulnerable for {@duration} more turns, protecting you from all damage and ambient poison, but not harmful status effects or disenchantment.")
  :flavor "A star-shaped pendant with two black spots in the center. Its magic is short-lived but potent indeed.")

(deftile "! " "a cloak of invisibility" StatusEffectItem
  :color 'blue
  :iq-ix 25
  :acquirement-points 100

  :effect StatusEffect.Ivis
  :duration 25

  :help (meth []
    f"Makes you invisible for {@duration} more turns. Most monsters can't track or shoot you while you're invisible, unless you're adjacent to them.")
  :flavor "A cape enchanted with the power of the night sky. Try not to get it snagged on any loose flagstones (especially when it's invisible).")

(deftile "! " "a potion of speed" StatusEffectItem
  :color 'red
  :iq-ix 34
  :acquirement-points 100

  :effect StatusEffect.Fast
  :duration 10

  :help (meth []
    f"Lets you act twice per turn for {@duration} more turns.")
  :flavor "This cool concoction puts a pep in your step and a swiftness in your sword-swings.")

(deftile "! " "an amulet of poison" StatusEffectItem
  :color 'dark-green
  :iq-ix 159
    ; IQ calls this an "amulet of poisonous touch", but no real
    ; touching is required, and IQ's poisonous amulets have also been
    ; renamed, so there should be no danger of confusion there.
  :acquirement-points 250

  :effect StatusEffect.Pois
  :duration 15

  :help (meth []
    f"Surrounds you in a cloud of poison for {@duration} more turns. You are immune to the effect, but all monsters within 1 square take {G.rules.player-poison-damage} damage at the end of each turn.")
  :flavor "Finally, somebody concocted a poison that hurts monsters but not you. This handy wearable fumigator lets you apply it hands-free.")

(deftile "! " "a passwall amulet" StatusEffectItem
  :color 'purple
  :iq-ix 151
  :acquirement-points 150

  :effect StatusEffect.Pass
  :duration 20

  :help (meth []
    f"Makes you semi-material for {@duration} more turns, allowing you to walk through walls (plus other scenery types noted as affected by a passwall amulet). You ignore most properties of the affected scenery, such as a one-way door's restrictions on movement direction, or a locked door's consumption of a key.")
  :flavor "Looks like the ethereal power of one of those many, many evil undead phantasms rubbed off onto this little trinket. Try not to let the magic run out when you're entirely surrounded by walls. Getting buried alive is a bad way to go.")

(deftile "! " "a ring of protection" StatusEffectItem
  :color 'dark-orange
  :iq-ix 158
  :acquirement-points 150

  :effect StatusEffect.Prot
  :duration 25

  :help (meth []
    f"Protects you (for {@duration} more turns) from harmful status effects and disenchantment.")
  :flavor "Also known as a ring of anti-anti-magic. Fortunately, anti-anti-anti-magic traps are still in dungeon R&D.")

(deftile "⚷ " "a magical key" StatusEffectItem
  :color 'purple
  :iq-ix 183
  :acquirement-points 100

  :effect StatusEffect.MKey
  :duration 12

  :help (meth []
    f"Grants you a magical key for {@duration} more turns. In this state, you can open scenery objects that would require a key without actually using one. Furthermore, you can open metal doors, and bumping into a one-way door turns it into a normal door (even if its direction is such that you could normally enter it).")
  :flavor "A huge key blinged out with a nearly-as-huge finely cut amethyst. It's so powerful that within a few moments of being activated by your body heat, it opens itself, leaving only a shiny dust that has too many heavy metals to be useful as glitter.")

;; --------------------------------------------------------------
;; * Usables
;; --------------------------------------------------------------

(defclass Usable [Item]
  "An item that's added to your inventory and can thereafter be
  consumed as an action."

  (setv
    targeted T)
      ; Whether the item should be used with a target. The argument
      ; `target` is provided to `use` only if this is true.

  (defmeth hook-player-walk-to [origin]
    (unless (any (gfor  x G.player.inventory  (is x None)))
      (raise (CommandError "Your inventory is full."))))

  (defmeth pick-up []
    (doc f"Adds the item to your inventory. If you're at the maximum number
      of usable items ({G.rules.max-usables}), you can't step on its square.")
    (@rm-from-map)
    (setv
      (get G.player.inventory (next (gfor
        [ix item] (enumerate G.player.inventory)
        :if (is item None)
        ix)))
      @))

  (defmeth use [target]
    "Called when the player applies the item for use. The caller
    will destroy the item afterwards."
    (raise NotImplementedError))

  (defmeth info-bullets [#* extra]
    (.info-bullets (super)
      (@dod f"Effect when applied ({(if @targeted "" "un")}targeted)" 'use)
      #* extra)))

;; --------------------------------------------------------------
;; ** Wands
;; --------------------------------------------------------------

(deftile "/ " "a wand of nothing" Usable
  :iq-ix 147  ; wand of light
  :acquirement-points 50

  :targeted F
  :use (meth []
    "Does nothing."
    (msg (get simalq.strings.wand-of-nothing-messages (%
      (turn-and-pos-seed G.player.pos)
      (len simalq.strings.wand-of-nothing-messages)))))

  :flavor "Technically magical, but not terribly useful.")

(deftile "/ " "a wand of shielding" Usable
  :color 'dark-orange
  :iq-ix 200
  :acquirement-points 100

  :targeted F
  :use (meth []
    "Creates a magical energy shield in each square adjacent to you. These shield tiles block monsters and their shots, but not you or your shots."
    (for [p (burst G.player.pos 1 :exclude-center T)]
      (Tile.make p "magical energy shield")))

  :flavor "Cowardice is the better part of valor.")

(deftile "/ " "a wall-making wand" Usable
  :color 'red
  :iq-ix 33
  :acquirement-points 150

  :use (meth [target]
    "Creates one tile of ordinary wall."
    (Tile.make target "wall"))

  :flavor "This device is detested by the stonemason's union, but valued by homeowners and combat engineers, not to mention tyrants who desire vast dungeons.")

(deftile "/ " "a wall-destroying wand" Usable
  :color 'dark-green
  :iq-ix 32
    ; IQ calls this a "passwall wand", not to be confused with
    ; passwall amulets, which follow different rules.
  :acquirement-points 150

  :use (meth [target]
    "Destroys one tile of wall, or other scenery types noted as affected by wall destruction."
    (for [tile (at target)]
      (when (and (isinstance tile Scenery) tile.wand-destructible)
        (.rm-from-map tile)
        (return)))
    (raise (CommandError "There isn't a destructible tile there.")))

  :flavor #[[I always thought the phrase "open sesame" was a humorous deliberate corruption of "open says-a-me", but since it comes to us from French, if not from Arabic and then French, this is unlikely.\n\n    I've got a hole in me pocket.]])

(deftile "/ " "a wand of exit" Usable
  :color-bg 'lime
  :iq-ix 105  ; exit-making wand
  :acquirement-points 200

  :use (meth [target]
    #[[Creates an exit on a free square. The exit's destination level will be equal to the default "next level" of the current level (which might actually be a previous level, or even the very same level).]]
    (when (at target)
      (raise (CommandError "This wand can only target an empty square.")))
    (Tile.make target "exit"))

  :flavor "Showmanship, George. When you hit that high note, you say goodnight and walk off.")

(deftile "/ " "a phase wand" Usable
  :color 'brown
  :iq-ix 143
  :acquirement-points 50

  :use (meth [target]
    "If there's at least one phasing wall on the target square, then the topmost one is phase-shifted. Otherwise, a new out-of-phase wall is created."
    (for [t (at target)]
      (when (isinstance t hy.I.simalq/tile/scenery.PhasingWall)
        (.phase-shift t)
        (return)))
    (Tile.make target "phasing wall (out of phase)"))

  :flavor "This attempt to create a portable phase trigger didn't entirely succeed.")

(deftile "/ " "a wand of webs" Usable
  :color 'dark-gray
  :iq-ix 170
  :acquirement-points 150

  :targeted F
  :use (meth []
    "Creates a web at every empty square in the reality bubble."
    (for [
        pos (burst G.player.pos G.rules.reality-bubble-size)
        :if (not (at pos))]
      (Tile.make pos "web")))

  :flavor "Of all the wands you can find in the dungeon, this is definitely one of them. It looks and feels awfully like a body part from an oversized arachnid, and it makes a huge mess, but it has its uses.")

(deftile "/ " "a wand of death" Usable
  :color 'blue
  :iq-ix 156
  :acquirement-points 250

  :!mon-damage #(Inf Inf Inf)
  :!player-damage 25
  :use (meth [target]
    (doc f"Kills all monsters in {(burst-size (len @mon-damage))}, except those immune to death magic. If you're in the burst, you take {@player-damage} damage.")
    (burst-damage target :damage-type DamageType.DeathMagic
      :amount @mon-damage :player-amount @player-damage
      :color 'dark-gray))

  :flavor "This fell device vibrates with the barely contained energies of Hades as they hunger for the souls of the living. Aim carefully.")

(deftile "/ " "a wand of annihilation" Usable
  :color 'navy
  :iq-ix 104
  :acquirement-points 250

  :use (meth [target]
    "Utterly destroys everything at the target."
    (for [t (at target)  :if (not t.superblock)]
      (annihilate target)
      (return))
    (raise (CommandError "There isn't anything you can annihilate there.")))

  :flavor "Now this is a real wand of nothing. A wand of nothingness. A wand of nothing left.")

(deftile "/ " "a wand of flame" Usable
  :color 'orange
  :iq-ix 154
  :acquirement-points 100

  :!mon-damage #(2 1 1)
  :use (meth [target]
    (doc f"Does {(get @mon-damage 1)} fire damage to all monsters in {(burst-size (len @mon-damage))}, except at the center square, where they take {(get @mon-damage 0)} fire damage. Furthermore, all webs in the burst are destroyed. You take no damage.")
    (for [
        pos (burst-damage target :damage-type DamageType.Fire
          :amount @mon-damage
          :color 'orange)
        tile (list (at pos))
        :if (= tile.stem "web")]
      (.rm-from-map tile)))

  :flavor "Clean out the cobwebs and have yourself some barbecued goblin.")

(deftile "/ " "a wand of teleportation" Usable
  :color 'purple
  :iq-ix 106  ; wand of gating
  :acquirement-points 150

  :use (meth [target]
    #[[Teleports you to a free square.]]
    (when (at target)
      (raise (CommandError "This wand can only target an empty square.")))
    (.move G.player target))

  :flavor "A disposable controllable teleporter in convenient portable form, to get you in or out of trouble as the situation demands.")

(deftile "/ " "a wand of remote action" Usable
  :color 'medium-green
  :iq-ix 198
  :acquirement-points 50

  :use (meth [target]
    "Performs an action on the topmost applicable tile on the square. Scenery that has a special effect when bumped, such as phase triggers, can be activated, and items can be picked up."
    (for [tile (at target)]
      (when (.hook-remote-action tile)
        (return)))
    (raise (CommandError "There isn't anything you can affect there.")))

  :flavor "This wand bears an uncanny resemblance to a grabber arm, and is nearly as useful. Many a wizard has used one to eat tortilla chips from across the room. Now this marvelous power is yours.")

;; --------------------------------------------------------------
;; ** Bombs
;; --------------------------------------------------------------

(defclass FireBomb [Usable]
  (setv
    use-blast-damage None
    shot-blast-damage None)

  (defmeth bomb-burst [target amount]
    (burst-damage target :damage-type DamageType.Fire
      :amount amount
      :color 'orange))

  (defmeth use [target]
    (doc f"Explodes in {(burst-size (len @use-blast-damage))} of fire, damaging monsters according to their distance from the center. The amounts of damage at the center and each successive distance are: {(.join ", " (map str @use-blast-damage))}. You take no damage.")
    (@bomb-burst target @use-blast-damage))

  (defmeth hook-player-shot []
    (doc f"Explodes in a weaker {(burst-size (len @shot-blast-damage) :article? F)}, with these damages: {(.join ", " (map str @shot-blast-damage))}.")
    (@bomb-burst @pos @shot-blast-damage)
    (@rm-from-map)))

(deftile "0 " "a standard bomb" FireBomb
  :color 'brown
  :iq-ix 31  ; Just called a "bomb" in IQ
  :acquirement-points 100

  :use-blast-damage #(3 2 1)
  :shot-blast-damage #(2 1)

  :flavor "Medieval-esque swordsmen tossing the occasional explosive has been a tradition since 1986. But you shouldn't expect to find a lot of these, so make 'em count.")

(deftile "0 " "a strong bomb" FireBomb
  :color 'blue
  :iq-ix 84
  :acquirement-points 150

  :use-blast-damage #(3 3 2 1)
  :shot-blast-damage #(3 2 1)

  :flavor "A bigger bomb for the discerning bomber.")

(deftile "0 " "a super-bomb" FireBomb
  :color 'red
  :iq-ix 85
  :acquirement-points 200

  :use-blast-damage #(3 3 2 2 1 1)
  :shot-blast-damage #(3 3 2 1)

  :flavor "Heavy ordnance. Kills monsters dead, with a bomb pattern that would put a feather in Colonel Cathcart's cap.\n\n    \"Kaboom?\"\n    \"Yes, Rico. Kaboom.\"")


(deftile "0 " "a poison-gas bomb" Usable
  :color 'dark-green
  :iq-ix 121
  :acquirement-points 150

  :!player-damage 20
  :!mon-damage 3
  :!use-burst-size 3
  :!shot-burst-size 2

  :!bomb-burst (meth [pos size]
    (burst-damage pos :damage-type DamageType.Poison
      :amount (* [@mon-damage] size)
      :color 'moss-green
      :player-amount @player-damage))

  :use (meth [target]
    (doc f"Explodes in {(burst-size @use-burst-size)} of poison, which does {@mon-damage} poison damage to monsters and {@player-damage} to you.")
    (@bomb-burst target @use-burst-size))

  :hook-player-shot (meth []
    (doc f"As when applied, but with a smaller {(burst-size @shot-burst-size :article? F)}.")
    (@bomb-burst @pos @shot-burst-size)
    (@rm-from-map))

  :flavor "A lot of highly concentrated toxic gas, coupled with an explosive that quickly disperses it. Such poisons are indiscriminate in their death-dealing. Well, that's not true. They discriminate thoroughly in favor of the many poison-proof monsters. They just won't hold back on you.\n\n    Chemical Weapons Convention? More like Chemical Weapons Suggestion.")


(deftile "0 " "an earthquake bomb" Usable
  :color 'dark-orange
  :iq-ix 157
  :acquirement-points 200

  :!quake-damage-monster 3
  :!quake-damage-wall 2
  :!new-cracked-wall-starting-hp 4
  :!use-burst-size 2
  :!shot-burst-size 1

  :!bomb-burst (meth [target size]
    (for [
        p (burst-damage target :damage-type DamageType.Fire
          :amount (* [@quake-damage-monster] (+ size 1))
          :color 'dark-orange)
        tile (list (at p))]
      ; Contra IQ, we leave empty floor alone and don't create holes.
      (branch (in tile.stem it)
        ["wall" "trapped wall"]
          (.replace tile "cracked wall" :hp @new-cracked-wall-starting-hp)
        ["cracked wall"]
          (.damage tile @quake-damage-wall None)
        ["breakable wall (meridional)" "breakable wall (zonal)" "fading wall"]
          (.rm-from-map tile)
        ["pillar"]
          (.replace tile "broken pillar"))))

  :use (meth [target]
    (doc f"Explodes in {(burst-size @use-burst-size)} that does {@quake-damage-monster} fire damage to monsters and {@quake-damage-wall} damage to cracked walls. Furthermore, the blast turns normal walls and trapped walls into cracked walls with {@new-cracked-wall-starting-hp} HP, instantly destroys breakable walls and fading walls, and breaks pillars.")
    (@bomb-burst target @use-burst-size))

  :hook-player-shot (meth []
    (doc f"As when applied, but with a smaller {(burst-size @shot-burst-size :article? F)}.")
    (@bomb-burst @pos @shot-burst-size)
    (@rm-from-map))

  :flavor "Magic and technology have combined into an explosive that can make the earth itself tremble in a limited area. The methodology was originally developed as an alternative to fracking, but it proved more useful in military applications.\n\n    You were too tricky for your own good, Thanos!")

;; --------------------------------------------------------------
;; * Artifacts
;; --------------------------------------------------------------

(defclass Artifact [Item]
  (setv
    color-bg 'magenta)

  (defmeth help [])

  (defmeth pick-up []
    (doc (@help))
    (setv (get G.player.artifacts @stem) T)))

(deftile "[ " "the Magic Shield" Artifact
  :iq-ix 28
  :acquirement-points 3,000

  :help (meth []
    f"Permanently reduces all damage from monsters' attacks to {G.rules.artifact-shield-factor} the normal value, rounded up. Damage from other sources is unaffected.")
  :flavor "The best defense is a good defense.")

(deftile "( " "the Elven Bow" Artifact
  :iq-ix 29
  :acquirement-points 4,000

  :help (meth []
    f"Permanently increases your shot damage (with mundane arrows) to {G.rules.player-shot-damage-artifact}.")
  :flavor "A mighty weapon carved from sacred wood in the realm of maple syrup and free healthcare.")

(deftile ") " "the Holy Sword" Artifact
  :iq-ix 30
  :acquirement-points 5,000

  :help (meth []
    f"Permanently increases your melee damage to {G.rules.player-melee-damage-artifact}.")
  :flavor "A weapon blessed by the vaguely defined divine personages that may or may not watch over you. What it lacks in lore, in makes up for in murderyness.\n\n    I came not to send peace, but a sword.")
