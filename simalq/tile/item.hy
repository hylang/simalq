(require
  simalq.macros [defn-dd fn-dd unless])
(import
  simalq.strings
  simalq.util [CommandError]
  simalq.game-state [G]
  simalq.geometry [pos-seed burst at]
  simalq.tile [Tile deftile destroy-tile rm-tile add-tile]
  simalq.tile.scenery [Scenery]
  simalq.util [CommandError DamageType StatusEffect hurt-player msg burst-damage])
(setv  T True  F False)


(defclass Item [Tile]
  "An object the player can pick up."

  (setv
    __slots__ []
    destroy-after-pickup T)

  (defn hook-player-walked-into [self]
    (+= G.score self.points)
    (.pick-up self)
    (when self.destroy-after-pickup
      (destroy-tile self)))
  (defn pick-up [self])

  (defn info-bullets [self #* extra] [
    #("Point value" (format self.points ","))
    (when (is-not (. (type self) pick-up) Item.pick-up)
      #("Pickup effect" (or
        self.pick-up.__doc__
        (self.pick-up.dynadoc self))))
    #* extra
    (when self.hook-player-shot
      #("Effect when you shoot it" (or
        self.hook-player-shot.__doc__
        (self.hook-player-shot.dynadoc self))))]))


(deftile Item "$ " "a pile of gold"
  :color 'dark-yellow
  :iq-ix 18
  :points 100
  :flavor "Ooh, shiny.")

(deftile Item "$ " "a handful of gems"
  :color 'red
  :iq-ix 109
  :points 250
  :flavor "Ooh, shinier.")


(defclass Food [Item]
  (setv
    __slots__ []
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

  (defn hook-player-shot [self]
    "The item is destroyed."
    (destroy-tile self)
    (msg "Someone shot the food."))

  (defn-dd pick-up [self]
    (doc (cond
      it.hp-set-min
        f"Sets your hit points to at least {it.hp-set-min}. If you already have that many, the effect is wasted."
      (< it.hp-effect 0)
        f"Deals {(- it.hp-effect)} poison damage to you."
      T
        f"Grants {it.hp-effect} hit points."))
    (msg (if (and self.hp-set-min (>= G.player.hp self.hp-set-min))
      self.waste-message
      (get self.eat-messages (%
        ; Food at the same position on the same level number will have
        ; the same message. Nearby pieces of food will typically have
        ; different messages.
        (pos-seed self.pos)
        (len self.eat-messages)))))
    (cond
      self.hp-set-min
        (setv G.player.hp (max self.hp-set-min G.player.hp))
      (< self.hp-effect 0)
        (hurt-player (- self.hp-effect) DamageType.Poison)
      T
        (+= G.player.hp self.hp-effect))))

(setv (get Tile.types-by-iq-ix 21) (fn [pos _ te-v2]
  ; IQ's three types of unknown potion are mapped to items with fixed
  ; HP effects equal to the mean HP effect of the given type.
  [(
    (get Tile.types
      (get ["meal" "empty platter" "rotten food"] (- te-v2 1)))
    :pos pos)]))

(deftile Food "% " "a meal"
  :color 'red
  :iq-ix 20  ; healing potion
  :points 0

  :hp-effect 100
  :eat-messages simalq.strings.meal-messages

  :flavor "Food, glorious food!")

(deftile Food "% " "a snack"
  :color 'navy
  :iq-ix 83  ; healing salve
  :points 0

  :hp-effect 25
  :eat-messages simalq.strings.snack-messages

  :flavor "A little something to tide you over.")

(deftile Food "☠ " "some rotten food"
  :color 'red
  :iq-ix None  ; subtype of unknown potion
  :points 0

  :hp-effect -100
  :eat-messages simalq.strings.rotten-food-messages

  :flavor #[[Idok was hoping to fool you with this, but he forgot to take off the label that says "POISON: Do Not Eat".]])

(deftile Food "% " "an empty platter"
  :color 'black
  :iq-ix None  ; subtype of unknown potion
  :points 0

  :hp-effect 0
  :eat-messages #("There's nothing to eat here. Rats.")

  :flavor #[[It looks like one of the monsters got to this food first.]])

(deftile Food "% " "some dessert"
  :color 'rose
  :iq-ix 155  ; super-healing potion
  :points 0

  :hp-set-min 500
  :eat-messages simalq.strings.dessert-messages
  :waste-message "You don't have room for dessert. It goes to waste."

  :flavor "In the gustatory tradition of Tris's kingdom, saving room for dessert became so highly valued that there arose a practice of eating dessert as the first course.")

(deftile Food "☠ " "a jar of poison"
  :color 'dark-green
  :iq-ix 86
  :points 0

  :hp-effect -50
  :eat-messages #("You drink a jar of poison. It tastes pretty bad.")

  :hook-player-shot (fn-dd [self]
    (doc #[f[Explodes in a size-{(get poison-burst "size")} burst of poison, which does {(get poison-burst "dmg_monster")} poison damage to monsters and {(get poison-burst "dmg_player")} to you.]f])
    (burst-damage self.pos :damage-type DamageType.Poison
      :amount (*
        [(get poison-burst "dmg_monster")]
        (+ 1 (get poison-burst "size")))
      :color 'moss-green
      :player-amount (get poison-burst "dmg_player"))
    (destroy-tile self))

  :flavor "I think you're not supposed to drink this.")
(setv poison-burst (dict
  :size 2
  :dmg-player 20
  :dmg-monster 3))


(deftile Item "⚷ " "a key"
  :iq-ix 19
  :points 50

  :hook-player-walk-to (fn [self origin]
    (when (>= G.player.keys G.rules.max-keys)
      (raise (CommandError "Your keyring has no room for another key."))))

  :pick-up (fn-dd [self]
    (doc f"Adds to your count of keys. If you're at the maximum number
      of keys ({G.rules.max-keys}), you can't step on its square.")
    (+= G.player.keys 1)
    (assert (<= G.player.keys G.rules.max-keys)))

  :flavor "Idok uses only the worst locks and keys that money can buy. The keys are bulky and heavy, yet immediately snap into pieces on being used once, and every lock can be opened by any old key.")


(deftile Item "↑ " "some magic arrows"
  :color 'purple
  :iq-ix 27
  :points 100

  :pick-up (fn-dd [self]
    (doc f"Gives you {G.rules.magic-arrows-pickup-size} magic arrows. Magic arrows are fired in place of regular arrows. They do {G.rules.player-shot-damage-magic} damage, and hurt some monsters that are immune to mundane arrows. If a magic arrow destroys a monster or object, it continues on its path and can keep doing damage.")
    (+= G.player.magic-arrows G.rules.magic-arrows-pickup-size))

  :flavor "Now we're talkin'! These missiles engraved with mystical runes hit hard and keep on going. Sadly, Tris is so excited to use them that she can't shoot mundane arrows until she's used them all up.")


(defclass StatusEffectItem [Item]
  (setv
    __slots__ []
    effect None
    duration None)

  (defn help [self])

  (defn-dd pick-up [self]
    (doc (.help it))
    (+=
      (get G.player.status-effects
        (getattr StatusEffect (str self.effect)))
      self.duration)))

(deftile StatusEffectItem "! " "an amulet of invulnerability"
  :color 'dark-yellow
  :iq-ix 26
  :points 100

  :effect 'Ivln
  :duration 20

  :help (fn [self]
    f"Makes you invulnerable for {self.duration} more turns, protecting you from all damage and ambient poison, but not harmful status effects or disenchantment.")
  :flavor "A star-shaped pendant with two black spots in the center. Its magic is short-lived but potent indeed.")

(deftile StatusEffectItem "! " "a potion of speed"
  :color 'dark-green
  :iq-ix 34
  :points 100

  :effect 'Fast
  :duration 10

  :help (fn [self]
    f"Lets you act twice per turn for {self.duration} more turns.")
  :flavor "This cool concoction puts a pep in your step and a swiftness in your sword-swings.")

(deftile StatusEffectItem "! " "a cloak of invisibility"
  :color 'blue
  :iq-ix 25
  :points 100

  :effect 'Ivis
  :duration 25

  :help (fn [self]
    f"Makes you invisible for {self.duration} more turns. Most monsters can't track or shoot you while you're invisible, unless you're adjacent to them.")
  :flavor "A cape enchanted with the power of the night sky. Try not to get it snagged on any loose flagstones (especially when it's invisible).")


(defclass Usable [Item]
  "An item that's added to your inventory and can thereafter be
  consumed as an action."

  (setv
    __slots__ []
    destroy-on-pickup F
    targeted T)
      ; Whether the item should be used with a target. The argument
      ; `target` is provided to `use` only if this is true.

  (defn hook-player-walk-to [self origin]
    (unless (any (gfor  x G.player.inventory  (is x None)))
      (raise (CommandError "Your inventory is full."))))

  (defn-dd pick-up [self]
    (doc f"Adds the item to your inventory. If you're at the maximum number
      of usable items ({G.rules.max-usables}), you can't step on its square.")
    (rm-tile self)
    (setv
      (get G.player.inventory (next (gfor
        [ix item] (enumerate G.player.inventory)
        :if (is item None)
        ix)))
      self))

  (defn use [self target]
    "Called when the player applies the item for use. The caller
    will destroy the item afterwards."
    (raise NotImplementedError))

  (defn info-bullets [self #* extra]
    (.info-bullets (super)
      #(f"Effect when applied ({(if self.targeted "" "un")}targeted)" (or
        self.use.__doc__
        (self.use.dynadoc self)))
      #* extra)))


(deftile Usable "/ " "a wand of shielding"
  :color 'orange
  :iq-ix 200
  :points 100

  :targeted F
  :use (fn [self]
    "Creates a magical energy shield in each square adjacent to you. These shield tiles block monsters and their shots, but not you or your shots."
    (for [p (burst G.player.pos 1 :exclude-center T)]
      (add-tile p "magical energy shield")))

  :flavor "Cowardice is the better part of valor.")

(deftile Usable "/ " "a wall-making wand"
  :color 'red
  :iq-ix 33
  :points 150

  :use (fn [self target]
    "Creates one tile of ordinary wall."
    (add-tile target "wall"))

  :flavor "This device is detested by the stonemason's union, but valued by homeowners and combat engineers, not to mention tyrants who desire vast dungeons.")

(deftile Usable "/ " "a passwall wand"
  :color 'dark-green
  :iq-ix 32
  :points 150

  :use (fn [self target]
    "Destroys one tile of wall, or other scenery types noted as destructible with a passwall wand."
    (for [tile (at target)]
      (when (and (isinstance tile Scenery) tile.destructible-by-passwall-wand)
        (destroy-tile tile)
        (return)))
     (raise (CommandError "There isn't a destructible tile there.")))

  :flavor #[[I always thought the phrase "open sesame" was a humorous deliberate corruption of "open says-a-me", but since it comes to us from French, if not from Arabic and then French, this is unlikely.]])


(defclass FireBomb [Usable]
  (setv
    __slots__ []
    use-blast-damage None
    shot-blast-damage None)

  (defn bomb-burst [self target amount]
    (burst-damage target :damage-type DamageType.Fire
      :amount amount
      :color 'orange))

  (defn-dd use [self target]
    (doc f"Explodes in a size-{(- (len it.use-blast-damage) 1)} burst of fire, damaging monsters according to their distance from the center. The amounts of damage at the center and each successive distance are: {(.join ", " (map str it.use-blast-damage))}. You take no damage.")
    (.bomb-burst self target self.use-blast-damage))

  (defn-dd hook-player-shot [self]
    (doc f"Explodes in a weaker size-{(- (len it.shot-blast-damage) 1)} burst, with these damages: {(.join ", " (map str it.shot-blast-damage))}.")
    (.bomb-burst self self.pos self.shot-blast-damage)
    (destroy-tile self)))

(deftile FireBomb "0 " "a standard bomb"
  :color 'dark-green
  :iq-ix 31  ; Just called a "bomb" in IQ
  :points 100

  :use-blast-damage #(3 2 1)
  :shot-blast-damage #(2 1)

  :flavor "Medieval-esque swordsmen tossing the occasional explosive has been a tradition since 1986. But you shouldn't expect to find a lot of these, so make 'em count.")

(deftile FireBomb "0 " "a strong bomb"
  :color 'blue
  :iq-ix 84
  :points 150

  :use-blast-damage #(3 3 2 1)
  :shot-blast-damage #(3 2 1)

  :flavor "A bigger bomb for the discerning bomber.")

(deftile FireBomb "0 " "a super-bomb"
  :color 'red
  :iq-ix 85
  :points 200

  :use-blast-damage #(3 3 2 2 1 1)
  :shot-blast-damage #(3 3 2 1)

  :flavor "Heavy ordnance. Kills monsters dead, with a bomb pattern that would put a feather in Colonel Cathcart's cap.\n\n    \"Kaboom?\"\n    \"Yes, Rico. Kaboom.\"")


(defclass Artifact [Item]
  (setv
    __slots__ []
    color-bg 'magenta)

  (defn help [self])

  (defn-dd pick-up [self]
    (doc (.help it))
    (setv (get G.player.artifacts self.stem) T)))

(deftile Artifact "[ " "the Magic Shield"
  :iq-ix 28
  :points 3,000

  :help (fn [self]
    f"Permanently reduces all damage from monsters' attacks to {G.rules.artifact-shield-factor} the normal value, rounded up. Damage from other sources is unaffected.")
  :flavor "The best defense is a good defense.")

(deftile Artifact "( " "the Elven Bow"
  :iq-ix 29
  :points 4,000

  :help (fn [self]
    f"Permanently increases your shot damage (with mundane arrows) to {G.rules.player-shot-damage-artifact}.")
  :flavor "A mighty weapon carved from sacred wood in the realm of maple syrup and free healthcare.")

(deftile Artifact ") " "the Holy Sword"
  :iq-ix 30
  :points 5,000

  :help (fn [self]
    f"Permanently increases your melee damage to {G.rules.player-melee-damage-artifact}.")
  :flavor "A weapon blessed by the vaguely defined divine personages that may or may not watch over you. What it lacks in lore, in makes up for in murderyness.\n\n    I came not to send peace, but a sword.")
