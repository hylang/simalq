(require
  simalq.macros [defn-dd fn-dd])
(import
  simalq.strings
  simalq.util [CommandError]
  simalq.game-state [G]
  simalq.tile [Tile deftile destroy-tile]
  simalq.util [DamageType hurt-player msg])


(defclass Item [Tile]
  "An object the player can pick up."

  (setv __slots__ [])

  (defn hook-player-walked-into [self]
    (+= G.score self.points)
    (.pick-up self)
    (destroy-tile self))
  (defn pick-up [self])

  (defn info-bullets [self] [
    #("Point value" self.points)
    (when (is-not (. (type self) pick-up) Item.pick-up)
      #("Pickup effect" (or
        self.pick-up.__doc__
        (self.pick-up.dynadoc self))))]))


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
    eat-messages #())
      ; Messages that can print when you eat the food. A pseudorandom
      ; one is chosen.

  (defn-dd pick-up [self]
    (doc (if (< it.hp-effect 0)
      f"Deals {(- it.hp-effect)} poison damage to you."
      f"Grants {it.hp-effect} hit points."))
    (msg (get self.eat-messages (%
      ; Food at the same position on the same level number will have
      ; the same message. Nearby pieces of food will typically have
      ; different messages.
      (+ (* G.level-n 10,000) self.pos.x (* G.map.width self.pos.y))
      (len self.eat-messages))))
    (if (< self.hp-effect 0)
      (hurt-player (- self.hp-effect) DamageType.Poison)
      (+= G.player.hp self.hp-effect))))

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

(deftile Food "☠ " "a jar of poison"
  :iq-ix 86
  :points 0

  :hp-effect -50
  :eat-messages #("You drink a jar of poison. It tastes pretty bad.")

  :flavor "I think you're not supposed to drink this.")


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
