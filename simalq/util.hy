"Utility functions and other frequently imported components."


(import
  enum [Enum]
  simalq.game-state [G])


(defn seq [a b [step 1]]
  "A version of `range` that includes both ends (given a compatible
  step size)."
  (range a (+ b step) step))


(defclass ActionError [Exception]
  "Represents an attempt by the player to make an illegal move, such as walking through a wall.")

(defclass GameOverException [Exception]
  "Represents a game ending, whether by losing or winning.")


(defn player-melee-damage []
  "Return how much damage the player does with her sword."
  2)


(setv DamageType (Enum "DamageType" (list (map str '[
  PlayerMelee MonsterMelee
  MundaneArrow MagicArrow MonsterShot
  Poison Trap Fire DeathMagic]))))

(defn hurt-player [amount damage-type]
  (assert (isinstance amount int))
  (assert (isinstance damage-type DamageType))
    ; The type isn't really used yet.
  (-= G.player-hp amount)
  (when (<= G.player-hp 0)
    (raise (GameOverException 'dead))))
