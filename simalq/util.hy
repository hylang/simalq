"Utility functions and other frequently imported components."


(import
  enum [Enum]
  simalq.game-state [G])


(defn seq [a b [step 1]]
  "A version of `range` that includes both ends (given a compatible
  step size)."
  (range a (+ b step) step))

(defn sign [x]
  (cond
    (< x 0) -1
    (> x 0)  1
    (= x 0)  0
    True     (raise TypeError)))

(defn next-in-cycle [sequence e]
  (when (is e None)
    (return (get sequence 0)))
  (setv i (+ 1 (.index sequence e)))
  (get sequence (if (= i (len sequence)) 0 i)))


(defclass CommandError [Exception]
  "Represents a failure to execute the player's command.")

(defclass GameOverException [Exception]
  "Represents a game ending, whether by losing or winning.")


(defn player-melee-damage []
  "Return how much damage the player does with her sword."
  G.rules.base-player-melee-damage)


(setv DamageType (Enum "DamageType" (list (map str '[
  PlayerMelee MonsterMelee
  MundaneArrow MagicArrow MonsterShot
  Poison Trap Fire DeathMagic]))))

(defn hurt-player [amount damage-type]
  (hy.M.simalq/tile.damage-tile G.player amount damage-type))
