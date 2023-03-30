"Utility functions and other frequently imported components."


(import
  enum [Enum]
  simalq.game-state [G])


(defclass ActionError [Exception]
  "Represents an attempt by the player to make an illegal move, such as walking through a wall.")

(defclass GameOverException [Exception]
  "Represents a game ending, whether by losing or winning.")


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
