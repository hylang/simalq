"Utility functions and other frequently imported components."


(import
  os
  pathlib [Path]
  enum [Enum]
  simalq.game-state [G])


(setv save-game-path (/
  (Path (get os.environ "XDG_DATA_HOME")) "simalq" "save.pklz"))


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



(setv message-queue [])

(defn msg [#* args]
  (.append message-queue (.join " " (map str args))))


(defn player-melee-damage []
  "Return how much damage the player does with her sword."
  G.rules.base-player-melee-damage)

(defn player-shot-damage []
  "Return how much damage the player does with her bow."
  G.rules.base-player-shot-damage)


(setv DamageType (Enum "DamageType" (list (map str '[
  PlayerMelee MonsterMelee
  MundaneArrow MagicArrow MonsterShot
  Poison Trap Fire DeathMagic]))))


(setv hp-warning-threshold 100)

(defn hurt-player [amount damage-type]
  (setv hp-was G.player.hp)
  (hy.M.simalq/tile.damage-tile G.player amount damage-type)
  (when (chainc G.player.hp <= hp-warning-threshold < hp-was)
    (msg "Princess needs food badly!")))


(defn burst-damage [center size amount damage-type [player-amount 0]]
  "Damage every tile in the specified burst. Notice that `amount`
  damage is only done to non-player tiles. The player gets the
  separate value `player-amount`."
  (import
    simalq.geometry [burst at]
    simalq.tile [damage-tile])

  (for [p (burst center size)  tile (at p)  :if tile.damageable]
    (if (is tile G.player)
      (hurt-player player-amount damage-type)
      (damage-tile tile amount damage-type))))
