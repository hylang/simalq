"Utility functions and other frequently imported components."

(require
  hyrule [unless]
  simalq.macros [pop-integer-part defmeth])
(import
  os
  pathlib [Path]
  time [sleep]
  enum [Enum]
  simalq.game-state [G]
  simalq.color :as colors)
(setv  T True  F False)


(setv platform-dirs (hy.I.platformdirs.PlatformDirs
  :appname "simalq" :appauthor "hylang"))
(setv cache-dir (Path platform-dirs.user-cache-dir))
(setv saved-games-dir (/ (Path platform-dirs.user-data-dir) "save"))


(defn mixed-number [fraction]
  (setv n (pop-integer-part fraction))
  (or
    (.join " " (lfor  x [n fraction]  :if x  (str x)))
    "0"))

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
  (when (hy.I.simalq/main.displaying)
    (.append message-queue (.join " " (map str args)))))


(defn flash-map [color ps labels flash-time-s]
  "Animate the map by briefly flashing some positions a certain color,
  optionally with a text label replacing the map character. `ps`
  should be an iterable of `Pos` to flash, and `labels` a dictionary
  mapping a subset of positions in `ps` to 2-character `str`s."

  (import
    simalq.main [print-main-screen displaying]
    simalq.display [ColorChar])

  (unless (and ps (displaying))
    (return))

  (print-main-screen
    :messages message-queue
    :overmarks (dfor  p ps  p (lfor  i (range 2)
      (ColorChar
        :char (when (in p labels) (get labels p i))
        :fg colors.flash-label
        :bg color
        :bold F))))
  (sleep flash-time-s))


(setv
  ; Letters assigned to menu items, in order. "q" is deliberately
  ; omitted, so it can be used for "quit".
  menu-letters "asdfzxcvwerghjklbnmtyuiop"
  menu-letters (+ menu-letters (.upper menu-letters)))


(defn player-melee-damage []
  "Return how much damage the player does with her sword."
  (-
    (if (get G.player.artifacts "Holy Sword")
      G.rules.player-melee-damage-artifact
      G.rules.player-melee-damage-base)
    (if (.player-has? StatusEffect.Weak)
      G.rules.player-melee-damage-weakness-reduction
      0)))

(defn player-shot-damage [magic?]
  "Return how much damage the player does with her bow."
  (cond
    magic?
      G.rules.player-shot-damage-magic
    (get G.player.artifacts "Elven Bow")
      G.rules.player-shot-damage-artifact
    T
      G.rules.player-shot-damage-base))


(defclass DamageType [Enum] (setv
  ; These only apply to damage against monsters.
  PlayerMelee   "sword attacks"
  MundaneArrow  "mundane arrows"
  MagicArrow    "magic arrows"
  Fire          "fire"
  ; These only apply to damage against the player.
  MonsterMelee  "monsters' melee attacks"
  MonsterShot   "monsters' shots"
  Trap          "traps"
  ; These are equal-opportunity.
  Poison        "poison"
  DeathMagic    "death magic"))


(defclass StatusEffect [Enum]
  (setv
    Para  "paralysis"
    Weak  "weakness"
    Ivln  "invulnerability"
    Ivis  "invisibility"
    Fast  "haste"
    Pois  "poisonous touch"
    Pass  "passwall"
    Prot  "protection"
    MKey  "magical key")

  (defmeth player-has? []
    "Does the player have this status effect?"
    (bool (get G.player.status-effects @)))

  (defmeth add [duration]
    "Give the player this status effect for the given duration."
    (when (and (@bad?) (.player-has? StatusEffect.Prot))
      ; Protection prevents harmful effects.
      (return))
    (+= (get G.player.status-effects @) duration))

  (defn [classmethod] disenchantable [cls]
    (lfor
      effect cls
      :if (not (.bad? effect))
      :if (not-in effect [StatusEffect.Prot StatusEffect.MKey])
        ; Protection itself prevents disenchantment.
        ; `MKey` is excluded somewhat arbitrarily per IQ.
      effect))

  (defn [classmethod] disenchant-player [cls]
    "Try to remove a beneficial status effect from the player. Return
    `True` if there was a removable beneficial effect; under
    protection, it still won't actually be removed."
    ; Remove the first eligible effect that the player actually has.
    (for [effect (.disenchantable cls)  :if (.player-has? effect)]
      (unless (.player-has? StatusEffect.Prot)
        (setv (get G.player.status-effects effect) 0))
      (return T))
    F)

  (defmeth bad? []
    (in @ [StatusEffect.Para StatusEffect.Weak])))


(setv hp-warning-threshold 100)

(defn refactor-hp [x]
  (int (round (* G.rules.player-hp-factor x))))


(defn burst-damage [
    center color amount damage-type [player-amount 0] [quick-flash F]]
  "Damage every tile in a burst. `amount` should be a list like `[3 2
  1]`, which means that 3 damage is dealt at the center, 2 damage is
  dealt at distance 1 from the center, and so on. But note that this
  damage is only dealt to non-player tiles. The player gets the
  separate number `player-amount`, regardless of distance.

  Return the result from `burst`."

  (import
    simalq.geometry [burst at dist]
    simalq.tile [Damageable])

  (setv b (tuple (burst center (- (len amount) 1))))
  (flash-map
    color
    :ps b
    :labels {}
    :flash-time-s (if quick-flash .125 .5))

  (for [p b  tile (at p)  :if (isinstance tile Damageable)]
    (.damage tile
      (if (is tile G.player) player-amount (get amount (dist center p)))
      damage-type))

  b)
