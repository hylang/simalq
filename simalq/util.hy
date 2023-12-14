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


(defn flash-map [focus color ps labels flash-time-s]
  "Animate the map by briefly flashing some positions a certain color,
  optionally with a text label replacing the map character. `focus`
  should be a `Pos`, `ps` an iterable of `Pos` to flash, and
  `labels` a dictionary mapping a subset of positions in `ps` to
  2-character `str`s."

  (import
    simalq.main [print-main-screen displaying]
    simalq.display [ColorChar])

  (unless (and ps (displaying))
    (return))

  (print-main-screen focus
    :status-bar T
    :messages message-queue
    :overmarks (dfor  p ps  p (lfor  i (range 2)
      (ColorChar
        :char (when (in p labels) (get labels p i))
        :fg colors.flash-label
        :bg color))))
  (sleep flash-time-s))


(setv
  ; Letters assigned to menu items, in order. "q" is deliberately
  ; omitted, so it can be used for "quit".
  menu-letters "asdfzxcvwerghjklbnmtyuiop"
  menu-letters (+ menu-letters (.upper menu-letters)))


(defn player-melee-damage []
  "Return how much damage the player does with her sword."
  (if (get G.player.artifacts "Holy Sword")
    G.rules.player-melee-damage-artifact
    G.rules.player-melee-damage-base))

(defn player-shot-damage [magic]
  "Return how much damage the player does with her bow."
  (cond
    magic
      G.rules.player-shot-damage-magic
    (get G.player.artifacts "Elven Bow")
      G.rules.player-shot-damage-artifact
    T
      G.rules.player-shot-damage-base))


(defclass DamageType [Enum] (setv
  ; These only apply to damage against monsters
  PlayerMelee   "sword attacks"
  MundaneArrow  "mundane arrows"
  MagicArrow    "magic arrows"
  Fire          "fire"
  ; These only apply to damage against the player
  MonsterMelee  "monsters' melee attacks"
  MonsterShot   "monsters' shots"
  Trap          "traps"
  ; These are equal-opportunity
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
    MKey  "magical key"
    Prot  "protection")
  (defmeth [property] bad []
    (in @ [StatusEffect.Para StatusEffect.Weak])))

(defn player-status [abbreviation]
  "Does the player have the given status effect?"
  (bool (get
    G.player.status-effects
    (getattr StatusEffect (str abbreviation)))))


(setv hp-warning-threshold 100)

(defn refactor-hp [x]
  (int (round (* G.rules.player-hp-factor x))))


(defn burst-damage [
    center color amount damage-type [player-amount 0]]
  "Damage every tile in a burst. `amount` should be a list like `[3 2
  1]`, which means that 3 damage is dealt at the center, 2 damage is
  dealt at distance 1 from the center, and so on. But note that this
  damage is only dealt to non-player tiles. The player gets the
  separate number `player-amount`, regardless of distance."

  (import
    simalq.geometry [burst at dist]
    simalq.tile [Damageable])

  (setv b (tuple (burst center (- (len amount) 1))))
  (flash-map
    G.player.pos
    color
    :ps b
    :labels {}
    :flash-time-s .5)

  (for [p b  tile (at p)  :if (isinstance tile Damageable)]
    (.damage tile
      (if (is tile G.player) player-amount (get amount (dist center p)))
      damage-type)))
