"Utility functions and other frequently imported components."

(require
  hyrule [unless]
  simalq.macros [pop-integer-part])
(import
  os
  pathlib [Path]
  time [sleep]
  enum [Enum]
  simalq.game-state [G]
  simalq.color :as colors)
(setv  T True  F False)


(assert (get os.environ "XDG_CACHE_HOME"))
(setv cache-dir (/ (Path (get os.environ "XDG_CACHE_HOME")) "simalq"))
(assert (get os.environ "XDG_DATA_HOME"))
(setv saved-games-dir (/
  (Path (get os.environ "XDG_DATA_HOME")) "simalq" "save"))


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
  (when (hy.M.simalq/main.displaying)
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
  (defn [property] bad [self]
    (in self [StatusEffect.Para StatusEffect.Weak])))

(defn player-status [abbreviation]
  "Does the player have the given status effect?"
  (bool (get
    G.player.status-effects
    (getattr StatusEffect (str abbreviation)))))


(setv hp-warning-threshold 100)

(defn hurt-player [amount damage-type [animate T] [attacker None]]
  (import simalq.geometry [ray dir-to dist])

  (unless amount
    (return))

  (when (and
      (get G.player.artifacts "Magic Shield")
      (in damage-type #(DamageType.MonsterMelee DamageType.MonsterShot)))
    (setv amount (int (.__ceil__
      (* G.rules.artifact-shield-factor amount)))))

  (when animate
    (flash-map
      G.player.pos
      colors.flash-player-damaged
      (+
        (if (and attacker attacker.pos)
          (ray G.player.pos
            (dir-to G.player.pos attacker.pos)
            (dist G.player.pos attacker.pos))
          #())
        (if (player-status 'Ivln)
          #()
          #(G.player.pos)))
      {G.player.pos (if (> amount 99) "OW" (format amount "2"))}
      :flash-time-s .2))

  (setv hp-was G.player.hp)
  (unless (player-status 'Ivln)
    (hy.M.simalq/tile.damage-tile G.player amount damage-type))
  (when (chainc G.player.hp <= hp-warning-threshold < hp-was)
    (msg "Princess needs food badly!")))


(defn burst-damage [
    center color amount damage-type [player-amount 0]]
  "Damage every tile in a burst. `amount` should be a list like `[3 2
  1]`, which means that 3 damage is dealt at the center, 2 damage is
  dealt at distance 1 from the center, and so on. But note that this
  damage is only dealt to non-player tiles. The player gets the
  separate number `player-amount`, regardless of distance."

  (import
    simalq.geometry [burst at dist]
    simalq.tile [damage-tile])

  (setv b (tuple (burst center (- (len amount) 1))))
  (flash-map
    G.player.pos
    color
    :ps b
    :labels {}
    :flash-time-s .5)

  (for [p b  tile (at p)  :if tile.damageable]
    (if (is tile G.player)
      (hurt-player player-amount damage-type)
      (damage-tile
        tile
        (get amount (dist center p))
        damage-type))))
