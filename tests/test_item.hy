(require
  hyrule [do-n ecase]
  tests.lib [cant wk])
(import
  tests.lib [init mk-quest assert-at assert-hp set-square mv-player shoot wait use-item mk-tile add-usable]
  simalq.geometry [Pos Direction at]
  simalq.game-state [G])
(setv  T True  F False)


(defn test-treasure []
  (init "Boot Camp 2")

  (mv-player 6 14)
  (assert (= G.score 0))
  (assert-at 'E "pile of gold")
  (wk E)
  (assert (= G.score 100))
  (assert-at 'here 'player)

  (mv-player 5 10)
  (assert-at 'N "handful of gems")
  (wk N)
  (assert (= G.score 350))
  (assert-at 'here 'player))


(defn test-key-get []
  (init "Boot Camp 2")
  (mv-player 13 10)
  (assert (and (= G.player.keys 0) (= G.score 0)))
  (wk S)
  (assert (and (= G.player.keys 1) (= G.score 50)))

  (init "Boot Camp 2")
  (mv-player 13 10)
  (setv G.player.keys G.rules.max-keys)
  (assert (and (= G.player.keys G.rules.max-keys) (= G.score 0)))
  (cant (wk S) "Your keyring has no room for another key.")
  (assert (and (= G.player.keys G.rules.max-keys) (= G.score 0)))

  ; Being maxed out on keys doesn't prevent you from unlocking a door
  ; on the same square.
  (init (mk-quest []))
  (set-square 'E "key" "locked door")
  (assert-at 'E ["key" "locked door"])
  (setv G.player.keys G.rules.max-keys)
  (wk E)
  (assert-at 'E ["key" "door"])
  (assert (= G.player.keys (- G.rules.max-keys 1)))
  (wk E)
  (assert-at 'here ['player "door"]))


(defn test-magic-arrow []
  (init (mk-quest [
    :map "
      ████████d ████
      @ ↑ o d ☠ ##G "
    :map-marks {
      "o " ["orc" :hp 4]
      "☠ " "jar of poison"
      "##" ["cracked wall" :hp 3]}]))

  ; Pick up some magic arrows.
  (assert (= G.player.magic-arrows 0))
  (wk E)
  (assert (= G.player.magic-arrows 10))
  (assert-at 'E "orc")
  (assert-hp 'E 4)
  ; Shoot at the orc. That does 3 damage to it. The devil is unhurt.
  (shoot 'E)
  (assert-at 'E "orc")
  (assert-hp 'E 1)
  (assert-at [3 0] "devil")
  ; Shoot again. This kills the orc, so the magic arrow penetrates
  ; and continues along the whole line, smashing the jar of poison
  ; and hitting the ghost. Unlike IQ, a magic arrow can continue
  ; after destroying an object.
  (shoot 'E)
  (assert-at 'E 'floor)
  (assert-at [3 0] 'floor)
  (assert-at [4 1] 'floor)
  (assert-at [6 0] 'floor))


(defn test-food-eat []
  (init (mk-quest [:tiles [
     "snack" "meal" "jar of poison" "rotten food" "empty platter"
     "dessert" "snack" "dessert"]]))
  (assert (= G.player.hp 100))
  (wk E)
  (assert (= G.player.hp 125))
  (wk E)
  (assert (= G.player.hp 225))
  (wk E)
  (assert (= G.player.hp 175))
  (wk E)
  (assert (= G.player.hp  75))
  (wk E)
  (assert (= G.player.hp  75))
  (wk E)
  (assert (= G.player.hp 500))
  (wk E)
  (assert (= G.player.hp 525))
  ; The second dessert has no effect, because we're already at 500 HP.
  (wk E)
  (assert (= G.player.hp 525)))


(defn test-food-shoot []
  (init (mk-quest [
    :map "
      @ % ☠ ██o o
      ████##██G ██
      ████o ██████"
    :map-marks {
      "% " "snack"
      "☠ " "jar of poison"
      "##" ["cracked wall" :hp 2]
      "o " ["orc" :hp 4]}]))
  ; Shooting a snack (or most other foods) just destroys it.
  (assert-at 'E "snack")
  (shoot 'E)
  (assert-at 'E 'floor)
  ; Shooting a jar of poison creates a 5 × 5 cloud of poison, which
  ; can damage both the player and monsters. It goes through walls.
  (wk E)
  (assert-at 'E "jar of poison")
  (assert (= G.player.hp 100))
  (shoot 'E)
  (assert-at 'E 'floor)
  ; The player takes 20 damage.
  (assert (= G.player.hp 80))
  ; The two orcs in the blast radius take 3 damage each.
  (assert-hp [2 0] 1)
  (assert-hp [4 2] 1)
  ; The ghost, being undead, takes no damage from poison.
  (assert-hp [4 1] 1)
  ; Cracked walls are also immune to poison.
  (assert-hp [2 1] 2)
  ; The third orc is outside of the blast radius.
  (assert-hp [5 2] 4)
  ; Points are earned for all damage dealt.
  (assert (= G.score (py "3*3 + 3*3"))))


(defn test-inventory []
  (init (mk-quest [:tiles [
     "wand of shielding" "wall-making wand"
     "standard bomb" "standard bomb"]]))

  (defn check [#* args]
    (assert (all (gfor
      [got expected] (zip G.player.inventory args)
      (if (is expected None) (is got None) (= got.stem expected))))))

  ; Inventory slots fill from the top.
  (check None None None)
  (cant (use-item 0) "That inventory slot is empty.")
  (wk E)
  (check "wand of shielding" None None)
  (cant (use-item 1) "That inventory slot is empty.")
  (wk E)
  (check "wand of shielding" "wall-making wand" None)
  (setv
    [(get G.player.inventory 0) (get G.player.inventory 2)]
    [(get G.player.inventory 2) (get G.player.inventory 0)])
  (check None "wall-making wand" "wand of shielding")
  (wk E)
  (check "standard bomb" "wall-making wand" "wand of shielding")
  (cant (wk E) "Your inventory is full.")
  (assert-at 'E "standard bomb")
  (assert-at 'W 'floor))


(defn test-wand-shielding []
  (init (mk-quest [
    :map "
      . o .
      ██. .
      $ @ ."
    :map-marks {
      "$ " "pile of gold"}]))
  (add-usable "wand of shielding" 2)

  ; A wand of shielding creates a shield on each adjacent square (even
  ; if something else is there, unlike IQ). Monsters can't move onto
  ; the shields.
  (use-item 0)
  (assert-at 'W ["magical energy shield" "pile of gold"])
  (assert-at 'NW ["magical energy shield" "wall"])
  (assert-at 'N "magical energy shield")
  (assert-at 'NE "magical energy shield")
  (assert-at 'E "magical energy shield")
  ; But Tris can walk onto them. Since this puts her on top of the
  ; shield tiles rather than under them, they no longer protect her.
  (assert (= G.player.hp 100))
  (wk N)
  (assert (= G.player.hp 97))

  ; Unlike IQ, creating new shields don't affect existing ones, which
  ; keep their own timers.
  (defn check [turn-n old new]
    (assert (= G.turn-n turn-n))
    (setv old (if old ["magical energy shield"] []))
    (setv new (if new ["magical energy shield"] []))
    (assert-at 'here ['player #* old])
    (assert-at 'NE [#* new])
    (assert-at 'E [#* old #* new]))
  (use-item 1)
  (check 3 T T)
  ; Shields protect for 12 turns, so the first set (created on turn 0,
  ; and therefore providing its first turn of protection on turn 0)
  ; disappates at the end of turn 11, soon before `G.turn-i` becomes
  ; 12.
  (wait 8)
  (check 11 T T)
  (wait)
  (check 12 F T)
  ; The second set disspiates just before turn 2 + 12 = 14.
  (wait)
  (check 13 F T)
  (wait)
  (check 14 F F))


(defn test-wand-wall-making []
  (init :max-usables 4 :quest (mk-quest
    [:tiles ["orc" "wall"]]))
  (add-usable "wall-making wand" 4)

  ; Unlike IQ, walls can be added regardless of what's already on the
  ; target square.
  (use-item 0  0 0)
  (assert-at [0 0] ["wall" 'player])
  (use-item 1  1 0)
  (assert-at [1 0] ["wall" "orc"])
  (use-item 2  2 0)
  (assert-at [2 0] ["wall" "wall"])
  (use-item 3  3 0)
  (assert-at [3 0] "wall"))


(defn test-fire-bomb []
  "Put some orcs in a line and check how much damage is done to each."

  (defn check [item-stem usage #* damages]
    (setv starting-orc-hp 10)
    (init (mk-quest [
      :height 1
      :tiles (lfor  _ damages  ["orc" :hp starting-orc-hp])]))
    (ecase usage
      'use (do
        ; Actually use the bomb.
        (add-usable item-stem)
        (use-item 0 1 0))
      'shoot (do
        ; Shoot the bomb while it's on the floor. This creates a
        ; less impressive explosion.
        (mk-tile [1 0] item-stem)
        (shoot 'E)))
     (for [[i dmg] (enumerate damages)]
       (assert-hp [(+ i 1) 0] (- starting-orc-hp dmg))))

  (check  "standard bomb" 'shoot  2 1 0 0 0 0 0)
  (check  "standard bomb" 'use    3 2 1 0 0 0 0)
  (check  "strong bomb"   'shoot  3 2 1 0 0 0 0)
  (check  "strong bomb"   'use    3 3 2 1 0 0 0)
  (check  "super-bomb"    'shoot  3 3 2 1 0 0 0)
  (check  "super-bomb"    'use    3 3 2 2 1 1 0))


(defn test-artifact-weapon []

  ; The sword artifact increases melee damage to 3. Multiple copies
  ; don't stack.
  (init (mk-quest [
    :height 1
    :tiles ["Holy Sword" "Holy Sword" ["orc" :hp 4]]]))
  (wk E 3)
  (assert-hp 'E 1)

  ; The bow artifact is similar, but increases ranged damage to 2.
  (init (mk-quest
    [:tiles ["Elven Bow" ["orc" :hp 10]]]))
  (wk E)
  (shoot 'E)
  (assert-hp 'E 8)
  ; The Elven Bow has no effect on magic arrows. They still do 3
  ; damage.
  (+= G.player.magic-arrows 10)
  (shoot 'E)
  (assert-hp 'E 5))


(defn test-artifact-shield []
  (init (mk-quest [
    :tiles ["Magic Shield" 'floor ["devil" :hp 1] "jar of poison"]]))

  ; The shield makes you take 3/4 damage, rounded up.
  ; A devil's shot normally does 10 damage, but now does
  ;     ceil(.75 * 10) = ceil(7.5) = 8.
  (assert (= G.player.hp 100))
  (wk E)
  (assert (= G.player.hp 92))
  ; The shield only protects against monster attacks, so the 20
  ; poison damage we take from shooting the jar of poison is
  ; unreduced.
  (shoot 'E)
  (wk E)
  (shoot 'E)
  (assert (= G.player.hp 72)))
