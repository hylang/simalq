(require
  hyrule [do-n])
(import
  collections [Counter]
  fractions [Fraction :as f/]
  hyrule [thru]
  tests.lib [init assert-at assert-full-name assert-hp assert-textmap wait set-square wk shoot mv-player use-item top]
  simalq.util [StatusEffect]
  simalq.geometry [Direction Pos ray at burst]
  simalq.game-state [G])
(setv  T True  F False)


(defn test-simple-full-name []
  (init
    [:tiles [["orc" :hp 3]]])
  (assert-full-name 'E "an orc (HP 3)"))


(defn test-simple-melee-combat []
  (init
    [:tiles [["Dark Knight" :hp 5]]])
  (defn check [turn score tris mon]
    (assert (and (= G.turn-n turn) (= G.score score)))
    (assert (= G.player.hp tris))
    (if (is mon None)
      (assert-at 'E 'floor)
      (assert-hp 'E mon)))

  (check :turn 0 :score 0 :tris 100 :mon 5)
  ; Attack the monster, doing 2 damage. We get hit for 12 damage.
  (wk 'E)
  (check :turn 1 :score 0 :tris 88 :mon 3)
  ; And again.
  (wk 'E)
  (check :turn 2 :score 0 :tris 76 :mon 1)
  (assert-at 'E "Dark Knight")
    ; The monster's still there.
  ; Finish the monster off. We take no damage this time.
  (wk 'E)
  (check :turn 3 :score 75 :tris 76 :mon None))


(defn test-monster-melee-diag []

  ; A monster can melee-attack diagonally around a column.
  (init
    [:map "
      | @
      o ."])
  (assert (= G.player.hp 100))
  (assert-at 'SW "orc")
  (wait)
  (assert (= G.player.hp 97))
  (assert-at 'SW "orc")

  ; But not a wall. So the orc will use its first turn to walk east.
  (init
    [:map "
      ██@
      o ."])
  (assert (= G.player.hp 100))
  (assert-at 'SW "orc")
  (wait)
  (assert (= G.player.hp 100))
  (assert-at 'S "orc"))


(defn test-approach []

  ; A monster can approach in a straight line.
  (init
    [:tiles ['floor 'floor 'floor "orc"]])
  (assert-at 'E 'floor)
  (wait 2)
  (assert-at 'E 'floor)
  (wait)
  (assert-at 'E "orc")
  (assert (= G.player.hp 100))
  ; Now if Tris lollygags for another turn, he can hit her. Owie.
  (wait)
  (assert (= G.player.hp 97))

  ; A monster stymied when trying to go west will first try northwest.
  (init
    [:tiles ["pillar" "orc"]])
  (wait)
  (assert-at 'NE "orc")

  ; A monster stymied going west *and* northwest will take 3 turns
  ; to finally try southwest.
  (init
    [:map "
      . | .
      @ | o
      . . ."])
  (do-n 3
    (assert-at 'SE 'floor)
    (wait))
  (assert-at 'SE "orc")

  ; When the monster is blocked W, NW, and SW, he'll just sit there
  ; forever.
  (init
    [:map "
      . . .
      @ ██o
      . . ."])
    ; A wall, unlike a pillar, blocks diagonal movement.
  (assert-at [2 1] "orc")
  (wait 100)
  (assert-at [2 1] "orc")

  ; A monster outside the reality bubble can't move.
  (setv r G.rules.reality-bubble-size)
  (init [:tiles [
    #* (* ['floor] r)
    "orc"]])
  (assert-at [(+ r 1) 0] "orc")
  (wait 100)
  (assert-at [(+ r 1) 0] "orc")
  (wk 'E)
  (assert-at [r 0] "orc")

  ; A monster can chase Tris around a wrapped map.
  ; (Accompanied by "Yakety Sax", one imagines.)
  (init [
    :player-start #(4 4)
    :width 9 :height 9 :wrap-y T])
  (set-square 'N "orc")
  (do-n 100
    (wk 'S)
    (assert-at 'N "orc")))


(defn test-approach-march []
  "Lines of monsters can march towards the player without losing
  turns or going astray by blocking each other. This is an important
  property of the spiral activation order of monsters that we inherit
  from IQ."

  (init
    [:player-start #(8 8)])

  (defn arms []
    (lfor
      direction Direction.all
      (ray G.player.pos direction 4)))

  (for [arm (arms)  p (cut arm 1 None)]
    (set-square p "orc"))
  (assert (=
    (sfor  arm (arms)  (tuple (gfor  p arm  (len (at p)))))
    #{#(0 1 1 1)}))
  (wait)
  (assert (=
    (sfor  arm (arms)  (tuple (gfor  p arm  (len (at p)))))
    #{#(1 1 1 0)})))


(defn test-nondainty []
  (for [dainty [F T]]
    (init
      [:height 1 :tiles ["pile of gold" "orc" "wall" "orc" "orc"]])
    (when (not dainty)
      (setv G.rules.dainty-monsters F))

    (if dainty
      ; By default, normal monsters will only step on plain floor.
      (do
        (wait 10)
        (assert-at [2 0] "orc"))
      ; If dainty-monsters mode is off, they can step on e.g. items.
      (do
        (wait 1)
        (assert-at 'E "orc" "pile of gold")))

    ; Either way, monsters block each other.
    (assert-at [4 0] "orc")
    (assert-at [5 0] "orc")))


(defn test-orc-or-goblin []
  (init
    [:tiles [["orc" :hp 3]]])

  (assert-at 'E "orc")
  ; Get stabbed.
  (wait)
  (assert (and (= G.player.hp 91) (= G.score 0)))
  ; Hit the orc. It survives, but it now does less damage,
  ; and we get some points.
  (wk 'E)
  (assert (and (= G.player.hp 88) (= G.score 6)))
  ; Kill it. We only get 3 points because it only has 1 more HP to
  ; take off.
  (wk 'E)
  (assert (and (= G.player.hp 88) (= G.score 9)))
  (assert-at 'E 'floor))


(defn test-generated-high-hp []
  "A generated monster with more than 3 HP does damage as if it had
  3 HP."

  (init
    [:tiles [["orc" :hp 10]]])

  (for [[orc-hp score player-hp] [
      [10 0 100]
      [ 8 6  91]
      [ 6 12 82]
      [ 4 18 73]
      [ 2 24 67]
      [ 0 30 67]]]
    (assert (= G.player.hp player-hp))
    (assert (= G.score score))
    (if (= orc-hp 0)
      (assert-at [1 0] 'floor)
      (assert-hp [1 0] orc-hp))
    (wk 'E)))


(defn test-generator []
  (init [
    :map "
      @ ████. .
      ████. G .
      ████████. "
    :map-marks {
      "G " ["generator"
        :summon-class "orc"
        :summon-frequency (f/ 2 3)
        :summon-hp 2]}])
  (assert-full-name [3 1] "an orc generator (HP 1, pw 0, freq 2/3, sHP 2)")

  (defn check [power tN tNE tE tSE tW]
    (assert (= (top [3 1] 'summon-power) power))
    (assert-at [3 2] tN)
    (assert-at [4 2] tNE)
    (assert-at [4 1] tE)
    (assert-at [4 0] tSE)
    (assert-at [2 1] tW))

  None      (check (f/ 0 1) 'floor 'floor 'floor 'floor 'floor)
  (wait)    (check (f/ 2 3) 'floor 'floor 'floor 'floor 'floor)
  (wait)    (check (f/ 1 3) "orc"  'floor 'floor 'floor 'floor)
  (wait)    (check (f/ 0 1) "orc"  "orc"  'floor 'floor 'floor)
  (wait)    (check (f/ 2 3) "orc"  "orc"  'floor 'floor 'floor)
  (wait)    (check (f/ 1 3) "orc"  "orc"  "orc"  'floor 'floor)
  (wait)    (check (f/ 0 3) "orc"  "orc"  "orc"  "orc"  'floor)
  (wait)    (check (f/ 2 3) "orc"  "orc"  "orc"  "orc"  'floor)
  (wait)    (check (f/ 1 3) "orc"  "orc"  "orc"  "orc"  "orc")
  (wait)    (check (f/ 0 1) "orc"  "orc"  "orc"  "orc"  "orc")
  (wait 30) (check (f/ 0 1) "orc"  "orc"  "orc"  "orc"  "orc"))


(defn test-generator-reality-bubble []
  (init [
    :height 1
    :tiles [
      'floor 'floor "wall" 'floor
      ["generator" :summon-class "orc" :summon-frequency (f/ 1)]
      'floor]])
  (setv G.rules.reality-bubble-size 4)

  ; Outside the reality bubble, generators do nothing.
  (wait 10)
  (assert-at [4 0] 'floor)
  (assert-at [6 0] 'floor)
  ; Once in it, they can spawn monsters into adjacent squares even
  ; if those squares aren't in the reality bubble themselves.
  (wk 'E)
  (wait 2)
  (assert-at [4 0] "orc")
  (assert-at [6 0] "orc"))


(defn test-generated-first-turn []
  (for [gen-west [T F]]
    ; Try a case with the generator nearer to the player than the
    ; monster (`gen-west T`) and farther (`gen-west F`).

    (init [
      :map "
        . . $ $
        @ . a b
        . . $ $"
      :map-marks {
        (if gen-west "a " "b ") ["generator"
          :summon-class "orc"
          :summon-frequency (f/ 1 4)]
        (if gen-west "b " "a ") 'floor}])
    (setv G.rules.dainty-monsters F)

    (defn check [orc-at-p1 orc-at-p2]
      (setv p1 (if gen-west (Pos G.map 3 1) (Pos G.map 2 1)))
        ; The orc's generated position.
      (setv p2 (if gen-west (Pos G.map 2 2) (Pos G.map 1 1)))
        ; The orc's first move.
      (assert (and
        (= (any (gfor  x (at p1)  (= x.stem "orc"))) orc-at-p1)
        (= (any (gfor  x (at p2)  (= x.stem "orc"))) orc-at-p2))))

    (wait 3)
    (check F F)
    ; Monsters don't get to act on the turn that they're generated.
    (wait)
    (check T F)
    ; They do get to act on the next turn.
    (wait)
    (check F T)))


(defn test-ghost []
  (init
    [:tiles [["ghost" :hp 3]]])
  (assert (and (= G.player.hp 100) (= G.score 0)))
  (assert-at 'E "ghost")
  ; Attack the ghost. We get 10 points for doing 2 damage.
  ; The ghost strikes back, dying by kamikaze, which grants no points.
  (wk 'E)
  (assert (and (= G.player.hp 95) (= G.score 10)))
  (assert-at 'E 'floor))


(defn test-bat-or-bee []

  ; A bat that's next to Tris just chews on her instead of moving.
  (init
    [:tiles ["bat"]])
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 99))
  (wait)
  (assert (= G.player.hp 98))
  (wait)
  (assert (= G.player.hp 97))

  ; A wandering bat will eventually cover the reality bubble, if
  ; it can't attack the player. It can't leave the reality bubble
  ; on its own.
  (init
    [:map "
        . . . . .
        . . b . .
        ██. . . .
        @ ██. . ."])
  (setv G.rules.reality-bubble-size 3)
  (assert (= G.player.hp 100))
  (setv seen-at (dfor
    x (range G.map.width)
    y (range G.map.height)
    #(x y) F))
  (do-n 200
    (wait)
    (for [[x y] (list (.keys seen-at))]
      (when (any (gfor  tile (at (Pos G.map x y))  (= tile.stem "bat")))
        (setv (get seen-at #(x y)) T)
        (break))))
  (assert (= G.player.hp 100))
  (assert (all (gfor
    [[x y] seen] (.items seen-at)
    (= seen (and (!= x 4) (not-in [x y] [[0 0] [1 0] [0 1]])))))))


(defn test-devil []

  ; At range, a devil shoots for 10 damage.
  (init
    [:tiles ['floor "devil"]])
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 90))
  ; Close up, a 1-HP devil melees for 3 damage.
  (wk 'E)
  (assert (= G.player.hp 87))

  ; A diagonally blocked devil can't melee, so it shoots.
  (init
    [:map "
      . d
      @ ██"])
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 90))

  ; Monster shots can wrap around.
  (init [:wrap-x T
    :map ". @ ████████d ."])
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 90))

  ; However, monsters only consider one direction for shooting. If the
  ; shortest path to the player (as the xorn phases) is blocked, they
  ; won't consider another valid direction.
  (init [:wrap-x T
    :map ". . @ ██d . ."])
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 100))
  ; The player can shoot in that other direction.
  (assert-at [4 0] "devil")
  (shoot 'W)
  (assert-at [4 0] 'floor))


(defn test-wizard []
  (init
    [:player-start #(5 0) :tiles [["wizard" :hp 3]]])

  (assert (= G.player.hp 100))
  ; Wizards always melee for 4 damage.
  (wait)
  (assert (= G.player.hp 96))
  ; A 3-HP wizard shoots for 12 damage.
  (wk 'W)
  (assert (= G.player.hp 84))
  ; At 2 HP, its shot damage drops to 8.
  (shoot 'E)
  (assert (= G.player.hp 76)))


(defn test-imp []

  (init
    [:height 1 :tiles ["wall" "wall" "imp"]])
  (assert (= G.player.hp 100))
  ; An imp initially needs to gain some shot power before it can
  ; shoot.
  (wait)
  (assert (= G.player.hp 100))
  ; Now it can shoot (and is left with 3/5 power). Walls are no
  ; obstacle to imp shots.
  (wait)
  (assert (= G.player.hp 99))
  (assert (= (top [3 0] 'shot-power) (f/ 3 5)))
  ; The void blocks imp shots. Since the imp can't shoot, it gains
  ; no shot power.
  (set-square [2 0] "Void")
  (wait)
  (assert (= G.player.hp 99))
  (assert (= (top [3 0] 'shot-power) (f/ 3 5)))
  ; At ranges of 2 or less, an imp flees.
  (mv-player 1 0)
  (wait)
  (assert-at [4 0] "imp")

  ; On a turn that an imp isn't shooting or fleeing (even if it's
  ; gaining shot power), it wanders.
  (init
    [:map "
      . . . . .
      @ . . i .
      . . . . ."])
  (defn imp []
    (setv [mon] (gfor
      col G.map.data
      stack col
      t stack
      :if (= t.stem "imp")
      t))
    mon)
  ; We have to test this by peeking at the imp's internal state
  ; because staying in place is a possible wandering step.
  (assert (is (. (imp) wander-state) None))
  (wait)
  (assert (is-not (. (imp) wander-state) None)))


(defn test-thorn-tree []
  (init
    [:tiles ['floor ["thorn tree" :hp 3] ["thorn tree" :hp 10]]])

  ; Thorn trees are immobile.
  (assert-at [2 0] "thorn tree")
  (wait)
  (assert-at [2 0] "thorn tree")
  ; They're immune to arrows.
  (assert-hp [2 0] 3)
  (shoot 'E)
  (assert-hp [2 0] 3)
  ; Up close, they scratch for 4 damage.
  (assert (= G.player.hp 100))
  (wk 'E)
  (assert (= G.player.hp 96))
  ; They're damaged normally by Tris's sword.
  (assert-hp [2 0] 3)
  (wk 'E)
  (assert-hp [2 0] 1)
  ; They're weak against fire: if they take 1 or more fire damage,
  ; they die instantly.
  (assert-at [2 0] "thorn tree")
  (assert-at [3 0] "thorn tree")
  (use-item "standard bomb" 'here)
  (assert-at [2 0] 'floor)
  (assert-at [3 0] 'floor))


(defn test-generator-thorn-tree []
  "Test a generator of a monster type that isn't allowed in IQ
  (namely, thorn trees)."

  (init
    [:tiles [["generator" :hp 3 :summon-class "thorn tree"]]])
  ; Generators inherit the immunities of the monster type they
  ; generate, so a thorn-tree generator is immune to arrows.
  (shoot 'E)
  (assert-hp 'E 3)
  ; They also inherit whether you get points for hitting them or
  ; destroying them. When IQ doesn't specify a point value, the
  ; point value is quadruple the monster's.
  (wk 'E)
  (assert-hp 'E 1)
  (assert (= G.score 0))
  (wk 'E)
  (assert (= G.score (* 4 10)))

  (setv shp 7)
  (init [
    :map "
      . . . . . .
      . . . . . .
      @ . . G . .
      . . . . . .
      . . . . . ."
    :map-marks {
      "G " ["generator"
        :hp 3
        :summon-class "thorn tree"
        :summon-frequency (f/ 1)
        :summon-hp shp]}])
  ; Check the full name.
  (assert-full-name [3 2]
    f"a thorn-tree generator (HP 3, pw 0, freq 1, sHP {shp})")
  ; Check that the generator produces a ring of trees.
  (wait 9)
  (for [p (burst (Pos G.map 3 2) 1 :exclude-center T)]
    (assert-at p "thorn tree")
    (assert-hp p shp)))


(defn test-tricorn []
  (init
    [:tiles ['floor 'floor 'floor "Tricorn"]])

  ; Tricorns can't shoot from more than 3 squares away. When they
  ; can't shoot, they approach as normal.
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 94)))


(defn test-death []
  (init
    [:tiles [["Death" :hp 10]]])

  (assert-hp 'E 10)
  ; Deaths are immune to mundane arrows.
  (shoot 'E)
  (assert-hp 'E 10)
  ; They take only 1 damage from magic arrows.
  (+= G.player.magic-arrows 10)
  (shoot 'E)
  (assert-hp 'E 9)
  ; They take normal damage from sword attacks.
  (wk 'E)
  (assert-hp 'E 7))


(defn test-floater []

  (init [
    :height 1
    :tiles ["wall" "floater" "wall"]])
  ; If a floater activates while adjacent to us, we gain 1/5 floater
  ; disturbance.
  (assert (= G.player.floater-disturbance (f/ 0)))
  (wait)
  (assert (= G.player.floater-disturbance (f/ 0)))
  (mv-player 1 0)
  (wait)
  (assert (= G.player.floater-disturbance (f/ 1 5)))
  (wait)
  (assert (= G.player.floater-disturbance (f/ 2 5)))
  ; Hit the floater. It explodes and we take 10 damage.
  (assert (= G.player.hp 100))
  (wk 'E)
  (assert (= G.player.hp 90))
  (assert-at 'E 'floor)
  (assert (= G.player.floater-disturbance (f/ 2 5)))
  ; Waiting next to another floater increases disturbance some more.
  (set-square 'E "floater")
  (wait)
  (assert (= G.player.floater-disturbance (f/ 3 5)))
  (wait)
  (assert (= G.player.floater-disturbance (f/ 4 5)))
  ; When disturbance reaches 1, it's cleared and the floater explodes
  ; spontaneously.
  (wait)
  (assert (= G.player.hp 80))
  (assert-at 'E 'floor)
  (assert (= G.player.floater-disturbance (f/ 0)))

  ; Contra IQ, floaters can explode on death when killed with e.g.
  ; poison.
  (init [
    :map "
      ██f ██.
      @ > . ☠"
    :map-marks {
      "☠ " "jar of poison"}])
  (assert (= G.player.hp 100))
  (assert-at 'NE "floater")
  (assert-at [3 0] "jar of poison")
  ; Shoot the jar of poison, killing the floater.
  (shoot 'E)
  (assert (= G.player.hp 90))
  (assert-at 'NE 'floor)
  (assert-at [3 0] 'floor)

  ; But instakills, like a wand of death, prevent the explosion.
  (init [
    :height 1
    :tiles ["floater" "wall"]])
  (assert-at 'E "floater")
  (assert (= G.player.hp 100))
  (use-item "wand of death" [3 0])
  (assert-at 'E 'floor)
  (assert (= G.player.hp 100)))


(defn test-blob []
  (init [
    :map "
      . ████.
      @ ██O ██"
    :map-marks {
      "O " ["blob" :hp 5]}])

  (wait 9)
  (assert-full-name [2 0] f"a blob (HP 5, wd ....., pw 9/10)")
  (assert-at [3 1] 'floor)
  ; After 10 turns, the blob splits. The child has half the parent's
  ; HP, rounded down, and the parent's HP is reduced by this number.
  (wait)
  (assert-full-name [2 0] f"a blob (HP 3, wd ....., pw 0)")
  (assert-full-name [3 1] f"a blob (HP 2, wd ....., pw 0)"))


(defn test-gunk []

  (setv marks {
    "O " "gunk"
    "s " "gunk seed"})
  (init [
    :map "
      O . . .
      ████████
      @ ██s . "
    :map-marks marks])
  ; Gunks take 5 turns to reproduce, and gunk seeds take 5 turns to
  ; grow up.
  (wait 4)
  (assert-textmap :map-marks marks :text "
    O . . .
    ████████
    @ ██s . ")
  (wait)
  (assert-textmap :map-marks marks :text "
    O s . .
    ████████
    @ ██O . ")
  ; The timing continues to all line up when gunks that are continuously in
  ; the reality bubble grow or reproduce.
  (wait 4)
  (assert-textmap :map-marks marks :text "
    O s . .
    ████████
    @ ██O . ")
  (wait)
  (assert-textmap :map-marks marks :text "
    O O . .
    ████████
    @ ██O s ")
  ; A slain adult gunk leaves behind a seed.
  (use-item "standard bomb" [1 1])
  (assert-textmap :map-marks marks :text "
    s s . .
    ████████
    @ ██s . ")

  ; Test for a bug where the game tried to set an attribute on the
  ; wrong tile when there's more than one tile in a stack.
  (init [])
  (set-square 'E "gunk seed" "wall")
  (wait 5)
  (assert-at 'E "gunk" "wall"))


(defn test-specter []

  (defn check [turns #* middle-tiles]
    (init [
      :height 1
      :tiles ['floor #* middle-tiles "specter"]])
    (if (= turns Inf)
      (do
        (wait 100)
        (assert-at [(+ (len middle-tiles) 2) 0] "specter"))
      (do
        (wait turns)
        (assert-at 'E "specter"))))
  ; Normally, specters move one square at a time, like any other
  ; monster.
  (check 2    'floor)
  ; They can also jump over blocked squares, in which case they
  ; move two squares in one turn.
  (check 1    "wall")
  ; They can jump over items or other monsters.
  (check 1    "pile of gold")
  (check 1    "thorn tree")
  ; They can't jump over void. (IQ documents this, but doesn't
  ; implement it.)
  (check Inf  "Void")
  ; Nor over two blocked squares in a row.
  (check Inf  "wall" "wall")

  ; Test for a bug where a specter's movement state cycles too quickly
  ; to allow jumping east here.
  (init [
    :map "
       S ██.
       ████@"
    :map-marks {
      "S " "specter"}])
  (wait)
  (assert-at 'N "specter"))


(defn test-spider []

  (init
    [:tiles ['floor "web" "giant spider"]])
  ; A spider can walk onto webs.
  (wk 'E)
  (assert-at 'E "giant spider" "web")
  ; It also creates a web if it steps onto a tile without one.
  (wk 'W)
  (assert-at 'E "giant spider" "web")
  ; The web is created where it goes to, not where it came from.
  (assert-at [3 0] 'floor)

  ; A spider that doesn't move during its turn can still create a web.
  (init
    [:tiles ["wall" "giant spider"]])
  (assert-at [2 0] "giant spider")
  (wait)
  (assert-at [2 0] "giant spider" "web"))


(defn test-turret []
  (init
    [:tiles ["turret"]])

  ; Turrets are immune to a lot of things.
  (wk 'E)
  (shoot 'E)
  (assert-at 'E "turret")
  (assert (= G.player.hp 80))

  ; In IQ, fire mages won't attack an invisible Argonn even if he's
  ; adjacent. This is probably a bug.
  (set-square 'N "cloak of invisibility")
  (wk 'N)
  (assert (= G.player.hp 70))

  ; Contra IQ, killing a turret with a wand of death scores points.
  (setv G.score 0)
  (use-item "wand of death" 'here)
  (assert-at 'NE 'floor)
  (assert (= G.score 150)))


(defn test-teleporting-mage []
  (defn check [text-map]
    (init [:map text-map :map-marks {
      "1 " "teleporting mage"
      "2 " 'floor}])
    (wait)
    (assert-textmap text-map :map-marks {
      "1 " 'floor
      "2 " "teleporting mage"}))

  ; Teleporting mages prefer to be distance 2 to the north.
  (check "
    . . . . . . .
    . . . 2 . . .
    . . . . . . .
    . . . @ . . .
    . 1 . . . . .
    . . . . . . .
    . . . . . . .")
  ; They'll try farther distances if they can't get distance 2.
  (check "
    . . . . . . .
    . ██. ██. ██.
    . . . . . . .
    . ██. @ . ██.
    . 1 . . . . .
    . ██. ██. ◀▶.
    . . . . . . 2")
  ; And adjacency if they can't get farther.
  (check "
    . . . . . . .
    . ██. ██. ██.
    . . . ◀▶◀▶. .
    . ██2 @ ◀▶██.
    . 1 ◀▶◀▶◀▶. .
    . ██. ██. ██.
    . . . . . . ."))


(defn test-archmage []

  (init [:tiles [
    "archmage"
    'floor
    "cloak of invisibility"
    "cloak of invisibility"]])
  (mv-player 2 0)
  (assert (= G.player.hp 100))
  ; The shot of an archmage can disenchant, in which case it does no
  ; damage.
  (wk 'E)
  (assert (not (.player-has? StatusEffect.Ivis)))
  (assert (= G.player.hp 100))
  ; Without disenchantment, it does 12 damage.
  (wait)
  (assert (= G.player.hp 88))
  ; The melee attack of an archmage always does 2 damage and never
  ; disenchants.
  (set-square [1 0])
  (wk 'E)
  (set-square 'E "archmage")
  (wait)
  (assert (.player-has? StatusEffect.Ivis))
  (assert (= G.player.hp 86))

  ; Test for a bug where a diagonally blocked but adjacent archmage
  ; didn't disenchant.
  (init [
    :map "
       ██W
       . ██
       ! ██
       @ ██"
    :map-marks {
      "W " "archmage"
      "! " "cloak of invisibility"}])
  (wk 'N)
  (assert (.player-has? StatusEffect.Ivis))
  (wk 'N)
  (assert (not (.player-has? StatusEffect.Ivis)))
  (assert (= G.player.hp 100)))


(defn test-ant []
  (init [
    :tiles ["giant ant"]])

  ; Ants paralyze if you're not already paralyzed, and otherwise do
  ; damage.
  (wait 1)
  (assert (= G.player.hp 100))
  (assert (.player-has? StatusEffect.Para))
  (wait 1)
  (assert (= G.player.hp 93))
  (assert (.player-has? StatusEffect.Para))
  (wait 1)
  (assert (= G.player.hp 86))
  (assert (not (.player-has? StatusEffect.Para))))


(defn test-siren []
  (init [:tiles
    (+ (* ['floor] 10) ["siren"])])
  (setv G.rules.reality-bubble-size 20)

  ; The siren needs to build up shot power before it can paralyze.
  (assert-at [11 0] "siren")
  (wait 3)
  (assert (not (.player-has? StatusEffect.Para)))
  (assert-at [8 0] "siren")
  (wait)
  (assert (.player-has? StatusEffect.Para))
  (assert-at [8 0] "siren")
  (wait 2)
  (assert-at [6 0] "siren")
  (wait 3)
  (assert (not (.player-has? StatusEffect.Para)))
  (assert-at [3 0] "siren")
  (wait)
  (assert (.player-has? StatusEffect.Para))
  (assert-at [3 0] "siren")
  (wait 2)
  (assert-at [1 0] "siren")
  (assert (not (.player-has? StatusEffect.Para)))
  (assert (= G.player.hp 100))
  ; Up close, sirens use an ordinary melee attack.
  (wait)
  (assert-at [1 0] "siren")
  (assert (not (.player-has? StatusEffect.Para)))
  (assert (= G.player.hp 95)))


(defn test-golem []
  (init
    [:tiles ['floor 'floor 'floor "golem"]])

  ; Golems do nothing unless they're within 3 squares of Tris.
  (assert-at [4 0] "golem")
  (wk 'N)
  (wk 'S)
  (wait 10)
  (assert-at [4 0] "golem")
  (wk 'E)
  (assert-at [3 0] "golem"))


(defn test-cyclops []

  ; Cyclopes approach, but don't attack.
  (init [:tiles ['floor "cyclops"]])
  (wait 1)
  (assert-at 'E "cyclops")
  (wait 5)
  (assert (= G.player.hp 100))
  ; They're immune to swords and arrows.
  (wk 'E)
  (shoot 'E)
  (assert-at 'E "cyclops")

  ; Contra IQ, cyclopes will try to get orthogonal to you even if
  ; they're diagonally adjacent and there are no diagonal blockers.
  (init [
    :map "
       @ .
       . C"])
  (wait)
  (assert-at 'S "cyclops"))


(defn test-umber-hulk []

  (init [
    :map "
      @ ▒▒| ##U █1^█++██◀▶██"
    :map-marks {
      "▒▒" "Void"
      "##" "cracked wall"
      "█1" "trapped wall"
      "++" "door"}])
  (setv G.rules.reality-bubble-size 20)

  ; Umber hulks can destroy somewhat more tiles than IQ's kroggs. Not
  ; everything, though.
  (wait 100)
  (assert-at 'E "Void")
  (assert-at [9 0] "hole")
  (assert-at [10 0] "wall")
  (for [x (thru 2 7)]
    (setv stack (at (Pos G.map x 0)))
    (print x stack)
    (assert (or
      (not stack)
      (and (= (len stack) 1) (= (. stack [0] stem) "umber hulk"))))))


(defn test-lord []
  "This test effectively covers archdevils and Dark Princes, which are
  special cases of Lords of the Undead."

  (init [
    :map "
       . . .
       . L .
       . . .
       ██████
       @ ! ."
    :map-marks {
      "! " "cloak of invisibility"}])
        ; We use invisbility so monsters stay immobile.
  ; A Lord of the Undead can summon 2 monsters every 4 turns.
  (wk 'E)
  (assert-at [1 4] 'floor)
  (assert-at [2 4] 'floor)
  (wait 2)
  (assert-at [1 4] 'floor)
  (assert-at [2 4] 'floor)
  (wait)
  (assert-at [1 4] "ghost")
  (assert-at [2 4] "shade")
  ; It summons a 1-HP ghost and a 2-HP shade, then a 2-HP ghost and
  ; shade, and eventually cycles back to a 1-HP ghost and shade. The
  ; summons are placed in the same clockwise order as a generator.
  (wait (* 3 4))
  (setv p (Pos G.map 1 3))
  (for [[direction [stem hp]] (zip
      Direction.all
      (hy.I.itertools.cycle [
        ["ghost" 1] ["shade" 1]
        ["ghost" 2] ["shade" 2]
        ["ghost" 3] ["shade" 3]]))]
    (assert-at (+ p direction) stem)
    (assert-hp (+ p direction) hp))

  ; Lords don't get to summon while attacking the player.
  (init :starting-hp 500 [:map "@ L ."])
  (wait 10)
  (assert (= G.player.hp (- 500 (* 10 15))))
  (assert-at [2 0] 'floor))


(defn test-vampire []

  (init [
    :map "
       d o w
       . V g
       . . .
       ██████
       @ . ."
    :map-marks {
      "w " ["wizard" :hp 10]}])
  (assert-full-name [1 3] #[[a vampire (HP 1, wd ....., act "wander")]])
  ; Given enough time, all the non-vampires (except the devil, which
  ; isn't vampirizable) are vampirized, and the remaining empty space
  ; is filled with 3-HP bats.
  (wait 20)
  (assert (=
    (Counter (lfor
      p (burst (Pos G.map 1 3) 1)
      tile (at p)
      #(tile.stem tile.hp)))
    (Counter {
      #("vampire" 1) 3
      #("vampire" 10) 1
      #("bat" 3) 4
      #("devil" 1) 1})))

  ; A slain vampire becomes a 3-HP bat.
  (init
    [:tiles ["vampire"]])
  (wk 'E)
  (assert-at 'E "bat")
  (assert-hp 'E 3))


(defn test-doppelganger []

  ; Tris takes 5 damage for each point of damage received by a
  ; doppelganger (ignoring overkill).
  (init [:tiles ["doppelganger"]])
  (shoot 'E)
  (assert (= G.player.hp 95))
  (init [:tiles ["doppelganger"]])
  (wk 'E)
  (assert (= G.player.hp 95))
  (init [:tiles [["doppelganger" :hp 2]]])
  (wk 'E)
  (assert (= G.player.hp 90))
  (init [:tiles ['floor 'floor "jar of poison" "doppelganger"]])
  (shoot 'E)
  (assert (= G.player.hp 95))

  ; Instakills avoid this empathy damage.
  (init [:tiles ["doppelganger"]])
  (use-item "wand of death" [3 0])
  (assert-at 'E 'floor)
  (assert (= G.player.hp 100)))


(defn test-dragon []
 (init
   :starting-hp 10,000
   [:tiles ['floor "dragon egg"]])

 ; It takes a 1-HP dragon egg 4 turns to hatch. The newly created
 ; wyrmling doesn't get to move or attack on the same turn.
 (wait 3)
 (assert-at [2 0] "dragon egg")
 (wait)
 (assert-at [2 0] "wyrmling")
 (assert-hp [2 0] 4)
 ; The wyrmling can now move and attack.
 (wait)
 (assert (= G.player.hp 10,000))
 (assert-at 'E "wyrmling")
 (wait)
 (assert (= G.player.hp 9,992))
 ; After a total of 7 turns after hatching, the wyrmling becomes a
 ; dragon.
 (wait 5)
 (assert-at 'E "dragon")
 (assert-hp 'E 8)
 ; The dragon can continue gaining HP, up to 16.
 (wait 50)
 (assert-hp 'E 16))
