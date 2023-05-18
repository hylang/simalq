(require
  hyrule [do-n]
  tests.lib [wk])
(import
  collections [Counter]
  fractions [Fraction :as f/]
  tests.lib [init mk-quest locate assert-at wait set-square shoot mv-player]
  simalq.geometry [Direction Pos ray at]
  simalq.game-state [G])
(setv  T True  F False)


(defn test-simple-melee-combat []
  (init (mk-quest
    [:tiles [["Dark Knight" :hp 5]]]))
  (defn monster [] (get (at (locate 'E)) 0))
  (defn check [turn score tris mon]
    (assert (and
      (= G.turn-n turn) (= G.score score)
      (= G.player.hp tris) (or (is mon None) (= (. (monster) hp) mon)))))

  (check :turn 0 :score 0 :tris 100 :mon 5)
  ; Attack the monster, doing 2 damage. We get hit for 12 damage.
  (wk E)
  (check :turn 1 :score 0 :tris 88 :mon 3)
  ; And again.
  (wk E)
  (check :turn 2 :score 0 :tris 76 :mon 1)
  (assert (= (. (monster) stem) "Dark Knight"))
    ; The monster's still there.
  ; Finish the monster off. We take no damage this time.
  (wk E)
  (check :turn 3 :score 75 :tris 76 :mon None)
  (assert-at 'E 'floor))


(defn test-monster-melee-diag []

  ; A monster can melee-attack diagonally around a column.
  (init (mk-quest
    [:map "
      | @
      o ."]))
  (assert (= G.player.hp 100))
  (assert-at 'SW "orc")
  (wait)
  (assert (= G.player.hp 97))
  (assert-at 'SW "orc")

  ; But not a wall. So the orc will use its first turn to walk east.
  (init (mk-quest
    [:map "
      ██@
      o ."]))
  (assert (= G.player.hp 100))
  (assert-at 'SW "orc")
  (wait)
  (assert (= G.player.hp 100))
  (assert-at 'S "orc"))


(defn test-approach []

  ; A monster can approach in a straight line.
  (init (mk-quest
    [:tiles ['floor 'floor 'floor "orc"]]))
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
  (init (mk-quest
    [:tiles ["pillar" "orc"]]))
  (wait)
  (assert-at 'NE "orc")

  ; A monster stymied going west *and* northwest will take 3 turns
  ; to finally try southwest.
  (init (mk-quest
    [:map "
      . | .
      @ | o
      . . ."]))
  (do-n 3
    (assert-at 'SE 'floor)
    (wait))
  (assert-at 'SE "orc")

  ; When the monster is blocked W, NW, and SW, he'll just sit there
  ; forever.
  (init (mk-quest
    [:map "
      . . .
      @ ██o
      . . ."]))
    ; A wall, unlike a pillar, blocks diagonal movement.
  (defn assert-mon-at [x y]
    (assert (= (. (at (Pos G.map x y)) [0] stem) "orc")))
  (assert-mon-at 2 1)
  (wait 100)
  (assert-mon-at 2 1)

  ; A monster outside the reality bubble can't move.
  (setv r G.rules.reality-bubble-size)
  (init (mk-quest [:tiles [
    #* (* ['floor] r)
    "orc"]]))
  (assert-mon-at (+ r 1) 0)
  (wait 100)
  (assert-mon-at (+ r 1) 0)
  (wk E)
  (assert-mon-at r 0)

  ; A monster can chase Tris around a wrapped map.
  ; (Accompanied by "Yakety Sax", one imagines.)
  (init (mk-quest [
    :player-start #(4 4)
    :width 9 :height 9 :wrap-y T]))
  (set-square 'N "orc")
  (do-n 100
    (wk S)
    (assert-at 'N "orc")))


(defn test-approach-march []
  "Lines of monsters can march towards the player without losing
  turns or going astray by blocking each other. This is an important
  property of the spiral activation order of monsters that we inherit
  from IQ."

  (init (mk-quest
    [:player-start #(8 8)]))

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
    (init (mk-quest
      [:height 1 :tiles ["pile of gold" "orc" "wall" "orc" "orc"]]))
    (when (not dainty)
      (setv G.rules.dainty-monsters F))

    (if dainty
      ; By default, normal monsters will only step on plain floor.
      (do
        (wait 10)
        (assert (= (. (at (Pos G.map 2 0)) [0] stem) "orc")))
      ; If dainty-monsters mode is off, they can step on e.g. items.
      (do
        (wait 1)
        (assert-at 'E ["orc" "pile of gold"])))

    ; Either way, monsters block each other.
    (assert-at (Pos G.map 4 0) "orc")
    (assert-at (Pos G.map 5 0) "orc")))


(defn test-orc-or-goblin []
  (init (mk-quest
    [:tiles [["orc" :hp 3]]]))

  (assert-at 'E "orc")
  ; Get stabbed.
  (wait)
  (assert (and (= G.player.hp 91) (= G.score 0)))
  ; Hit the orc. It survives, but it now does less damage,
  ; and we get some points.
  (wk E)
  (assert (and (= G.player.hp 88) (= G.score 6)))
  ; Kill it. We only get 3 points because it only has 1 more HP to
  ; take off.
  (wk E)
  (assert (and (= G.player.hp 88) (= G.score 9)))
  (assert-at 'E 'floor))


(defn test-generated-high-hp []
  "A generated monster with more than 3 HP does damage as if it had
  3 HP."

  (init (mk-quest
    [:tiles [["orc" :hp 10]]]))

  (for [[orc-hp score player-hp] [
      [10 0 100]
      [ 8 6  91]
      [ 6 12 82]
      [ 4 18 73]
      [ 2 24 67]
      [ 0 30 67]]]
    (assert (and
      (= G.player.hp player-hp)
      (= G.score score)
      (if (= orc-hp 0)
        (not (at (Pos G.map 1 0)))
        (= (. (at (Pos G.map 1 0)) [0] hp) orc-hp))))
    (wk E)))


(defn test-generator []
  (init (mk-quest [
    :map "
      @ ████. .
      ████. G .
      ████████. "
    :map-marks {
      "G " ["orc generator"
        :generate-frequency (f/ 2 3)
        :generate-hp 2]}]))

  (defn check [power tN tNE tE tSE tW]
    (and
      (= (. (at (Pos G.map 3 1)) [0] generation-power) power)
      (assert-at (Pos G.map 3 2) tN)
      (assert-at (Pos G.map 4 2) tNE)
      (assert-at (Pos G.map 4 1) tE)
      (assert-at (Pos G.map 4 0) tSE)
      (assert-at (Pos G.map 2 1) tW)))

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
  (init (mk-quest [
    :height 1
    :tiles [
      'floor 'floor "wall" 'floor
      ["orc generator" :generate-frequency (f/ 1)]
      'floor]]))
  (setv G.rules.reality-bubble-size 4)

  ; Outside the reality bubble, generators do nothing.
  (wait 10)
  (assert-at (Pos G.map 4 0) 'floor)
  (assert-at (Pos G.map 6 0) 'floor)
  ; Once in it, they can spawn monsters into adjacent squares even
  ; if those squares aren't in the reality bubble themselves.
  (wk E)
  (wait 2)
  (assert-at (Pos G.map 4 0) "orc")
  (assert-at (Pos G.map 6 0) "orc"))


(defn test-generated-first-turn []
  (for [gen-west [T F]]
    ; Try a case with the generator nearer to the player than the
    ; monster (`gen-west T`) and farther (`gen-west F`).

    (init (mk-quest [
      :map "
        . . $ $
        @ . a b
        . . $ $"
      :map-marks {
        (if gen-west "a " "b ") ["orc generator"
          :generate-frequency (f/ 1 4)]
        (if gen-west "b " "a ") 'floor
        "$ " "pile of gold"}]))
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
  (init (mk-quest
    [:tiles [["ghost" :hp 3]]]))
  (assert (and (= G.player.hp 100) (= G.score 0)))
  (assert-at 'E "ghost")
  ; Attack the ghost. We get 10 points for doing 2 damage.
  ; The ghost strikes back, dying by kamikaze, which grants no points.
  (wk E)
  (assert (and (= G.player.hp 95) (= G.score 10)))
  (assert-at 'E 'floor))


(defn test-bat-or-bee []

  ; A bat that's next to Tris just chews on her instead of moving.
  (init (mk-quest
    [:tiles ["bat"]]))
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
  (init (mk-quest
    [:map "
        . . . . .
        . . b . .
        ██. . . .
        @ ██. . ."]))
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
  (init (mk-quest
    [:tiles ['floor "devil"]]))
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 90))
  ; Close up, a 1-HP devil melees for 3 damage.
  (wk E)
  (assert (= G.player.hp 87))

  ; A diagonally blocked devil can't melee, so it shoots.
  (init (mk-quest
    [:map "
      . d
      @ ██"]))
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 90))

  ; Monster shots can wrap around.
  (init (mk-quest [:wrap-x T
    :map ". @ ████████d ."]))
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 90))

  ; However, monsters only consider one direction for shooting. If the
  ; shortest path to the player (as the xorn phases) is blocked, they
  ; won't consider another valid direction.
  (init (mk-quest [:wrap-x T
    :map ". . @ ██d . ."]))
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 100))
  ; The player can shoot in that other direction.
  (assert-at (Pos G.map 4 0) "devil")
  (shoot 'W)
  (assert-at (Pos G.map 4 0) 'floor))


(defn test-wizard []
  (init (mk-quest
    [:player-start #(5 0) :tiles [["wizard" :hp 3]]]))

  (assert (= G.player.hp 100))
  ; Wizards always melee for 4 damage.
  (wait)
  (assert (= G.player.hp 96))
  ; A 3-HP wizard shoots for 12 damage.
  (wk W)
  (assert (= G.player.hp 84))
  ; At 2 HP, its shot damage drops to 8.
  (shoot 'E)
  (assert (= G.player.hp 76)))


(defn test-imp []

  (init (mk-quest
    [:height 1 :tiles ["wall" "wall" "imp"]]))
  (assert (= G.player.hp 100))
  ; An imp initially needs to gain some shot power before it can
  ; shoot.
  (wait)
  (assert (= G.player.hp 100))
  ; Now it can shoot (and is left with 3/5 power). Walls are no
  ; obstacle to imp shots.
  (wait)
  (assert (= G.player.hp 99))
  (assert (= (. (at (Pos G.map 3 0)) [0] shot-power) (f/ 3 5)))
  ; The void blocks imp shots. Since the imp can't shoot, it gains
  ; no shot power.
  (set-square (Pos G.map 2 0) "Void")
  (wait)
  (assert (= G.player.hp 99))
  (assert (= (. (at (Pos G.map 3 0)) [0] shot-power) (f/ 3 5)))
  ; At ranges of 2 or less, an imp flees.
  (mv-player 1 0)
  (wait)
  (assert-at (Pos G.map 4 0) "imp")

  ; On a turn that an imp isn't shooting or fleeing (even if it's
  ; gaining shot power), it wanders.
  (init (mk-quest
    [:map "
      . . . . .
      @ . . i .
      . . . . ."]))
   (wait)
   (assert (=
     (Counter (gfor
       x [2 3 4]
       y [0 1 2]
       :if (!= [x y] [3 1])
       (tuple (gfor  t (at (Pos G.map x y))  t.stem))))
     {#() 7  #("imp") 1})))


(defn test-thorn-tree []
  (init (mk-quest
    [:tiles ['floor ["thorn tree" :hp 3]]]))

  ; Thorn trees are immobile.
  (assert-at (Pos G.map 2 0) "thorn tree")
  (wait)
  (assert-at (Pos G.map 2 0) "thorn tree")
  ; They're immune to arrows.
  (assert (= (. (at (Pos G.map 2 0)) [0] hp) 3))
  (shoot 'E)
  (assert (= (. (at (Pos G.map 2 0)) [0] hp) 3))
  ; Up close, they scratch for 4 damage.
  (assert (= G.player.hp 100))
  (wk E)
  (assert (= G.player.hp 96))
  ; They're damaged normally by Tris's sword.
  (assert (= (. (at (Pos G.map 2 0)) [0] hp) 3))
  (wk E)
  (assert (= (. (at (Pos G.map 2 0)) [0] hp) 1)))


(defn test-tricorn []
  (init (mk-quest
    [:tiles ['floor 'floor 'floor "Tricorn"]]))

  ; Tricorns can't shoot from more than 3 squares away. When they
  ; can't shoot, they approach as normal.
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 100))
  (wait)
  (assert (= G.player.hp 94)))
