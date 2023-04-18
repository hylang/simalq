(require
  hyrule [do-n]
  tests.lib [wk])
(import
  tests.lib [init mk-quest locate assert-at wait set-square]
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
    [:player-start #(1 1)
      :tiles ["pillar" "orc"]]))
  (set-square 'NE "pillar")
  (do-n 3
    (assert-at 'SE 'floor)
    (wait))
  (assert-at 'SE "orc")

  ; When the monster is blocked W, NW, and SW, he'll just sit there
  ; forever.
  (init (mk-quest
    [:player-start #(1 1)
      :tiles ["wall" "orc"]]))
        ; A wall, unlike a pillar, blocks diagonal movement.
  (defn assert-mon-at [x y]
    (assert (= (. (at (Pos G.map x y)) [0] stem) "orc")))
  (assert-mon-at 3 1)
  (wait 100)
  (assert-mon-at 3 1)

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
