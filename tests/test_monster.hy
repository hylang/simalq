(require
  hyrule [do-n]
  tests.lib [wk])
(import
  tests.lib [init mk-quest locate assert-at wait set-square]
  simalq.geometry [Pos at]
  simalq.game-state [G])
(setv  T True  F False)


(defn test-simple-melee-combat []
  (init (mk-quest
    [:tiles [["Dark Knight" :hp 5]]]))
  (setv monster-p (locate 'E))
  (setv [monster] (at monster-p))
  (defn check [turn score tris mon]
    (assert (and
      (= G.turn-n turn) (= G.score score)
      (= G.player-hp tris) (or (is mon None) (= monster.hp mon)))))

  (check :turn 0 :score 0 :tris 100 :mon 5)
  ; Attack the monster, doing 2 damage. We get hit for 12 damage.
  (wk E)
  (check :turn 1 :score 0 :tris 88 :mon 3)
  ; And again.
  (wk E)
  (check :turn 2 :score 0 :tris 76 :mon 1)
  (assert (is (get (at monster-p) 0) monster))
    ; The monster's still there, and he's the same object that we
    ; started with.
  ; Finish the monster off. We take no damage this time.
  (wk E)
  (check :turn 3 :score 75 :tris 76 :mon None)
  (assert-at monster-p 'floor))


(defn test-approach []

  ; A monster can approach in a straight line.
  (init (mk-quest
    [:tiles ['floor 'floor 'floor "Dark Knight"]]))
  (assert-at 'E 'floor)
  (wait 2)
  (assert-at 'E 'floor)
  (wait)
  (assert-at 'E "Dark Knight")
  (assert (= G.player-hp 100))
  ; Now if Tris lollygags for another turn, he can hit her. Owie.
  (wait)
  (assert (= G.player-hp 88))

  ; A monster stymied when trying to go west will first try northwest.
  (init (mk-quest
    [:tiles ["pillar" "Dark Knight"]]))
  (wait)
  (assert-at 'NE "Dark Knight")

  ; A monster stymied going west *and* northwest will take 3 turns
  ; to finally try southwest.
  (init (mk-quest
    [:player-start #(1 1)
      :tiles ["pillar" "Dark Knight"]]))
  (set-square 'NE "pillar")
  (do-n 3
    (assert-at 'SE 'floor)
    (wait))
  (assert-at 'SE "Dark Knight")

  ; When the monster is blocked W, NW, and SW, he'll just sit there
  ; forever.
  (init (mk-quest
    [:player-start #(1 1)
      :tiles ["wall" "Dark Knight"]]))
        ; A wall, unlike a pillar, blocks diagonal movement.
  (setv [monster] (at (Pos G.map 3 1)))
  (assert (= monster.stem "Dark Knight"))
  (assert (= monster.pos (Pos G.map 3 1)))
  (wait 100)
  (assert (= monster.pos (Pos G.map 3 1)))

  ; A monster outside the reality bubble can't move.
  (setv r G.rules.reality-bubble-size)
  (init (mk-quest [:tiles [
    #* (* ['floor] r)
    "Dark Knight"]]))
  (setv [monster] (at (Pos G.map (+ r 1) 0)))
  (assert (= monster.stem "Dark Knight"))
  (assert (= monster.pos (Pos G.map (+ r 1) 0)))
  (wait 100)
  (assert (= monster.pos (Pos G.map (+ r 1) 0)))
  ; Move east, bringing the monster into the reality bubble.
  ; Now he can approach.
  (wk E)
  (assert (= monster.pos (Pos G.map r 0)))

  ; A monster can chase Tris around a wrapped map.
  ; (Accompanied by "Yakety Sax", one imagines.)
  (init (mk-quest [
    :player-start #(4 4)
    :width 9 :height 9 :wrap-y T]))
  (set-square 'N "Dark Knight")
  (do-n 100
    (wk S)
    (assert-at 'N "Dark Knight")))


(defn test-nondainty []
  (init (mk-quest
    [:tiles ["pile of gold" "Dark Knight"]]))
  (set-square 'NE "pile of gold")
  (setv [monster] (at (Pos G.map 2 0)))
  (assert (= monster.stem "Dark Knight"))

  ; By default, normal monsters will only step on plain floor.
  (wait 10)
  (assert (= monster.pos (Pos G.map 2 0)))

  ; If dainty-monsters mode is off, they can step on e.g. items.
  (setv G.rules.dainty-monsters F)
  (wait 1)
  (assert (= monster.pos (Pos G.map 1 0)))
  (assert-at monster.pos ["Dark Knight" "pile of gold"]))
