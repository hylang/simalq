(require
  tests.lib [wk])
(import
  tests.lib [init mk-quest locate assert-at]
  simalq.geometry [at]
  simalq.game-state [G])


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
