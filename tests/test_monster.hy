(require
  tests.lib [wk])
(import
  tests.lib [init mk-quest locate assert-at]
  simalq.geometry [at]
  simalq.game-state [G])


(defn test-basic-combat []
  (init (mk-quest
    [:tiles [["Dark Knight" :hp 3]]]))
  (setv monster-p (locate 'E))
  (setv [monster] (at monster-p))
  (assert (and (= G.turn-n 0) (= G.score 0) (= monster.hp 3)))
  ; Attack the monster, doing 2 damage.
  (wk E)
  (assert (and (= G.turn-n 1) (= G.score 0) (= monster.hp 1)))
  (assert (is (get (at monster-p) 0) monster))
  ; Finish him off.
  (wk E)
  (assert (and (= G.turn-n 2) (= G.score 75)))
  (assert-at monster-p 'floor))
