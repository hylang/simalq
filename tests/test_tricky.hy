"Tests of finicky interactions not associated with a specific tile."


(require
  tests.lib [wk])
(import
  tests.lib [init mk-quest assert-at]
  simalq.game-state [G]
  simalq.geometry [Pos])


(defn test-player-goes-first []
  ; The player acts first on every level.
  (init (mk-quest
    [:tiles [["Dark Knight" :hp 3] "exit"]]
    [:tiles [["Dark Knight" :hp 1]]]))
  (wk E 2)
  (assert (and (= G.player-hp 88)))
  (wk E 2)
  ; We're now on level 2, but the second monster won't get a hit in.
  (assert (and (= G.level-n 2)))
  (assert (and (= G.player-hp 88)))
  (assert-at 'E "Dark Knight")
  (wk E)
  (assert (and (= G.player-hp 88)))
  (assert-at 'E 'floor))


(defn test-refresh-level []
  ; Levels are refreshed when you revisit them.
  (init (mk-quest [
    :next-level 1 :tiles [["Dark Knight" :hp 3] "key" "exit"]]))

  (assert (= G.player.pos (Pos G.map 0 0)))
  (assert (= G.level-n 1))
  (wk E 4)
  (assert (and (= G.player-hp 88) (= G.keys 1)))
  (wk E)
  (assert (= G.player.pos (Pos G.map 0 0)))
  (assert (= G.level-n 1))
  ; The monster and the item are back now.
  (wk E 4)
  (print G.player-hp G.keys)
  (assert (and (= G.player-hp 76) (= G.keys 2))))
