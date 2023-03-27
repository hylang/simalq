(import
  simalq.geometry [Pos Direction pos+ at]
  simalq.game-state [G]
  simalq.un-iq [read-quest iq-quest]
  simalq.main [start-quest]
  simalq.player-actions [do-action Walk])


(defmacro wk [direction-abbr]
  `(do-action (Walk (. Direction ~direction-abbr))))


(defn test-treasure []
  (start-quest (read-quest (iq-quest "Boot Camp 2")))

  (setv G.player-pos (Pos G.map 6 14))
  (assert (= G.score 0))
  (assert (=
    (. (at (pos+ G.player-pos Direction.E)) [0] stem)
    "pile of gold"))
  (wk E)
  (assert (= G.score 100))
  (assert (= (at G.player-pos) []))

  (setv G.player-pos (Pos G.map 5 10))
  (assert (=
    (. (at (pos+ G.player-pos Direction.N)) [0] stem)
    "handful of gems"))
  (wk N)
  (assert (= G.score 350))
  (assert (= (at G.player-pos) [])))
