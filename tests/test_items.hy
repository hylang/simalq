(require
  tests.lib [cant wk])
(import
  tests.lib [init assert-at]
  simalq.geometry [Pos Direction pos+ at]
  simalq.game-state [G])


(defn test-treasure []
  (init "Boot Camp 2")

  (setv G.player-pos (Pos G.map 6 14))
  (assert (= G.score 0))
  (assert-at 'E "pile of gold")
  (wk E)
  (assert (= G.score 100))
  (assert-at 'here 'floor)

  (setv G.player-pos (Pos G.map 5 10))
  (assert-at 'N "handful of gems")
  (wk N)
  (assert (= G.score 350))
  (assert-at 'here 'floor))


(defn test-key-get []
  (init "Boot Camp 2")
  (setv G.player-pos (Pos G.map 13 10))
  (assert (= G.keys 0))
  (wk S)
  (assert (= G.keys 1))

  (init "Boot Camp 2")
  (setv G.player-pos (Pos G.map 13 10))
  (setv G.keys G.rules.max-keys)
  (cant (wk S) "Your keyring has no room for another key."))
