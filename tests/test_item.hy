(require
  tests.lib [cant wk])
(import
  tests.lib [init mk-quest assert-at set-square mv-player]
  simalq.geometry [Pos Direction pos+ at]
  simalq.game-state [G])


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
