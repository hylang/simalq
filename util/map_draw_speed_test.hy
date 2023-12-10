"Test speed and memory consumption of drawing a large map."


(require
  hyrule [case do-n])
(import
  sys
  time [perf-counter-ns]
  cProfile
  pstats
  simalq.game-state [G]
  simalq.un-iq [iq-quest]
  simalq.quest [start-quest start-level]
  tests.lib *)


(start-quest (iq-quest "New First Quest"))
(start-level 16)


(defn thing []
  (hy.I.simalq/display.draw-screen 244 54 G.player.pos :messages #()))

(case (get sys.argv 1)

  "time" (do
    (do-n 3
      (setv t1 (perf-counter-ns))
      (do-n 30
        (thing))
      (print (round :ndigits 3 (/ (- (perf-counter-ns) t1) 1e9))))
    (print (.format "Peak RAM: {} M" (round (/
      (. (hy.I.resource.getrusage hy.I.resource.RUSAGE-SELF) ru-maxrss)
      1024)))))

  "profile" (do
    (setv p (cProfile.Profile))
    (.enable p)
    (thing)
    (.disable p)
    (. (pstats.Stats p)
      (sort-stats pstats.SortKey.CUMULATIVE)
      (print-stats 20))))
