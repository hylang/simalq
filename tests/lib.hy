(defn init [quest]
  (import simalq.un-iq [read-quest iq-quest])
  (import simalq.main [start-quest])
  (start-quest (read-quest (iq-quest quest))))


(defmacro wk [direction-abbr [n-steps 1]]
  `(for [_ (range ~n-steps)]
    (hy.M.simalq/player-actions.do-action
      (hy.M.simalq/player-actions.Walk
        (. hy.M.simalq/geometry.Direction ~direction-abbr)))))
