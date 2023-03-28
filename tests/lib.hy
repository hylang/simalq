(defn init [quest]
  (import simalq.un-iq [read-quest iq-quest])
  (import simalq.main [start-quest])
  (start-quest (read-quest (iq-quest quest))))


(defn assert-at [locator stem]
  (import simalq.geometry [at pos+ Direction])
  (import simalq.game-state [G])

  (setv [tile] (at (cond
    (= locator 'here)
      G.player-pos
    (isinstance locator hy.models.Symbol)
      (pos+ G.player-pos (getattr Direction (str locator)))
    True
      locator)))

  (assert (= tile.stem stem)))


(defmacro cant [form msg-check]
  (setv e (hy.gensym))
  `(do
    (with [~e (hy.M.pytest.raises hy.M.simalq/player-actions.ActionError)]
      ~form)
    (assert (in ~msg-check (. ~e value args [0])))))


(defmacro wk [direction-abbr [n-steps 1]]
  `(for [_ (range ~n-steps)]
    (hy.M.simalq/player-actions.do-action
      (hy.M.simalq/player-actions.Walk
        (. hy.M.simalq/geometry.Direction ~direction-abbr)))))
