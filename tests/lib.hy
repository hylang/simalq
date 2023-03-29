(defn init [quest [level-n 1]]
  (import simalq.un-iq [read-quest iq-quest])
  (import simalq.main [start-quest start-level])
  (start-quest (read-quest (iq-quest quest)))
  (when (!= level-n 1)
    (start-level level-n)))


(defn assert-at [locator stem]
  (import simalq.geometry [at pos+ Direction])
  (import simalq.game-state [G])

  (setv stack (at (cond
    (= locator 'here)
      G.player-pos
    (isinstance locator hy.models.Symbol)
      (pos+ G.player-pos (getattr Direction (str locator)))
    True
      locator)))

  (if (= stem 'floor)
    (assert (= (len stack) 0))
    (assert (and (= (len stack) 1) (= (. stack [0] stem) stem)))))


(defmacro cant [form msg-check]
  (setv e (hy.gensym))
  `(do
    (with [~e (hy.M.pytest.raises hy.M.simalq/util.ActionError)]
      ~form)
    (assert (in ~msg-check (. ~e value args [0])))))


(defmacro wk [direction-abbr [n-steps 1]]
  `(for [_ (range ~n-steps)]
    (hy.M.simalq/main.take-turn
      (hy.M.simalq/player-actions.Walk
        (. hy.M.simalq/geometry.Direction ~direction-abbr)))))
