(import
  simalq.geometry [Map Pos at pos+ Direction]
  simalq.quest [Level]
  simalq.un-iq [read-quest iq-quest]
  simalq.game-state [G]
  simalq.tile [add-tile]
  simalq.player-actions [Wait]
  simalq.main [start-quest start-level take-turn])


(defn init [quest [level-n 1]]
  (start-quest (if (isinstance quest str)
    (read-quest (iq-quest quest))
    quest))
  (when (!= level-n 1)
    (start-level level-n)))

(defn mk-level [
    n
    [player-start #(0 0)]
    [width 16] [height 16] [wrap-x False] [wrap-y False]
    [tiles #()]
      ; The requested tiles are placed in a line east of (0, 0).
    [title None]
    [next-level None]
    [poison-interval None]
    [time-limit None] [exit-speed None] [moving-exit-start None]]
  (setv m (Map.make :wrap-x wrap-x :wrap-y wrap-y :width width :height height))
  (for [[i stem] (enumerate tiles)]
    (add-tile (Pos m (+ i 1) 0) stem))
  (Level
    :title title
    :n n
    :next-level (or next-level (+ n 1))
    :map m
    :player-start (Pos m #* player-start)
    :poison-interval poison-interval
    :time-limit time-limit
    :exit-speed exit-speed
    :moving-exit-start moving-exit-start))


(defn assert-at [locator stem]
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

(defn wait [[n-times 1]]
  (for [_ (range n-times)]
    (take-turn (Wait))))
