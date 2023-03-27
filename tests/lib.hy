(defn init [quest]
  (import simalq.un-iq [read-quest iq-quest])
  (import simalq.main [start-quest])
  (start-quest (read-quest (iq-quest quest))))


(defmacro wk [direction-abbr]
  (setv do-action (hy.gensym) Walk (hy.gensym) Direction (hy.gensym))
  `(do
    (import
      simalq.geometry [Direction :as ~Direction]
      simalq.player-actions [do-action :as ~do-action Walk :as ~Walk])
    (~do-action (~Walk (. ~Direction ~direction-abbr)))))
