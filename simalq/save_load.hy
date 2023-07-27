(require
  hyrule [unless])
(import
  fractions [Fraction :as f/]
  itertools [count]
  time [time]
  datetime [datetime]
  json
  pickle
  zipfile [ZipFile ZIP-STORED ZIP-DEFLATED]
  simalq.game-state [G]
  simalq.util [menu-letters saved-games-dir mixed-number])
(setv  T True  F False)

;; --------------------------------------------------------------
;; * Helper macros
;; --------------------------------------------------------------

(defmacro no-gc [#* body]
  "Temporarily disable garbage collection. This can substantially
  speed up pickling and unpickling."
  `(try
    (hy.M.gc.disable)
    ~@body
    (finally
      (hy.M.gc.enable))))

(defmacro suppressing-quest [#* body]
  "Pull the quest object out of `G` temporarily."
  (setv quest (hy.gensym))
  `(try
    (setv ~quest G.quest)
    (setv G.quest "stub")
    ~@body
    (finally
      (setv G.quest ~quest))))

;; --------------------------------------------------------------
;; * Basic functions
;; --------------------------------------------------------------

(defn save-game [path]
  "Save the global object to disk."

  (setv meta (.encode :encoding "UTF-8" (.format "{}\n"
    (json.dumps :ensure-ascii F (dict
      :quest G.quest.name
      :level-n G.level-n
      :turn-n G.turn-n
      :player-hp G.player.hp
      :score G.score
      :player-hp-factor (.as-integer-ratio G.rules.player-hp-factor)
      :poison-factor (.as-integer-ratio G.rules.poison-factor))))))
  (setv state (no-gc (suppressing-quest
    (pickle.dumps G pickle.HIGHEST-PROTOCOL))))

  (with [o (ZipFile path "w")]
    (.writestr o "meta.json" meta :compress-type ZIP-STORED)
    (.writestr o "state.pkl" state :compress-type ZIP-DEFLATED))
  None)

(defn load-game [path]
  "Replace the global object with a saved one."

  (no-gc (suppressing-quest
    (setv new-global (pickle.loads
      (with [o (ZipFile path "r")]
        (.read o "state.pkl"))))
    (for [k G.__slots__]
      (setattr G k (getattr new-global k))))))

(defn get-saved-game-meta [path]
  (json.loads (with [o (ZipFile path "r")]
    (.read o "meta.json"))))

;; --------------------------------------------------------------
;; * Higher-level functions
;; --------------------------------------------------------------

(defn save-game-to-slot [checkpoint?]
  (setv directory (/ saved-games-dir G.quest.name))
  (.mkdir directory :parents T :exist-ok T)
  (if checkpoint?
    (for [i (count)]
      (setv path (/ directory (.format f"{(int (time))}-{i}.zip")))
      (unless (.exists path)
        (break)))
    (setv path (/ directory "main.zip")))
  (save-game path))

(defn get-saves-list [[quest-name None]]

  (setv saves (sorted
    (gfor
      path (try
        (list (.iterdir (/ saved-games-dir (or quest-name G.quest.name))))
        (except [FileNotFoundError]
          []))
      (dict
        :path path
        :main (= path.stem "main")
        :time (. path (stat) st-mtime)
        #** (get-saved-game-meta path)))
    :key (fn [d] #(
      (not (get d "main"))
      (get d "time")))))

  (setv display-lines [
    "                       Date  DL   Turn    HP   Score  HP f.  Poison f."
    #* (lfor
      [i save] (enumerate saves)
      (.format " ({}) {:4} {:%Y %b %d %H:%M} {:3d} {:6,} {:5,} {:7,} {:>6} {:>10}"
        (get menu-letters i)
        (if (get save "main") "main" "")
        (datetime.fromtimestamp (get save "time"))
        (get save "level_n")
        (get save "turn_n")
        (get save "player_hp")
        (get save "score")
        (mixed-number (f/ #* (get save "player_hp_factor")))
        (mixed-number (f/ #* (get save "poison_factor")))))])

  #(saves display-lines))
