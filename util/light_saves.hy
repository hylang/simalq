#[[This file has functions for manipulating "light saves", an
alternative save-file format in which only the player's action history
is saved. Compared to the real save-file format, this format is more
robust to internal changes to SQ, and it results in smaller files, but
it's much slower to load games that have gone on for hundreds or
thousands of turns.]]


(require
  hyrule [ecase defmain])
(import
  simalq.commands *
  simalq.main [take-turn]
  pathlib [Path]
  tqdm [tqdm])


(defn save [path]
  (.write-bytes (Path path) (hy.I.gzip.compress (.encode :encoding "ASCII"
    (hy.I.json.dumps (lfor
      state (cut G.states 1 None)
      :setv a state.action
      (ecase (type a)
        Wait ["Wait"]
        Walk ["Walk"
          a.direction.x a.direction.y]
        Shoot ["Shoot"
          a.direction.x a.direction.y]
        UseItem ["UseItem"
          a.item-ix a.target-x a.target-y]
        UseControllableTeleporter ["UseControllableTeleporter"
          a.direction a.target-x a.target-y])))))))

(defn load [path]
  (defn d [xs]
    (get Direction.from-coords (tuple xs)))

  (for [[atype #* args] (tqdm
      (hy.I.json.loads (hy.I.gzip.decompress (.read-bytes (Path path)))))]
    (take-turn (ecase atype
      "Wait" (Wait)
      "Walk" (Walk (d args))
      "Shoot" (Shoot (d args))
      "UseItem" (UseItem #* args)
      "UseControllableTeleporter" (UseControllableTeleporter
        (d (get args 0))
        #* (cut args 1 None))))))


(defmain [_ action path]

  (import
    simalq.quest [start-quest start-level]
    simalq.un-iq [iq-quest]
    simalq.main [player-io main-io-loop])

  (start-quest (iq-quest "New First Quest"))
  (start-level 1)

  (ecase action

    "save-example" (do
      (do-mac `(do
        ; Define the compass directions as local variables.
        ~@(gfor
          d hy.I.simalq/geometry.Direction.all
          :setv a (hy.models.Symbol d.abbr)
          `(setv ~a (. Direction ~a)))))
      (setv actions [
        SE E E E
        SE SE SE SE
        E E E E E E E E E E E
        N N
        W W W W W W W
        E
        (Wait) (Wait) (Wait) (Wait) W W
        W E (Wait) (Wait) (Wait) (Wait) W
        W (Shoot W) (Shoot W)
        W W W W W W W W W
        N N N
        W W W W
        (UseItem 0 3 16)])
      (for [action actions]
        (take-turn (if (isinstance action Direction)
          (Walk action)
          action)))
      (save path))

    "load-example" (do
      (load path)
      (with [(player-io)]
        (main-io-loop)))))
