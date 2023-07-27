(require
  hyrule [unless])
(import
  fractions [Fraction :as f/]
  re
  hyrule [parse-args]
  simalq [__version__]
  simalq.un-iq [iq-quest iq-quests-raw]
  simalq.quest-definition [builtin-quests])


(setv url "http://hylang.org/simalq")

(setv advertised-iq-quests #(
   "New First Quest"
   "New DeathQuest"
   "New Nightmare"))
  ; The other IQ quests have at least one unimplemented tile type.


(defn handle-cmdline-args [args]
  (setv version-string f"Infinitesimal Quest 2 + ε version {__version__}")
  (setv p (parse-args :args args
    :prog "simalq"
    :description (+ version-string "\n" url)
    :formatter-class hy.M.argparse.RawDescriptionHelpFormatter
    [
      ["QUEST"
        :help "the quest to play; see --quests"]
      ["-V" "--version"
        :action "version" :version version-string]
      ["--quests"
        :action "version" :version (quest-list)
        :help "list available quests and exit"]
      ["-n" "--new"
        :action "store_true"
        :help "start a new game (default: load the main save slot, or start a new game if there isn't one)"]
      ["--player-hp-factor"
        :type f/ :metavar "X"
        :help "multiply your starting HP and all healing by the fraction X (affects new games only)"]
      ["--poison-factor"
        :type f/ :metavar "X"
        :help "multiply ambient poison rates by the fraction X (affects new games only)"]
      ["--skip-to-level"
        :type int :metavar "N"
        :help "skip to the requested level (for debugging)"]]))

  (setv p.QUEST (.replace p.QUEST "_" " "))
    ; We allow the quest name to be specified with underscores in
    ; place of spaces so the user doesn't have to quote them.
  (unless (in p.QUEST (available-quests))
    (exit f"No such quest: {(hy.repr p.QUEST)}. See `simalq --quests`."))

  (when (or
      (and p.player-hp-factor (< p.player-hp-factor 0))
      (and p.poison-factor (< p.poison-factor 0)))
    (exit "HP and poison factors must be nonnegative."))
  (when (= p.player-hp-factor 0)
    (exit "Okay, fine, you start with 0 HP. Welcome to the game. Game over. Wasn't that fun?"))

  (hy.M.simalq/main.main
    :quest ((get (available-quests) p.QUEST))
    :skip-to-level p.skip-to-level
    :load-main-save (not p.new)
    :rules (dfor
      k ["player_hp_factor" "poison_factor"]
      :if (is-not (getattr p k) None)
      k (getattr p k))))

(defn available-quests [] (dict
  #** hy.M.simalq/quest-definition.builtin-quests
  #** (dfor  x (iq-quests-raw)  x (fn [[x x]] (iq-quest x)))))
    ; `x` is passed through as a default argument so the values stay
    ; different.

(defn quest-list []
  (.join "\n\n" (gfor
    [name qf] (.items (available-quests))
    :if (or (in name advertised-iq-quests) (in name builtin-quests))
    (.join "\n" (gfor
      :setv q (qf)
      [k v] (.items {
        "Name" (.replace name " " "_")
        "Authors" q.authors
        "Levels" (len q.levels)
        "Description" (re.sub r"\s+" " " q.title)})
      f"{k}: {v}")))))
