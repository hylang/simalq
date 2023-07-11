(require
  hyrule [unless])
(import
  re
  hyrule [parse-args]
  simalq.un-iq [iq-quest iq-quests-raw])


(setv url "http://hylang.org/simalq")

(setv enabled-iq-quests #(
   "New First Quest"
   "New DeathQuest"))
  ; New Nightmare also works, but I want to denazify it before
  ; advertising it.
  ; The other IQ quests have at least one unimplemented tile type.


(defn handle-cmdline-args [args]
  (setv version-string "Infinitesimal Quest 2 + ε version 0")
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
        :help "start a new game (default: load the main save slot, or start a new game if there isn't one)"]]))

  (setv p.QUEST (.replace p.QUEST "_" " "))
    ; We allow the quest name to be specified with underscores in
    ; place of spaces so the user doesn't have to quote them.
  (unless (in p.QUEST (iq-quests-raw))
    (exit f"No such quest: {(hy.repr p.QUEST)}. See `simalq --quests`."))

  (hy.M.simalq/main.main
    :iq-quest-name p.QUEST
    :load-main-save (not p.new)))

(defn quest-list []
  (.join "\n\n" (gfor
    name enabled-iq-quests
    (.join "\n" (gfor
      :setv q (iq-quest name)
      [k v] (.items {
        "Name" (.replace name " " "_")
        "Levels" (len q.levels)
        "Description" (re.sub r"\s+" " " q.title)})
      f"{k}: {v}")))))
