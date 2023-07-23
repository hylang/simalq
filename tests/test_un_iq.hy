(import
  collections [Counter]
  fractions [Fraction :as f/]
  toolz [partition]
  simalq.util [seq]
  simalq.geometry [Pos]
  simalq.un-iq [iq-quest])


(defn get-level-map [quest-name level-n]
  (. (iq-quest quest-name) levels [(- level-n 1)] map data))

(defn assert-stem [m x y stem]
  (assert (= (. m [x] [y] [0] stem) stem)))


(defn test-get-all []
  (setv d (iq-quest 'all))
  (assert (is (type d) dict))
  (assert (= (len d) 7)))


(defn test-bootcamp []
  (setv quest (iq-quest "Boot Camp 2"))
  (assert (= quest.name "Boot Camp 2"))
  (assert (= quest.title "Boot Camp will teach you how to play Infinity Quest II"))
  (assert (= quest.starting-hp 500))
  (assert (= (len quest.levels) 26)))


(defn test-bootcamp-level1 []
  (setv level (. (iq-quest "Boot Camp 2") levels [0]))

  ; Check the level attributes.
  (for [[got expected] (partition 2 [
      level.title (.join "\n" [
        "Welcome to Boot Camp!"
        "Let's start with some basic scenery."
        "Shift-click to identify objects."])
      level.player-start (Pos level.map 0 15)
      level.next-level 2
      level.poison-intensity (f/ 1 5)
      level.time-limit 0
      level.exit-speed 10
      level.moving-exit-start None
      level.map.wrap-x False
      level.map.wrap-y False])]
    (assert (= got expected)))

  ; Count the number of tiles of each type that occur in the map.
  (assert (=

    (dict (Counter (gfor
      row level.map.data
      stack row
      tile stack
      tile.stem)))

    {

      "wall" 50
      "door" 1
      "one-way door (west)" 1
      "one-way door (south)" 1
      "one-way door (east)" 2
      "cracked wall" 2
      "locked door" 1
      "locked disappearing door" 1
      "pillar" 15
      "exit" 1

      "key" 2
      "pile of gold" 1
      "handful of gems" 1}))

  ; Check a few corner tiles, so we know we haven't rotated or
  ; reflected the map.
  (assert (= (. level map data [0] [0]) [])) ; I.e., floor
  (assert (= (. level map data [15] [0] [0] stem) "key"))
  (assert (= (. level map data [15] [15] [0] stem) "cracked wall"))

  ; Check the hit points of the two cracked walls.
  (assert (= (. level map data [7] [8] [0] hp) 4))
  (assert (= (. level map data [15] [15] [0] hp) 2)))


(defn test-denazify []
  (setv m (get-level-map "New Nightmare" 24))
  ; The original 5-by-5 trap swastikas on this level each comprise
  ; 17 traps and 8 floor squares.
  ; Our replacements have 13 traps and 12 floor squares.
  (assert (=
    (dict (Counter (gfor
      x (seq 12 16)
      y (seq 29 33)
      :setv stack (get m x y)
      (if (len stack)
        (.join "-" (gfor  t stack  t.stem))
        'floor))))
    {"fixed damaging trap" 13  'floor 12})))


(defn test-varlife []
  (setv m (get-level-map "New First Quest" 3))
  (setv t (get m 11 11 0))
  (assert (= t.stem "Dark Knight"))
  (assert (= t.hp 1))
  (setv t (get m 11 2 0))
  (assert (= t.stem "Tricorn"))
  (assert (= t.hp 4)))


(defn test-generator []
  (setv m (get-level-map "New First Quest" 1))
  (defn check [x y generator-hp generates frequency generate-hp]
    (setv t (get m x y 0))
    (assert (and
      (= t.hp generator-hp)
      (= t.summon-class generates)
      (= t.summon-frequency frequency)
      (= t.summon-hp generate-hp))))

  (check 12 19  2  "orc"   (f/ 2 3) 2)
  (check  2 17  1  "ghost" (f/ 1 6) 2)
  (check  3 16  1  "ghost" (f/ 1 6) 2)
  (check  4 15  1  "ghost" (f/ 1 6) 2)
  (check  6 10  3  "orc"   (f/ 1 2) 1))


(defn test-healing-potions []
  (setv m (get-level-map "Boot Camp 2" 9))

  (assert-stem m  1 14 "snack")         ; healing salve
  (assert-stem m  1 10 "meal")          ; healing potion
  (assert-stem m  1  6 "meal")          ; unknown potion, good
  (assert-stem m  6  1 "empty platter") ; unknown potion, neutral
  (assert-stem m 10  1 "rotten food")   ; unknown potion, bad
  (assert-stem m 14  1 "dessert"))      ; super-healing potion


(defn test-wallfall []
  (setv m (get-level-map "New DeathQuest" 2))
  (defn check [x y stem wallnum]
    (setv [tile] (get m x y))
    (assert (and (= tile.stem stem) (= tile.wallnum wallnum))))

  (check 10 4  "wallfall trap" 1)
  (check  5 0  "wallfall trap" 2)
  (check  0 4  "wallfall trap" 3)
  (check  5 8  "wallfall trap" 4)
  (check  8 4  "trapped wall" 1)
  (check  5 2  "trapped wall" 2)
  (check  5 6  "trapped wall" 3)
  (check  2 4  "trapped wall" 4))


(defn test-chest []
  (defn check [x y contents]
    (assert (=
      (lfor  tile (get m x y)  tile.stem)
      ["treasure chest" contents])))

  (setv m (get-level-map "Boot Camp 2" 25))
  (check 27 0 "pile of gold")
  (check 27 17 "snack")

  ; Chests containing unknown potions have a slightly more complex
  ; internal structure.
  (setv m (get-level-map "New First Quest" 11))
  (check 3 16 "meal"))


(defn test-gate []
  (defn check [gate-x gate-y target-x target-y]
    (setv [tile] (get m gate-x gate-y))
    (assert (= tile.stem "gate"))
    (assert (= tile.target.xy #(target-x target-y))))

  (setv m (get-level-map "Boot Camp 2" 8))
  (check  3 12  5 15)
  (check  0 10  30 12))


(defn test-removed-traps []
  (setv m (get-level-map "Boot Camp 2" 6))

  (assert-stem m 15  5 "pile of debris")  ; dimness trap
  (assert-stem m 10  3 "pile of debris")  ; darkness trap
  (assert-stem m 15 16 "broken trap"))    ; false exit
