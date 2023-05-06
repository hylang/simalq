(import
  collections [Counter]
  fractions [Fraction :as f/]
  toolz [partition]
  simalq.geometry [Pos]
  simalq.un-iq [read-quest iq-quest])


(defn test-get-all []
  (setv d (iq-quest 'all))
  (assert (is (type d) dict))
  (assert (= (len d) 7)))


(defn test-bootcamp []
  (setv quest (read-quest (iq-quest "Boot Camp 2")))
  (assert (= quest.title "Boot Camp will teach you how to play Infinity Quest II"))
  (assert (= quest.starting-hp 500))
  (assert (= (len quest.levels) 26)))


(defn test-bootcamp-level1 []
  (setv level (. (read-quest (iq-quest "Boot Camp 2")) levels [0]))

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


(defn test-varlife []
  (setv m (. (read-quest (iq-quest "New First Quest"))
    levels [2] map data))
  (setv t (get m 11 11 0))
  (assert (= t.stem "Dark Knight"))
  (assert (= t.hp 1))
  ; The life of variable-life monsters is stored in the second byte
  ; of their tile extras.
  (setv t (get m 11 2 0))
  (assert (= t.stem "tricorn"))
  (assert (= (get t.tile-extras 1) 4)))


(defn test-generator []
  (setv m (.
    (read-quest (iq-quest "New First Quest"))
    levels [0] map data))
  (defn check [x y generator-hp generates frequency generate-hp]
    (setv t (get m x y 0))
    (assert (and
      (= t.hp generator-hp)
      (= t.generate-class generates)
      (= t.generate-frequency frequency)
      (= t.generate-hp generate-hp))))

  (check 12 19  2  "orc"   (f/ 2 3) 2)
  (check  2 17  1  "ghost" (f/ 1 6) 2)
  (check  3 16  1  "ghost" (f/ 1 6) 2)
  (check  4 15  1  "ghost" (f/ 1 6) 2)
  (check  6 10  3  "orc"   (f/ 1 2) 1))


(defn test-healing-potions []
  (setv m (.
    (read-quest (iq-quest "Boot Camp 2"))
    levels [8] map data))
  (defn check [x y stem]
    (assert (= (. m [x] [y] [0] stem) stem)))

  (check  1 14 "snack")  ; healing salve
  (check  1 10 "meal")  ; healing potion
  (check  1  6 "meal")  ; unknown potion, good
  (check  6  1 "empty platter")  ; unknown potion, neutral
  (check 10  1 "rotten food")  ; unknown potion, bad
  (check 14  1 "dessert"))  ; super-healing potion


(defn test-wallfall []
  (setv m (.
    (read-quest (iq-quest "New DeathQuest"))
    levels [1] map data))
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
