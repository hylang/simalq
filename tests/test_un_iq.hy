(import
  collections [Counter]
  toolz [partition]
  simalq.geometry [Pos]
  simalq.un-iq [read-quest iq-quest])


(defn test-read-bootcamp []
  (setv quest (read-quest (iq-quest "Boot Camp 2")))
  (assert (= quest.title "Boot Camp will teach you how to play Infinity Quest II"))
  (assert (= quest.starting-life 500))
  (assert (= (len quest.levels) 26)))


(defn test-read-bootcamp-level1 []
  (setv level (. (read-quest (iq-quest "Boot Camp 2")) levels [0]))

  ; Check the level attributes.
  (for [[got expected] (partition 2 [
      level.title (.join "\n" [
        "Welcome to Boot Camp!"
        "Let's start with some basic scenery."
        "Shift-click to identify objects."])
      level.player-start (Pos level.map 0 15)
      level.next-level 2
      level.poison-interval 5
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
      tile row
      tile.stem)))

    (dict

      :floor (py "(
        4*3 + 4*3 - 1 + 4*6 + 4*2 + 4*3 - 1 + 6*10 - 7 +
        5*5 - 1 + 10*5 - 15 - 1)")
      :wall 50
      :door 1
      :one-way-door-west 1
      :one-way-door-south 1
      :one-way-door-east 2
      :damaged-wall 1
      :crumbling-wall 1
      :locked-door 1
      :locked-disappearing-door 1
      :pillar 15
      :exit 1

      :key 2
      :pile-of-gold 1
      :handful-of-gems 1)))

  ; Check a few corner tiles, so we know we haven't rotated or
  ; reflected the map.
  (assert (= (. level map data [0] [0] stem) "floor"))
  (assert (= (. level map data [15] [0] stem) "key"))
  (assert (= (. level map data [15] [15] stem) "crumbling_wall")))
