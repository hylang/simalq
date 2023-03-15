(import
  collections [Counter]
  toolz [partition]
  simalq.un-iq [read-quest iq-file]
  simalq.tile)


(defn test-read-bootcamp-level1 []
  (setv level (get (read-quest (iq-file "Boot Camp 2")) "levels" 0))

  ; Check the level attributes.
  (for [[got expected] (partition 2 [
      level.title (.join "\n" [
        "Welcome to Boot Camp!"
        "Let's start with some basic scenery."
        "Shift-click to identify objects."])
      level.start #(0 15)
      level.wrap-x False
      level.wrap-y False
      level.next-level 2
      level.light 6
      level.poison 5
      level.time 0
      level.exit-speed 10
      level.moving-exit-start None])]
    (assert (= got expected)))

  ; Count the number of tiles of each type that occur in the map.
  (assert (=

    (dict (Counter (gfor
      row level.map
      tile row
      (.replace tile.slug "-" "_"))))

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
  (assert (= (get level "map" 0 0 "slug") "floor"))
  (assert (= (get level "map" 15 0 "slug") "key"))
  (assert (= (get level "map" 15 15 "slug") "crumbling-wall")))
