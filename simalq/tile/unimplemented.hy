(require
  simalq.tile [deftile])
(import
  simalq.tile [Tile])
(setv  T True  F False)


(defclass UnimplementedTile [Tile]
  "An IQ tile type that we don't yet meaningfully implement or
  substitute, but we can put in `Tile.types-by-iq-ix` as a stub."

  (setv fields ["tile_extras"])
  (setv field-defaults {"tile_extras" None})

  (defn [classmethod] read-tile-extras [cls mk-pos a b]
    (dict :tile-extras #(a b))))


(do-mac `(do ~@(gfor [iq-ix stem] [

    [152 "random_gate"]
    [187 "magical_mirror"]
    [202 "rotation_trap"]
    [205 "moving_wall"]
    [210 "dark_king"]]

  `(deftile "� " ~stem UnimplementedTile
    :iq-ix ~iq-ix))))
