(require
  simalq.macros [defmeth]
  simalq.tile [deftile])
(import
  dataclasses [dataclass]
  simalq.tile [Tile])
(setv  T True  F False)


(defclass UnimplementedTile [Tile]
  "An IQ tile type that we don't yet meaningfully implement or
  substitute, but we can put in `Tile.types-by-iq-ix` as a stub."

  (setv fields ["tile_extras"])

  (defmeth __init__ [[pos None] [tile-extras None]]
    (object.__setattr__ @ "pos" pos)
    (object.__setattr__ @ "tile_extras" tile-extras))

  (defn [classmethod] read-tile-extras [cls mk-pos a b]
    (dict :tile-extras #(a b))))


(do-mac `(do ~@(gfor [iq-ix stem] [

    [106 "wand_of_gating"]
    [121 "poison_gas_bomb"]
    [129 "archdevil"]
    [130 "exit_mobile"]
    [132 "golem"]
    [134 "siren"]
    [152 "random_gate"]
    [164 "cyclops"]
    [165 "dark_prince"]
    [166 "blind_mage"]
    [177 "doppelganger"]
    [178 "magical_barrier_generator"]
    [179 "magical_barrier_east"]
    [180 "magical_barrier_north"]
    [184 "giant_ant"]
    [185 "dark_brain"]
    [187 "magical_mirror"]
    [190 "wall_generator_north"]
    [191 "wall_generator_south"]
    [192 "wall_generator_west"]
    [193 "wall_generator_east"]
    [195 "dragon_egg"]
    [196 "wyrm"]
    [197 "dragon"]
    [202 "rotation_trap"]
    [203 "krogg"]
    [204 "vampire"]
    [205 "moving_wall"]
    [206 "illusory_wall"]
    [207 "exploding_wall"]
    [208 "wall_making_trap"]
    [209 "snitch"]
    [210 "dark_king"]
    [211 "lord_of_the_undead"]]

  `(deftile "� " ~stem UnimplementedTile
    :iq-ix ~iq-ix))))
