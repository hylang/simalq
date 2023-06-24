(import
  dataclasses [dataclass]
  simalq.tile [Tile])
(setv  T True  F False)


(defclass UnimplementedTile [Tile]
  "An IQ tile type that we don't yet meaningfully implement or
  substitute, but we can put in `Tile.Tile.types-by-iq-ix` as a
  stub.")

(for [[iq-ix stem] [

    [93 "false_exit"]
    [94 "exit_to_level_"]
    [101 "fire_mage"]
    [102 "open_portcullis"]
    [103 "closed_portcullis"]
    [104 "wand_of_annihilation"]
    [105 "exit_making_wand"]
    [106 "wand_of_gating"]
    [108 "fountain"]
    [110 "candle"]
    [121 "poison_gas_bomb"]
    [122 "dimness_trap"]
    [129 "archdevil"]
    [130 "exit_mobile"]
    [131 "invisible_wall"]
    [132 "golem"]
    [133 "controllable_teleporter"]
    [134 "siren"]
    [135 "spider"]
    [136 "web"]
    [137 "poisonous_fountain"]
    [138 "hourglass"]
    [139 "phasing_wall_in"]
    [140 "phasing_wall_out"]
    [141 "phasing_wall_trap"]
    [142 "phasing_wall_trigger"]
    [143 "phase_wand"]
    [144 "unstable_moveable_wall_1"]
    [145 "unstable_moveable_wall_2"]
    [146 "fire_fountain"]
    [147 "wand_of_light"]
    [148 "darkness_trap"]
    [149 "counterpoison_amulet"]
    [150 "poisonous_amulet"]
    [151 "passwall_amulet"]
    [152 "random_gate"]
    [153 "amulet_of_sight"]
    [154 "wand_of_flame"]
    [156 "wand_of_death"]
    [157 "earthquake_bomb"]
    [158 "ring_of_protection"]
    [159 "amulet_of_poisonous_touch"]
    [160 "breakable_wall_north"]
    [161 "breakable_wall_east"]
    [162 "one_shot_gate"]
    [163 "archmage"]
    [164 "cyclops"]
    [165 "dark_prince"]
    [166 "blind_mage"]
    [167 "metal_door"]
    [168 "metal_door_control"]
    [169 "anti_magic_trap"]
    [170 "wand_of_webs"]
    [177 "doppelganger"]
    [178 "magical_barrier_generator"]
    [179 "magical_barrier_east"]
    [180 "magical_barrier_north"]
    [181 "gunk_seed"]
    [182 "gunk"]
    [183 "magical_key"]
    [184 "giant_ant"]
    [185 "dark_brain"]
    [186 "invisible_mage"]
    [187 "magical_mirror"]
    [188 "fading_wall"]
    [189 "confusion_trap"]
    [190 "wall_generator_north"]
    [191 "wall_generator_south"]
    [192 "wall_generator_west"]
    [193 "wall_generator_east"]
    [194 "arrow_trap"]
    [195 "dragon_egg"]
    [196 "wyrm"]
    [197 "dragon"]
    [198 "wand_of_remote_action"]
    [199 "random_damage_trap"]
    [201 "weakness_trap"]
    [202 "rotation_trap"]
    [203 "krogg"]
    [204 "vampire"]
    [205 "moving_wall"]
    [206 "illusory_wall"]
    [207 "exploding_wall"]
    [208 "wall_making_trap"]
    [209 "snitch"]
    [210 "dark_king"]
    [211 "lord_of_the_undead"]]]

  (assert (not-in iq-ix Tile.types-by-iq-ix))
  (setv cls (type
    stem
    #(UnimplementedTile)
    (dict
      :__slots__ ["tile_extras"]
      :__init__ (fn [self * pos [tile-extras None]]
        (object.__setattr__ self "pos" pos)
        (object.__setattr__ self "tile_extras" tile-extras))
      :stem stem
      :read-tile-extras (classmethod (fn [cls mk-pos a b]
        (dict :tile-extras #(a b)))))))
  (setv (get Tile.types-by-iq-ix iq-ix) cls)
  ; Add a global variable for the class so `pickle` can find it.
  (setv (get (globals) stem) cls))
