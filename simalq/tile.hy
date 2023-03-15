(require
  hyrule [ecase])
(import
  re
  toolz [partition]
  metadict [MetaDict]
  simalq.geometry [NORTH EAST SOUTH WEST]
  simalq.tile-defs [tile-defs Monster])


(setv max-walltrap-n 8)
(setv generator-steps 3)


(setv tile-types {})
(setv tile-types-by-iq-ix {})

((fn []
  "Initialize `tile-types`."

  (setv tdefs (lfor def tile-defs (MetaDict (dfor
    [k v] (partition 2 def)
    (hy.mangle k.name) v))))

  (for [odef tdefs]
    (setv def (.copy odef))

    (when (= def.iq-desc "A wyrm (young dragon) (9)")
      ; Remove the extra parenthetical.
      (setv def.iq-desc "A wyrm (9)"))

    (setv m (MetaDict (.groupdict (re.fullmatch
      r"
        ( (?P<article>  A | An | The | 10) \s )?
        (?P<stem> [^(]+ )
        (?: \s \( (?P<paren> [^)]+) \) )?"
      def.iq-desc
      re.VERBOSE))))

    (setv others-with-desc (lfor
      d tdefs
      :if (= d.iq-desc odef.iq-desc)
      d))
    (setv rep-n (+ 1 (.index others-with-desc odef)))

    (when (in m.stem ["trap" "trapped wall"])
      (setv def.walltrap-n rep-n)
      (when (> def.walltrap-n max-walltrap-n)
        (setv def.walltrap-n 0)))

    (when (= m.stem "phasing wall")
      (setv def.in-phase (ecase m.paren
        "in phase" True
        "out of phase" False)))

    (when (= m.stem "unstable moveable wall")
      (setv def.unstable-moves (ecase m.paren
        "1 move" 1
        "2 moves" 2)))

    (when (is def.class Monster)
      (setv def.generator (.endswith m.stem " generator"))
      (setv def.generated (and (not def.generator)
        (= (len others-with-desc) generator-steps)))
      (when (or def.generator def.generated)
        (setv def.life rep-n)))

    (setv m-attacks (and m.paren (re.fullmatch
      "([0-9/?]+)(?: - ([0-9/?]+))?"
      m.paren)))
    (when m-attacks
      (setv [def.damage-melee def.damage-ranged] (lfor
        x (.groups m-attacks)
        (cond
          (and x (in "/" x))
            (get (list (map int (.split x "/"))) (- def.life 1))
          (= x "?")
            None
          x
            (int x)
          True
            x))))

    (when (setx direction (.get (dict
        :up NORTH
        :down SOUTH
        :left WEST
        :right EAST) m.paren))
      (setv def.direction direction))

    (setv def.slug (or (.get def "slug") (re.sub " " "-" (.lower (.join " " (lfor
      part [
        m.stem
        (and (is (.get def "in_phase") True) "in")
        (and (is (.get def "in_phase") False) "out")
        (.get def "unstable_moves")
        (and (in "walltrap_n" def) def.walltrap-n)
        (and (in "direction" def) def.direction.name)
        (and (in "axis" def) def.axis.name)
        (and (.get def "mobile_exit") "mobile")
        (and (or (.get def "generator") (.get def "generated")) def.life)]
      :if (and (is-not part None) (is-not part False))
      (str part)))))))

    (assert (not-in def.slug tile-types))
    (setv (get tile-types def.slug) def))

  (.update tile-types-by-iq-ix (dfor
    tt (.values tile-types)
    tt.iq-ix tt))))
