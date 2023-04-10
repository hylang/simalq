(import
  simalq.color :as color
  simalq.game-state [G]
  simalq.geometry [Pos at dist])
(setv  T True  F False)


(defn draw-map [width height]
  "Return a generator of (foreground color, background color, map
  symbol) tuples."

  ; Loop over the screen coordinates `sy` and `sx`. I number `sy`
  ; bottom-to-top, although it's returned top-to-bottom, for
  ; similarity with `Pos`.
  (gfor sy (reversed (range height))  sx (range 0 width 2) (do
    (setv
      ; Find the map coordinates `mx` and `my`, using the rule
      ; that the screen should be centered on the player.
      mx (+ G.player.pos.x (- (// sx 2) (// width 4)))
      my (+ G.player.pos.y (- sy (// height 2))))
    (setv [color-fg color-bg mapsym]
      (if (and
          (or G.map.wrap-x (<= 0 mx (- G.map.width 1)))
          (or G.map.wrap-y (<= 0 my (- G.map.height 1))))
        ; We're on the map. Draw this square.
        (mapsym-at-pos (Pos G.map mx my))
        ; Otherwise, we're off the map. Draw border.
        [color.void color.void "██"]))
    (when (and (% width 2) (= sx (- width 1)))
      ; The screen has an odd width and we're on the right edge,
      ; so we can only draw the first character of the mapsym.
      (setv mapsym (get mapsym 0)))
    #(color-fg color-bg mapsym))))


(defn mapsym-at-pos [p]
  (setv floor-mapsym ". ")
  (setv multiple-tile-mapsym "&&")

  (setv stack (at p))
  (setv color-fg color.default-fg)
  (setv color-bg color.default-bg)
  (setv mapsym (cond
    (= (len stack) 0)
      floor-mapsym
    (= (len stack) 1)
      (. (get stack 0) mapsym)
    (and (= (len stack) 2) (is (get stack 0) G.player))
      (+ (get G.player.mapsym 0) (. stack [1] mapsym [1]))
    (is (get stack 0) G.player)
      (+ (get G.player.mapsym 0) (get multiple-tile-mapsym 1))
    True
      multiple-tile-mapsym))
  (when (=
      (dist p G.player.pos)
      (+ G.rules.reality-bubble-size 1))
    (if (= mapsym "██")
      (setv color-fg color.reality-fringe-block)
      (setv color-bg color.reality-fringe)))
  #(color-fg color-bg mapsym))
