(require
  hyrule [unless ecase]
  simalq.macros [defdataclass])
(import
  math [ceil]
  functools [reduce]
  textwrap
  hy.pyops *
  simalq.color :as color
  simalq.game-state [G]
  simalq.util [mixed-number menu-letters StatusEffect]
  simalq.geometry [Pos at dist]
  simalq.tile [Tile])
(setv  T True  F False)


(setv hide-messages #(
  ; These messages are probably too naggy to actually show. They're
  ; still implemented for the sake of testing the corresponding
  ; `CommandError`s.
  "Your way is blocked."
  "The border of the dungeon blocks your movement."))


(defdataclass ColorChar []
  "A character with a foreground and background color. Colors can
  be `None` or a symbol (a key of `color.by-name`)."
  [char fg bg])

(defn colorstr [s [fg None] [bg None]]
  "Convert a string to a colorstr, i.e., a list of `ColorChar`s."
  (lfor
    c s
    (ColorChar c fg bg)))

(defn uncolor [x]
  "Strip out the colors in a colorstr and return a plain string."
  (.join "" (gfor  c x  c.char)))

(defn bless-colorstr [B x]
  "Render a colorstr `x` into terminal sequences via the
  `blessed.Terminal` object `B`."
  (.join "" (gfor
    c x
    :setv fg (if c.fg
      (B.color-rgb #* (get color.by-name c.fg))
      (fn [x] x))
    :setv bg (if c.bg
      (B.on-color-rgb #* (get color.by-name c.bg))
      (fn [x] x))
    (fg (bg c.char)))))

(defn colorstr-to-width [x width]
  (cut (+ x (colorstr (* " " (max 0 (- width (len x)))))) width))


(defn draw-screen [
    width height
    focus
    [status-bar T]
    [tile-list None] [inventory F]
    [messages #()]
    [overmarks None]]
  "Return a colorstr for the main screen."

  (setv out [])

  ; The status bar is drawn first.
  (when status-bar
    (+= out (lfor
      line (draw-status-bar)
      (colorstr-to-width line width))))
  ; Then the map per se, including overmarks.
  (+= out (draw-map
    focus
    width
    (- height (if status-bar status-bar-lines 0))
    (or overmarks {})))

  ; Write other parts on top of the map, just under the status bar.
  ; They can overlap each other, should they be enabled
  ; simultaneously.
  (defn scribble-on-map [lines]
    (for [[i line] (enumerate lines)]
      (when status-bar
        (+= i status-bar-lines))
      (setv (cut (get out i) (len line)) line)))
  ; First, the tile list.
  (when tile-list
    (scribble-on-map [
      (colorstr f" {focus} ")
      #* (tile-menu
        (at focus)
        :pickable? (ecase tile-list  'pickable T  'nonpickable F))]))
  ; Then the inventory list.
  (when inventory
    (scribble-on-map (tile-menu
      G.player.inventory
      :pickable? T)))
  ; Then messages.
  (when messages
    (scribble-on-map (lfor
      m messages
      :if (not-in m hide-messages)
      line (textwrap.wrap m width)
      :setv line (+ line
        ; If there's room, pad the line with one extra space of the
        ; message color.
        (if (< (len line) width) " " ""))
      (colorstr line None color.message-bg))))

  out)


(defn draw-map [focus width height overmarks]
  "Return a list of colorstrs, one per line. The map will include
  (typically, is centered on) `focus`, a `Pos`. `overmarks` should be
  a dictionary mapping `Pos`es to pairs of `ColorChar`s to display
  over them. The `char` attribute of these `ColorChar`s can be `None`
  to indicate that the map character should be preserved."

  (setv xlim (map-limits (ceil (/ width 2)) G.map.width focus.x G.map.wrap-x))
  (setv ylim (map-limits height G.map.height focus.y G.map.wrap-y))

  (lfor
    my (reversed (range (get ylim 0) (+ (get ylim 1) 1)))
    (lfor mx (range (get xlim 0) (+ (get xlim 1) 1))
    ; Get the two characters and their colors for the corresponding
    ; mapsym.
    [i c] (enumerate
      (if (and
          (or G.map.wrap-x (<= 0 mx (- G.map.width 1)))
          (or G.map.wrap-y (<= 0 my (- G.map.height 1))))
        ; We're on the map. Draw this square, or an overmark if there
        ; is one for this position. If the square is ovewrapped, mark
        ; it specially and ignore any overmark.
        (do
          (setv p (Pos G.map mx my))
          (setv cs (mapsym-at-pos p))
          (for [[i c] (enumerate cs)] (cond
            (or
                (overwrapped G.map.wrap-x mx focus.x G.map.width)
                (overwrapped G.map.wrap-y my focus.y G.map.height)) (do
              (setv c.fg color.overwrapped)
              (setv c.bg color.default-bg))
            (in p overmarks) (do
              (setv o (get overmarks p i))
              (setv c.bg o.bg)
              (when o.char
                (setv c.fg o.fg)
                (setv c.char o.char)))
            (= [mx my] [focus.x focus.y])
              (setv c.bg color.focus)))
           cs)
        ; Otherwise, we're off the map. Draw border.
        (colorstr "██" color.void)))
    ; If the screen has an odd width, we can only draw the first
    ; character of the rightmost mapsym.
    :if (not (and (% width 2) (= mx (get xlim 1)) i))
    ; Yield one `ColorChar` at a time.
    c)))

(defn map-limits [sdim mdim fc wrap]
  "Return (inclusive) bounds for drawing a map. The idea here is to
  center the map on the focus so long as that doesn't show a lot of
  off-map squares. If it would, try to scroll the screen so that more
  map is shown.

  - `sdim` - length of the screen (actually, drawing window) on this
             dimension
  - `mdim` - length of the map on this dimension
  - `fc`   - coordinate of the focus
  - `wrap` - Boolean indicating whether this dimension is wrapped"

  (setv border 2)
    ; Don't show more than this number of off-map squares, if there
    ; are other map squares we could fit on the screen by scrolling in
    ; the other direction.

  (setv adj (if (% sdim 2) 0 -1))
  (when (and (not wrap) (>= sdim (+ mdim (* 2 border))))
    (return #((- (// mdim 2) (// sdim 2)) (+ (// mdim 2) (// sdim 2) adj))))
  (setv lo (- fc (// sdim 2)))
  (setv hi (+ fc (// sdim 2) adj))
  (when wrap
    (return #(lo hi)))
  (cond
    (< lo (- border))
      #((- border) (- sdim border 1))
    (>= hi (+ mdim border))
      #((- mdim (- sdim border)) (+ mdim border -1))
    T
      #(lo hi)))

(defn overwrapped [wraps? mc fc mdim]
  (unless wraps?
    (return F))
  (setv d (abs (- mc fc)))
  (or
    (> d (// mdim 2))
    (and (= d (// mdim 2)) (< fc mc))))
      ; When `(= d (// mdim 2))`, we only consider the greater-
      ; coordinate case to be overwapped. Thus, each `Pos` has exactly
      ; one (x, y) pair that is considered not to be overwrapped.


(setv status-bar-lines 2)

(defn draw-status-bar []
  "Return each line of status bar, as a colorstr."

  (defn j [#* xs]
    (reduce
      (fn [a b] (+ a (colorstr "  ") b))
      (gfor
        x xs
        :if (is-not x None)
        (if (is (type x) str) (colorstr x) x))))

  (defn status-effects [bad]
    (gfor
      se StatusEffect
      :if (= se.bad bad)
      :if (get G.player.status-effects se)
      (.format "{} {}" se.name (get G.player.status-effects se))))

  #(
    (j
      (.format "HP {:,}"
        G.player.hp)
      (when G.level.poison-intensity
        (+ "☠ " (mixed-number G.level.poison-intensity)))
      None ; Reserved for time limits, like "Tm 1,000"
      (when G.player.keys
        (.format "⚷ {}" G.player.keys))
      (when G.player.magic-arrows (+
        (color-tile (get Tile.types "magic arrows"))
        (colorstr (format G.player.magic-arrows ","))))
      (+ #* (gfor
        item G.player.inventory
        (if item (color-tile item) (colorstr "  "))))
      #* (status-effects :bad T))
    (j
      (.format "DL {:,}"
        G.level-n)
      (.format "Turn {:,}{}"
        G.turn-n
        ; When the current game state isn't the last (because the
        ; player has undone one or more states), count the number of
        ; redoable states as a negative number. This may not equal
        ; the difference in `G.turn-n` due to e.g.
        ; `PlayerStatus.Fast`.
        (if (= G.state-i (- (len G.states) 1))
          ""
          (.format " ({})" (- G.state-i (- (len G.states) 1)))))
      (.format "Score {:,}"
        G.score)
      (+ #* (gfor
        [stem has-artifact] (.items G.player.artifacts)
        (if has-artifact
          (color-tile (get Tile.types stem))
          (colorstr "  "))))
      #* (status-effects :bad F))))

(defn tile-menu [tiles pickable?]
  "Return a list of colorstrs, one for each tile."
  ; Create the text for each line.
  (setv lines (lfor
    [i item] (enumerate tiles)
    (.format " {}� {}  "
      (if pickable? (+ "(" (get menu-letters i) ") ") "")
      (if item item.full-name "---"))))
  ; Pad out short lines and replace the "�"s with colored mapsyms.
  (lfor
    [line item] (zip lines tiles)
    :setv cs (colorstr-to-width (colorstr line) (max (map len lines)))
    :setv (cut cs (.index line "�") (+ (.index line "�") 1))
      (if item (color-tile item) (colorstr "  "))
    cs))


(defn mapsym-at-pos [p]
  "Get a length-2 colorstr to represent the given `Pos`."

  (setv floor-mapsym ". ")

  (setv stack (at p))
  (setv out (colorstr floor-mapsym))
  (for [tile (reversed stack)]
    (setv out (lfor
      [below above] (zip out (color-tile tile))
      (if (= above.char " ")
        (ColorChar below.char below.fg above.bg)
        above))))
  (when (=
      (dist p G.player.pos)
      (+ G.rules.reality-bubble-size 1))
    (for [c out]
      (if (= (uncolor out) "██")
        (setv c.fg color.reality-fringe-block)
        (setv c.bg (or c.bg color.reality-fringe)))))
  out)


(defn color-tile [t]
  "Return a two-item colorstr."
  (lfor
    i [0 1]
    (ColorChar
      (get t.mapsym i)
      (if (isinstance t.color tuple) (get t.color i) t.color)
      (cond
        (isinstance t.color-bg tuple)
          (get t.color-bg i)
        t.color-bg
          t.color-bg
        (and t.damageable (is-not t G.player))
          ; Monsters etc. get an automatic background color to show
          ; how many HP they have.
          (.get color.tile-bg-by-hp
            t.hp
            (get color.tile-bg-by-hp "other"))))))
