"The `ColorChar` type, and functions to render various game or
interface elements as lists of `ColorChar`s."

;; --------------------------------------------------------------
;; * Imports
;; --------------------------------------------------------------

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
  simalq.tile [Tile Damageable]
  simalq.commands [move-blocked-msgs])
(setv  T True  F False)

;; --------------------------------------------------------------
;; * `ColorChar`
;; --------------------------------------------------------------

(defdataclass ColorChar []
  "A character with a foreground and background color, plus a boldface
  state. Colors can be `None` or a symbol (a key of `color.by-name`).
  `bold` is a `bool`."
  :fields [char fg bg bold])

(defn colorstr [s [fg None] [bg None] [bold F]]
  "Convert a string to a colorstr, i.e., a list of `ColorChar`s."
  (lfor
    c s
    (ColorChar c fg bg bold)))

(defn uncolor [x]
  "Strip out the colors in a colorstr and return a plain string."
  (.join "" (gfor  c x  c.char)))

(setv color-cache {})
  ; We cache color functions because they can be slow to compute in
  ; 256-color mode, in which case `blessed` needs to find an
  ; approximation for each color.
(defn bless-colorstr [B x]
  "Render a colorstr `x` into terminal sequences via the
  `blessed.Terminal` object `B`."
  (.join "" (gfor
    c x
    :setv k #(c.fg c.bg)
    :do (unless (in k color-cache)
      (setv
        fg (B.color-rgb #* (get color.by-name
          (or c.fg color.default-fg)))
        bg (B.on-color-rgb #* (get color.by-name
          (or c.bg color.default-bg)))
        (get color-cache k) (fn [x [fg fg] [bg bg]] (fg (bg x)))))
    ((get color-cache k) (if c.bold (B.bold c.char) c.char)))))

(defn colorstr-to-width [x width]
  (cut (+ x (colorstr (* " " (max 0 (- width (len x)))))) width))

(defn color-tile [t]
  "Return a two-item colorstr."
  (lfor
    i [0 1]
    (ColorChar
      :char (get t.mapsym i)
      :fg (if (isinstance t.color tuple) (get t.color i) t.color)
      :bg (cond
        (isinstance t.color-bg tuple)
          (get t.color-bg i)
        t.color-bg
          t.color-bg
        (and (isinstance t Damageable) (is-not t G.player))
          ; Monsters etc. get an automatic background color to show
          ; how many HP they have.
          (.get color.tile-bg-by-hp
            t.hp
            (get color.tile-bg-by-hp "other")))
      :bold t.bold)))

;; --------------------------------------------------------------
;; * Drawing functions
;; --------------------------------------------------------------

;; --------------------------------------------------------------
;; ** The top level
;; --------------------------------------------------------------

(defn draw-screen [
    width height
      ; Integers
    [target None]
      ; An optional `Pos` to focus the map. Otherwise, we use the
      ; player's current position.
    [tile-list None]
      ; `None`, or the symbols `pickable` or `nonpickable`
    [inventory F]
    [messages #()]
    [overmarks None]]
  "Return a colorstr for the main screen."

  (setv out [])

  ; The status bar is drawn first.
  (+= out (lfor
    line (draw-status-bar)
    (colorstr-to-width line width)))
  (setv status-bar-lines (len out))
  ; Then the map, including overmarks.
  (setv focus (or target G.player.pos))
  (+= out (draw-map
    focus
    (bool target)
    width
    (- height status-bar-lines)
    (or overmarks {})))

  ; Write other parts on top of the map, just under the status bar.
  ; They can overlap each other, should they be enabled
  ; simultaneously.
  (defn scribble-on-map [lines]
    (for [[i line] (enumerate lines)]
      (+= i status-bar-lines)
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

(setv hide-messages #(
  ; These messages are probably too naggy to actually show. They're
  ; still implemented for the sake of testing the corresponding
  ; `CommandError`s.
  move-blocked-msgs.simple
  move-blocked-msgs.map-border))

;; --------------------------------------------------------------
;; ** The map
;; --------------------------------------------------------------

(defn draw-map [focus targeting? width height overmarks]
  "Return a list of colorstrs, one per line. The map will include
  (typically, is centered on) `focus`, a `Pos`. `overmarks` should be
  a dictionary mapping `Pos`es to pairs of `ColorChar`s to display
  over them. The `char` attribute of these `ColorChar`s can be `None`
  to indicate that the map character should be preserved."

  (setv xlim (map-limits
    :sdim (ceil (/ width 2)) :mdim G.map.width
    :fc focus.x :wrap G.map.wrap-x))
  (setv ylim (map-limits
    :sdim height :mdim G.map.height
    :fc focus.y :wrap G.map.wrap-y))

  (lfor
    my (reversed (range (get ylim 0) (+ (get ylim 1) 1)))
    (lfor mx (range (get xlim 0) (+ (get xlim 1) 1))
    ; For this square, get the two characters and their colors.
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
                (overwrapped? G.map.wrap-x mx focus.x G.map.width)
                (overwrapped? G.map.wrap-y my focus.y G.map.height))
              (setv c (ColorChar c.char
                color.overwrapped color.default-bg None))
            (in p overmarks) (do
              ; Overmarks can override what would normally appear.
              (setv o (get overmarks p i))
              (setv c.bg o.bg)
              (when o.char
                (setv c.char o.char)
                (setv c.fg o.fg)
                (setv c.bold o.bold)))
            (= [mx my] [focus.x focus.y])
              ; Mark the focus with a special background color.
              (setv c.bg (cond
                targeting?
                  color.highlight-target
                (= G.player.game-over-state 'dead)
                  ; Use a different color if the player is dead. This
                  ; is the most prominent visual indication of death.
                  color.highlight-player-dead
                T
                  color.highlight-player-living))))
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
    ; We can easily fit this whole map dimension on screen. Ignore the
    ; focus and center the map instead.
    (return #(
      (- (// mdim 2) (// sdim 2))
      (+ (// mdim 2) (// sdim 2) adj))))
  (setv lo (- fc (// sdim 2)))
  (setv hi (+ fc (// sdim 2) adj))
  (cond
    wrap
      #(lo hi)
    (< lo (- border))
      #((- border) (- sdim border 1))
    (>= hi (+ mdim border))
      #((- mdim (- sdim border)) (+ mdim border -1))
    T
      #(lo hi)))

(defn overwrapped? [wraps? mc fc mdim]
  "Are we re-displaying the same square due to wrapping?"
  (unless wraps?
    (return F))
  (setv d (abs (- mc fc)))
  (or
    (> d (// mdim 2))
    (and (= d (// mdim 2)) (< fc mc))))
      ; When `(= d (// mdim 2))`, we only consider the greater-
      ; coordinate case to be overwapped. Thus, each `Pos` has exactly
      ; one (x, y) pair that is considered not to be overwrapped.

(defn mapsym-at-pos [p]
  "Get a length-2 colorstr to represent the given `Pos`."

  (setv floor-mapsym ". ")

  (setv stack (at p))
  (setv out (colorstr floor-mapsym))
  (for [tile (reversed stack)]
    (setv out (lfor
      [below above] (zip out (color-tile tile))
      (if (= above.char " ")
        ; A space character is transparent to the tile below.
        (ColorChar below.char below.fg above.bg below.bold)
        above))))
  (when (=
      (dist p G.player.pos)
      (+ G.rules.reality-bubble-size 1))
    ; Mark the boundary of the reality bubble.
    (for [c out]
      (if (= (uncolor out) "██")
        (setv c.fg color.reality-fringe-block)
        (setv c.bg (or c.bg color.reality-fringe)))))
  out)

;; --------------------------------------------------------------
;; ** The status bar
;; --------------------------------------------------------------

(defn draw-status-bar []
  "Return a tuple of colorstrs, one per line of status bar."

  (defn j [#* xs]
    "Join strings and colorstrs with spaces, ignoring `None`s."
    (reduce
      (fn [a b] (+ a (colorstr "  ") b))
      (gfor
        x xs
        :if (is-not x None)
        (if (is (type x) str) (colorstr x) x))))

  (defn status-effects [bad]
    "Names and remaining durations of status effects."
    (gfor
      se StatusEffect
      :if (= (.bad? se) bad)
      :if (.player-has? se)
      (.format "{} {}" se.name (get G.player.status-effects se))))

  (setv poison (* G.rules.poison-factor G.level.poison-intensity))

  #(
    (j
      (.format "HP {:,}"
        G.player.hp)
      (when poison
        (+ "☠ " (mixed-number poison)))
      (when G.time-left
        (.format "⏲ {:,} (→ {})"
          G.time-left
          (if (> G.level.next-level (len G.quest.levels))
            "victory"
            (.format "DL {:,}" G.level.next-level))))
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

;; --------------------------------------------------------------
;; ** Tile menus
;; --------------------------------------------------------------

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
