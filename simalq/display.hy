(require
  simalq.macros [defdataclass])
(import
  hy.pyops *
  simalq.color :as color
  simalq.game-state [G]
  simalq.geometry [Pos at dist])
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


(defn draw-screen [width height focus status-bar message]
  "Return a colorstr for the main screen."

  (setv the-map (draw-map
    focus
    width
    (- height (if status-bar status-bar-lines 0))))
  (when (and message (not-in message hide-messages))
    ; Write the message over the last line of the map.
    (setv message (cut (+ message " ") width))
    (setv (cut (get the-map -1) (len message))
      (colorstr message None color.message-bg)))
  (setv status-bar (if status-bar
    (lfor
      line (draw-status-bar)
      (colorstr (cut (.ljust line width) width)))
     []))
  (+ #* (+ the-map status-bar)))


(defn draw-map [focus width height]
  "Return a list of colorstrs, one per line. The map is centered on
  `focus`, a `Pos`."

  (lfor
    ; Loop over the screen coordinates `sy` and `sx`. I number `sy`
    ; bottom-to-top, although it's returned top-to-bottom, for
    ; similarity with `Pos`.
    sy (reversed (range height))
    (lfor sx (range 0 width 2)
    ; Find the map coordinates `mx` and `my`.
    :setv mx (+ focus.x (- (// sx 2) (// width 4)))
    :setv my (+ focus.y (- sy (// height 2)))
    ; Get the two characters and their colors for the corresponding
    ; mapsym.
    [i c] (enumerate
      (if (and
          (or G.map.wrap-x (<= 0 mx (- G.map.width 1)))
          (or G.map.wrap-y (<= 0 my (- G.map.height 1))))
        ; We're on the map. Draw this square.
        (mapsym-at-pos (Pos G.map mx my))
        ; Otherwise, we're off the map. Draw border.
        (colorstr "██" color.void)))
    :do (when (= [mx my] [focus.x focus.y])
      (setv c.bg color.focus))
    ; If the screen has an odd width, we can only draw the first
    ; character of the rightmost mapsym.
    :if (not (and (% width 2) (= sx (- width 1)) i))
    ; Yield one `ColorChar` at a time.
    c)))


(setv status-bar-lines 2)

(defn draw-status-bar []
  "Return each line of status bar, as a string."

  (defn j [#* xs]
    (.join "  " (gfor  x xs  :if (is-not x None)  x)))

  #(
    (j
      (.format "HP {:,}"
        G.player.hp)
      (when G.level.poison-intensity
        (+ "☠ " (str G.level.poison-intensity)))
      None ; Reserved for time limits, like "Tm 1,000"
      (when G.player.keys
        (.format "⚷ {}" G.player.keys))
      None) ; Reserved for inventory items (2 characters apiece)
    (j
      (.format "DL {:,}"
        G.level-n)
      (.format "Turn {:,}{}"
        G.turn-n
        (if (= G.turn-n (. G.states [-1] turn-n))
          ""
          (.format " ({})" (- G.turn-n (. G.states [-1] turn-n)))))
      (.format "Score {:,}"
        G.score)
      None))) ; Reserved for status-effect indicators


(defn mapsym-at-pos [p]
  "Get a length-2 colorstr to represent the given `Pos`."

  (setv floor-mapsym ". ")
  (setv multiple-tile-mapsym "&&")

  (setv stack (at p))
  (setv player-on-top F)
  (when (and stack (is (get stack 0) G.player))
    (setv stack (cut stack 1 None))
    (setv player-on-top T))
  (setv out (cond
    (not stack)
      (colorstr floor-mapsym)
    (= (len stack) 1)
      (color-tile (get stack 0))
    True
      (colorstr multiple-tile-mapsym)))
  (when player-on-top
    (setv (get out 0) (ColorChar
      (get G.player.mapsym 0)
      G.player.color
      G.player.color-bg)))
  (when (=
      (dist p G.player.pos)
      (+ G.rules.reality-bubble-size 1))
    (for [c out]
      (if (= (uncolor out) "██")
        (setv c.fg color.reality-fringe-block)
        (setv c.bg (or c.bg color.reality-fringe)))))
  out)


(defn color-tile [t]
  (lfor
    i [0 1]
    (ColorChar
      (get t.mapsym i)
      (if (isinstance t.color tuple) (get t.color i) t.color)
      (if (isinstance t.color-bg tuple) (get t.color-bg i) t.color-bg))))
