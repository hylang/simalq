(import
  hy.pyops *
  simalq.color :as color
  simalq.game-state [G]
  simalq.geometry [Pos at dist])
(setv  T True  F False)


(defn draw-screen [width height]
  "Return a list of lines. Each line is a list of (foreground color,
  background color, character) tuples."

  (setv the-map (draw-map
    width
    (- height status-bar-lines)))
  (setv status-bar (lfor line (draw-status-bar) (lfor
    c (cut (.ljust line width) width)
    #(color.default-fg color.default-bg c))))
  (+ the-map status-bar))


(defn draw-map [width height]
  (lfor
    ; Loop over the screen coordinates `sy` and `sx`. I number `sy`
    ; bottom-to-top, although it's returned top-to-bottom, for
    ; similarity with `Pos`.
    sy (reversed (range height))
    (lfor sx (range 0 width 2)
    ; Find the map coordinates `mx` and `my`, using the rule
    ; that the screen should be centered on the player.
    :setv mx (+ G.player.pos.x (- (// sx 2) (// width 4)))
    :setv my (+ G.player.pos.y (- sy (// height 2)))
    ; Get the two characters and their colors for the corresponding
    ; mapsym.
    [i [color-fg color-bg mapsym]] (enumerate
      (if (and
          (or G.map.wrap-x (<= 0 mx (- G.map.width 1)))
          (or G.map.wrap-y (<= 0 my (- G.map.height 1))))
        ; We're on the map. Draw this square.
        (mapsym-at-pos (Pos G.map mx my))
        ; Otherwise, we're off the map. Draw border.
        (* [#(color.void color.void "█")] 2)))
    ; If the screen has an odd width, we can only draw the first
    ; character of the rightmost mapsym.
    :if (not (and (% width 2) (= sx (- width 1)) i))
    ; Yield one character at a time.
    #(color-fg color-bg mapsym))))


(setv status-bar-lines 2)

(defn draw-status-bar []
  "Return each line of status bar."

  (defn j [#* xs]
    (.join "  " (gfor  x xs  :if (is-not x None)  x)))

  #(
    (j
      (.format "HP {:,}{}"
        G.player.hp
        "") ; Reserved for showing damage, like " (-999)"
      (when G.level.poison-intensity
        (+ "☠ " (str G.level.poison-intensity)))
      None ; Reserved for time limits, like "Tm 1,000"
      (when G.player.keys
        (.format "⚷ {}" G.player.keys))
      None) ; Reserved for three inventory items (2 characters apiece)
    (j
      (.format "Turn {:,}{}"
        G.turn-n
        (if (= G.turn-n (. G.states [-1] turn-n))
          ""
          (.format " ({})" (- G.turn-n (. G.states [-1] turn-n)))))
      (.format "Score {:,}"
        G.score)
      None))) ; Reserved for status-effect indicators


(defn mapsym-at-pos [p]
  (setv floor-mapsym ". ")
  (setv multiple-tile-mapsym "&&")

  (setv stack (at p))
  (setv player-on-top F)
  (when (and stack (is (get stack 0) G.player))
    (setv stack (cut stack 1 None))
    (setv player-on-top T))
  (setv color-fg (* [color.default-fg] 2))
  (setv color-bg (* [None] 2))
  (cond
    (not stack) (setv
      mapsym floor-mapsym)
    (= (len stack) 1) (setv
      t (get stack 0)
      mapsym t.mapsym
      color-fg (if (isinstance t.color tuple)
        (list t.color)
        [t.color t.color])
      color-bg (if (isinstance t.color-bg tuple)
        (list t.color-bg)
        [t.color-bg t.color-bg]))
    True (setv
      mapsym multiple-tile-mapsym))
  (when player-on-top
    (setv mapsym (+ (get G.player.mapsym 0) (get mapsym 1)))
    (setv (get color-fg 0) color.default-fg))
  (when (=
      (dist p G.player.pos)
      (+ G.rules.reality-bubble-size 1))
    (if (= mapsym "██")
      (setv color-fg (* [color.reality-fringe-block] 2))
      (setv color-bg (map or
        color-bg
        (* [color.reality-fringe] 2)))))
  (setv color-bg (map or
    color-bg
    (* [color.default-bg] 2)))
  (zip color-fg color-bg mapsym))
