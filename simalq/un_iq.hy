"Import IQ quest files."

;; --------------------------------------------------------------
;; * Imports
;; --------------------------------------------------------------

(require
  hyrule [unless case])
(import
  types [FunctionType]
  fractions [Fraction :as f/]
  zipfile [ZipFile]
  functools [cache]
  hyrule [thru]
  construct
  toolz [partition]
  simalq.util [cache-dir]
  simalq.geometry [Map Pos]
  simalq.quest [Quest Level]
  simalq.tile [Tile])
(setv  T True  F False)

;; --------------------------------------------------------------
;; * Helpers
;; --------------------------------------------------------------

(setv FLOOR 1)
  ; The IQ index number for plain floor.

(defmacro with-construct [#* body]
  "Replace all symbols starting with an uppercase letter, like
  `Foo`, with `construct.Foo`, plus `this`, minus booleans and
  `None`."
  `(do ~@(hy.I.hyrule.map-model body (fn [x]
    (when (or
          (= x 'this)
          (and
            (isinstance x hy.models.Symbol)
            (.isupper (get x 0))
            (not-in x '[True False T F None])))
      `(. construct ~x))))))

(defmacro kw-struct [#* args]
  "Create a `construct.Struct`, naming the fields with Hy keyword
  objects."
  `(construct.Struct ~@((fn []
    (setv xs (list args))
    (while xs
      (setv x (.pop xs 0))
      (yield (if (isinstance x hy.models.Keyword)
        `(/ ~(hy.mangle x.name) ~(.pop xs 0))
        x)))))))

(defmacro adapt [old-cls #* body]
  "Syntactic sugar for `construct.Adapter`."
  `(
    (type "adapter" #(construct.Adapter) (dict
      :_decode (fn [self obj context path]
        ~@body)))
    ~old-cls))

(defn mac-roman-str [n]
  "A Mac Roman-encoded string of length `n` with Mac-style newlines."
  (adapt
    (construct.Bytes n)
    (.replace (.decode obj "mac_roman") "\r" "\n")))

(defn iq-str [n]
  "A length-prefixed string (with skulls for bullets) that may have
  trailing junk data."
  (with-construct (FocusedSeq "string"
    (/ "len" Byte)
    (/ "string" (adapt (mac-roman-str this.len) (.replace obj "•" "☠")))
      ; IQ uses a font that displays bullets as skulls. We imitate it
      ; thus. The skull emoji (U+1F480, "💀") is arguably a closer
      ; match in design, but emoji in terminals sometimes cause funky
      ; spacing.
    (Bytes (- n 2 this.len)))))

(setv big-bool
  ; A Boolean encoded as the last bit of a 2-byte sequence.
  (adapt
    construct.Int16ub
    (get {0 F  1 T} obj)))

(setv iq-pos
  ; Coordinates for a map position, encoded as a pair of bytes.
  (adapt
    (with-construct (Sequence Byte Byte))
    (tuple obj)))

;; --------------------------------------------------------------
;; * The Construct format for quest files
;; --------------------------------------------------------------

(setv quest-fmt (with-construct (kw-struct
  :n-levels Byte
  (Bytes 261)
  :starting-hp Int16ub
  :title (iq-str 256)
  (Bytes 897)
  :levels (Array this.n-levels (kw-struct
    :title (iq-str 128)
    :floor-image Byte
    :width Byte
    :height Byte
    :player-start iq-pos
    :n-tile-extras Int16ub
    :wrap-x big-bool
    :wrap-y big-bool
    :next-level Int16ub
    :light Int16ub
    :poison-interval Int16ub
    :time-limit Int16ub
    :exit-speed Int16ub
    (Const (bytes [0]))
    :wall-image Byte
    :moving-exit-start iq-pos
    :map (Bytes (* this.width this.height))
    :tile-extras (Array this.n-tile-extras (kw-struct
      :pos iq-pos
      :data (Bytes 2)))))
  Terminated)))

;; --------------------------------------------------------------
;; * `iq-quest`
;; --------------------------------------------------------------

(defn iq-quest [quest-name]
  "Get the given original IQ quest, as a `Quest`. Or pass in the
  symbol `all` to get all quests as a dictionary."
  (if (= quest-name 'all)
    (dfor
      k (.keys (iq-quests-raw))
      k (read-quest k))
    (read-quest quest-name)))

(defn [cache] iq-quests-raw []
  "Get a dictionary of raw IQ quests as `bytes` objects."

  ; Download the quests if needed.
  (.mkdir cache-dir :parents T :exist-ok T)
  (setv path (/ cache-dir "infinity_quests_2.zip"))
  (unless (.exists path)
    (import http.client contextlib hashlib)
    (setv data (with [con
        (contextlib.closing (http.client.HTTPConnection "arfer.net"))]
      (.request con "GET" "/downloads/infinity_quests_2.zip"
        :headers {"User-Agent" "Infinitesimal Quest 2 + epsilon"})
      (setv r (.getresponse con))
      (when (!= r.status 200)
        (raise (ValueError f"The HTTP request to get IQ quests returned status code {r.status}")))
      (.read r)))
    (setv value (.hexdigest (hashlib.sha512 data)))
    (unless (= value "044b0c16fcc3bda2869d5b97ecaf32b39177988e510230e44456a33485c2df4e929443b9765463654961b7190ecf431a6871417a52c81e81c61f089d2793ab5d")
      (raise (ValueError f"Bad SHA-512 hash for IQ quests: {value}")))
    (.write-bytes path data))

  ; Read the files from the archive.
  (with [z (ZipFile path "r")] (dfor
    member (.namelist z)
    :if (not (.endswith member "/"))
    (.removeprefix member "infinity_quests_2/") (.read z member))))

(defn [cache] read-quest [name]
  "Parse `bytes` from the named element of `iq-quests-raw` into a
  `Quest`."

  (setv data (.parse quest-fmt (get (iq-quests-raw) name)))
  (Quest
    :name name
    :authors (if (= name "New DeathQuest")
      ; We're assuming this is a standard IQ quest, not user-made.
      "Yves and Serge Meynard"
      "Yves Meynard")
    :title data.title
    :starting-hp data.starting-hp
    :levels (tuple (gfor
      [level-n l] (enumerate data.levels)
      :do (+= level-n 1)

      :setv m (Map.make
        l.wrap-x l.wrap-y
        ; Trim border rows of Void that merely pad out the map to IQ's
        ; minimal dimensions. We don't bother to do this for Boot Camp 2
        ; because we only use that quest for testing and don't offer it
        ; for play.
        :width (- l.width (if (and (= name "BoneQuest") (= level-n 11))
          3
          0))
        :height (- l.height (cond
          (and (= name "BoneQuest") (= level-n 11))  3
          (and (= name "Delirium")  (= level-n  4))  1
          True                                       0)))
      :setv mk-pos (fn [xy]
        "Convert from IQ coordinates (1-based indices, y = 1 on top, 0
        means missing) to SQ coordinates (0-based indices with y = 0 on
        bottom, None means missing)."
        (if (and #* xy)
          (Pos m (- (get xy 0) 1) (- m.height (get xy 1)))
          None))
      :setv iq-ixes (dfor
        ix (thru 1 m.width)
        iy (thru 1 m.height)
        (mk-pos #(ix iy)) (get l.map (+ (* (- ix 1) l.height) (- iy 1))))
      :setv tile-extras (dfor
        pair l.tile-extras
        (mk-pos pair.pos) (tuple pair.data))

      :do (for [x (range m.width)  y (range m.height)]
        ; Fill in `m`.
        (setv p (Pos m x y))
        (setv iq-ix (get iq-ixes p))
        (when (= iq-ix FLOOR)
          (continue))
        (setv result (get Tile.types-by-iq-ix iq-ix))
        (defn te [cls]
          (if (in p tile-extras)
            (.read-tile-extras cls mk-pos
              #* (get tile-extras p))
            {}))
        (.extend (get m.data x y) (cond
          (is (type result) type)
            ; The usual case: the `iq-ix` specifies the type, and no
            ; more.
            [(result :pos p #** (te result))]
          (is (type result) dict)
            ; This `iq-ix` specifies the type and a certain value of
            ; a field.
            [((get result "cls") :pos p
              #** {(get result "field") (get result "value")}
              #** (te (get result "cls")))]
          (callable (type result))
            ; A special case where a callback makes the tiles
            ; itself. It can return any number of them.
            (result p #* (get tile-extras p))
          T
            (raise (ValueError (+ "Bad `Tile.types-by-iq-ix` entry: " (repr result)))))))

      (denazify name (Level
        :n level-n
        :title l.title
        :player-start (mk-pos l.player-start)
        :next-level l.next-level
        :poison-intensity (if (= l.poison-interval 0)
          (f/ 0)
          (f/ 1 l.poison-interval))
        :time-limit (or l.time-limit None)
        :exit-speed l.exit-speed
        :moving-exit-start (mk-pos l.moving-exit-start)
        :map m))))))

;; --------------------------------------------------------------
;; * Denazification
;; --------------------------------------------------------------

(defn denazify [quest-name level]
  "Remove swastika-based designs in the given IQ level. The level is
  modified in place and then returned."
  (case [quest-name level.n]

    ["New Nightmare" 1] (do
      ; Replace the large central swastika.
      (replace-map-rect level 10 10
        :text "
          . . . . . ██{}██. . . . .
          . . . ██##██{}██##██. . .
          . . ████d ██☉G██d ████. .
          . ████d d ██. ██d d ████.
          . ██% d d ██. ██d d % ██.
          ████████████+↑████████████
          {}{}☉G. . +←. +→. . ☉G{}{}
          ████████████+↓████████████
          . ██% d d ██. ██d d % ██.
          . ████d d ██. ██d d ████.
          . . ████d ██☉G██d ████. .
          . . . ██##██{}██##██. . .
          . . . . . ██{}██. . . . ."
        :map-marks {
          "% " "meal"
          "d " ["devil" :hp 3]
          "☉G" ["generator"
            :summon-class "ghost"
            :summon-frequency (f/ 2 3)
            :summon-hp 2]
          "##" ["cracked wall"]
          "{}" "gate"})
      ; Set targets for the gates.
      (for [[x1 y1 x2 y2] (partition 4 [
          16 11  3  3  11 16  3 29  16 21 29 29  21 16 29  3
          16 10 16 16  10 16 16 16  16 22 16 16  22 16 16 16])]
        (object.__setattr__ (get level.map.data x1 y1 0)
          "target" (Pos level.map x2 y2))))

    ["New Nightmare" 24] (do
      ; Replace the 5-by-5 trap swastikas.
      (for [[x y] [[12 29] [25 30] [25 18] [11 7] [26 6]]]
        (replace-map-rect level x y
          :text "
            <>. . . <>
            <>. . . <>
            <><><><><>
            <>. . . <>
            <>. . . <>"
          :map-marks {
            "<>" "fixed damaging trap"}))
      ; Reconfigure the room in the northwest corner.
      (replace-map-rect level 2 32
        :text "
          <1████K ██████
          <>████G ████K
          <>████G ████G
          ++G G k ██k G
          <>████████G ██
          <>████████G ██
          <><><><><>dd<>"
        :map-marks {
          "++" "door"
          "dd" "locked disappearing door"
          "<1" ["wallfall trap" :wallnum 1]
          "██" ["trapped wall" :wallnum 1]
          "<>" "fixed damaging trap"
          "k " "key"
          "G " ["ghost" :hp 3]
          "K " ["Dark Knight" :hp 12]}))

    ["BoneQuest" 3]
      ; Replace the two trapped-wall swastikas.
      (replace-map-rect level 13 24
        :text "
          . ████. i ██████. . ██
          . ████. ████████. ████
          i . <>. . . . . <>. ██
          . ██████. ████████. .
          . . . ██i . . ██████i "
        :map-marks {
          "██" ["trapped wall" :wallnum 1]
          "i " ["imp" :hp 3]
          "<>" "one-shot damaging trap"})

    ["BoneQuest" 8]
      ; Replace the large central swastika.
      (replace-map-rect level 6 6
        :text "
          ██, ███4███████1██████
          ██. ██. ██. ██. ██. ,
          ██d ██☉G██d ██☉G██d ██
          ██. ██> ██. ██> ██. ██
          ██o ██████, ██████o ██
          ██. d . , . , . d . ██
          ██████████, ██████████
          █3. ☉G> ██. ██> ☉G. █2
          ██████████d ██████████
          , . d . o . o . d . ██
          ██████████████████, ██"
        :map-marks {
          #** (dfor
            i (thru 1 4)
            f"█{i}" ["trapped wall" :wallnum i])
          ", " "broken trap"
          "o " ["orc" :hp 3]
          "☉G" ["generator"
            :hp 3
            :summon-class "ghost"
            :summon-frequency (f/ 1)
            :summon-hp 3]}))

  level)

(defn replace-map-rect [old x y text map-marks]
  "Replace a rectangular region of the level `old`. `x` and `y` are
  the coordinates of where (0, 0) of the new level should be placed in
  `old`."
  (import simalq.quest-definition [parse-text-map])

  (setv [new-map _] (parse-text-map text map-marks))
  (for [xn (range new-map.width)  yn (range new-map.height)]
    (for [tile (get old.map.data (+ xn x) (+ yn y))]
      (.rm-from-map tile))
    (for [tile (get new-map.data xn yn)]
      (.move tile (Pos old.map (+ xn x) (+ yn y))))))
