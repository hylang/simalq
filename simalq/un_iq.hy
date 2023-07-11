"Parse IQ quest files."

;; --------------------------------------------------------------
;; * Imports
;; --------------------------------------------------------------

(require
  hyrule [unless])
(import
  types [FunctionType]
  fractions [Fraction]
  zipfile [ZipFile]
  functools [cache]
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

(eval-when-compile (defn replace-atoms [x f]
  (import hyrule [coll?])
  (if (coll? x)
    ((type x) (gfor  elem x  (replace-atoms elem f)))
    (f x))))

(defmacro with-construct [#* body]
  "Replace all symbols starting with an uppercase letter, like
  `Foo`, with `construct.Foo`, plus `this`, minus booleans and
  `None`."
  `(do ~@(replace-atoms body (fn [x]
    (if (or
          (= x 'this)
          (and
            (isinstance x hy.models.Symbol)
            (.isupper (get x 0))
            (not-in x '[True False T F None])))
      `(. construct ~x)
      x)))))

(defn sym-struct [#* args]
  "Create a `construct.Struct`, naming the fields with Hy symbols."
  (setv  args (list args)  new [])
  (while args
    (setv k (when (isinstance (get args 0) hy.models.Symbol)
      (hy.mangle (.pop args 0))))
    (setv v (.pop args 0))
    (.append new (if k (/ k v) v)))
  (construct.Struct #* new))

(defmacro adapt [old-cls #* body]
  "Syntactic sugar for `construct.Adapter`."
  (setv C (hy.gensym))
  `(do
    (defclass ~C [construct.Adapter]
      (defn _decode [self obj context path]
        ~@body))
    (~C ~old-cls)))

(defn mac-roman-str [n]
  "A Mac Roman-encoded string of length `n` with Mac-style newlines."
  (adapt
    (construct.Bytes n)
    (.replace (.decode obj "mac_roman") "\r" "\n")))

(defn iq-str [n]
  "A length-prefixed string that may have trailing junk data."
  (with-construct (FocusedSeq "string"
    (/ "len" Byte)
    (/ "string" (mac-roman-str this.len))
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

(setv quest-fmt (with-construct (sym-struct
  'n-levels Byte
  (Bytes 261)
  'starting-hp Int16ub
  'title (iq-str 256)
  (Bytes 897)
  'levels (Array this.n-levels (sym-struct
    'title (iq-str 128)
    'floor-tile Byte
    'width Byte
    'height Byte
    'player-start iq-pos
    'n-tile-extras Int16ub
    'wrap-x big-bool
    'wrap-y big-bool
    'next-level Int16ub
    'light Int16ub
    'poison-interval Int16ub
    'time-limit Int16ub
    'exit-speed Int16ub
    (Const (bytes [0]))
    'wall-tile Byte
    'moving-exit-start iq-pos
    'map (Bytes (* this.width this.height))
    'tile-extras (Array this.n-tile-extras (sym-struct
      'pos iq-pos
      'data (Bytes 2)))))
  Terminated)))

;; --------------------------------------------------------------
;; * `iq-quest`
;; --------------------------------------------------------------

(defn iq-quest [quest-name]
  "Get the given original quest IQ, as a `Quest`. Or use the symbol
  `all` to get all quests as a dictionary."
  (if (= quest-name 'all)
    (dfor
      [k v] (.items (iq-quests-raw))
      k (read-quest k v))
    (read-quest quest-name (get (iq-quests-raw) quest-name))))

(defn [cache] iq-quests-raw []
  "Get a dictionary of raw IQ quests as `bytes` objects."

  ; Download the quests if needed.
  (.mkdir cache-dir :exist-ok T)
  (setv path (/ cache-dir "infinity_quests_2.zip"))
  (unless (.exists path)
    (import http.client contextlib)
    (with [con (contextlib.closing (http.client.HTTPConnection "arfer.net"))]
      (.request con "GET" "/downloads/infinity_quests_2.zip"
        :headers {"User-Agent" "Infinitesimal Quest 2 + epsilon"})
      (setv r (.getresponse con))
      (assert (= r.status 200))
      (.write-bytes path (.read r))))

  ; Read the files from the archive.
  (setv prefix "infinity_quests_2/")
  (with [z (ZipFile path "r")] (dfor
    q (.namelist z)
    :setv v (.read z q)
    :if v
    (.removeprefix q prefix) v)))

(defn read-quest [name inp]
  "Parse `bytes` into a `Quest`."

  (setv data (.parse quest-fmt inp))
  (Quest
    :name name
    :title data.title
    :authors (if (= name "New DeathQuest")
      "Yves and Serge Meynard"
      "Yves Meynard")
    :starting-hp data.starting-hp
    :levels (tuple (gfor
      [i l] (enumerate data.levels)

      :setv m (Map.make l.wrap-x l.wrap-y l.width l.height)
      :setv mk-pos (fn [xy]
        "Convert from IQ coordinates (1-based indices, y = 1 on top, 0
        means missing) to SQ coordinates (0-based indices with y = 0 on
        bottom, None means missing)."
        (if (and #* xy)
          (Pos m (- (get xy 0) 1) (- m.height (get xy 1)))
          None))
      :setv tile-extras (dfor
        c l.tile-extras
        (mk-pos c.pos) (tuple c.data))

      :do (for [x (range l.width)  y (range l.height)]
        ; Fill in `m`.
        (setv iq-ix (get l.map (+ (* x l.height) (- l.height y 1))))
          ; We have to reverse y-coordinates to get y = 0 as the
          ; bottom row.
        (unless (= iq-ix FLOOR)
          (setv p (Pos m x y))
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
              ; a slot.
              [((get result "cls") :pos p
                #** {(get result "slot") (get result "value")}
                #** (te (get result "cls")))]
            (callable (type result))
              ; A special case where a callback makes the tiles
              ; itself. It can return any number of them.
              (result p #* (get tile-extras p))
            T
              (raise (ValueError (+ "Bad `Tile.types-by-iq-ix` entry: " (repr result))))))))

      (Level
        :n (+ i 1)
        :title l.title
        :player-start (mk-pos l.player-start)
        :next-level l.next-level
        :poison-intensity (if (= l.poison-interval 0)
          (Fraction 0)
          (Fraction 1 l.poison-interval))
        :time-limit l.time-limit
        :exit-speed l.exit-speed
        :moving-exit-start (mk-pos l.moving-exit-start)
        :map m)))))
