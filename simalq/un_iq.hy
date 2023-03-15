"Parse IQ quest files."

;; --------------------------------------------------------------
;; * Imports
;; --------------------------------------------------------------

(require
  hyrule [unless])
(import
  os
  pathlib [Path]
  zipfile [ZipFile]
  construct
  toolz [partition]
  simalq.geometry [Map Pos]
  simalq.quest [Quest Level]
  simalq.tile [tile-types-by-iq-ix])
(setv  T True  F False)

;; --------------------------------------------------------------
;; * Helpers for Construct
;; --------------------------------------------------------------

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
  'starting-life Int16ub
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
;; * `read-quest`, `iq-file`
;; --------------------------------------------------------------

(defn read-quest [inp]
  (setv data (.parse quest-fmt inp))

  (defn mk-pos [m xy]
    "Convert from IQ coordinates (1-based indices, y = 1 on top, 0
    means missing) to SQ coordinates (0-based indices with y = 0 on
    bottom, None means missing)."
    (if (and #* xy)
      (Pos m (- (get xy 0) 1) (- m.height (get xy 1)))
      None))

  (Quest
    :title data.title
    :starting-life data.starting-life
    :levels (tuple (gfor
      [i l] (enumerate data.levels)
      :setv m (Map l.wrap-x l.wrap-y (lfor
        column (partition l.height l.map)
        (lfor
          iq-ix (reversed column)
            ; Reversed so that y = 0 is the bottom row.
          (get tile-types-by-iq-ix iq-ix))))
      (Level
        :n (+ i 1)
        :title l.title
        :player-start (mk-pos m l.player-start)
        :next-level l.next-level
        :poison-interval l.poison-interval
        :time-limit l.time-limit
        :exit-speed l.exit-speed
        :moving-exit-start (mk-pos m l.moving-exit-start)
        :map m)))))


(defn iq-quest [quest-name]
  "Get the given original quest file from IQ, as a raw `bytes`
  object."

  ; Download the quests if needed.
  (assert (get os.environ "XDG_CACHE_HOME"))
  (setv path (/ (Path (get os.environ "XDG_CACHE_HOME")) "simalq"))
  (.mkdir path :exist-ok T)
  (setv path (/ path "infinity_quests_2.zip"))
  (unless (.exists path)
    (import http.client contextlib)
    (with [con (contextlib.closing (http.client.HTTPConnection "arfer.net"))]
      (.request con "GET" "/downloads/infinity_quests_2.zip")
      (setv r (.getresponse con))
      (assert (= r.status 200))
      (.write-bytes path (.read r))))

  ; Get the requested quest (heh).
  (with [z (ZipFile path "r")]
    (.read z (+ "infinity_quests_2/" quest-name))))
