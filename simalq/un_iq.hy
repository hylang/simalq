(require
  hyrule [unless])
(import
  os
  pathlib [Path]
  construct
  machfs
  toolz [partition]
  simalq.tile [tile-types-by-iq-ix])
(setv  T True  F False)


(eval-when-compile (defn replace-atoms [x f]
  (import hyrule [coll?])
  (if (coll? x)
    ((type x) (gfor  elem x  (replace-atoms elem f)))
    (f x))))


(defmacro with-construct [#* body]
  ; Replaces all symbols starting with an uppercase letter, like
  ; `Foo`, with `construct.Foo`.
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
  (setv  args (list args)  new [])
  (while args
    (setv k (when (isinstance (get args 0) hy.models.Symbol)
      (hy.mangle (.pop args 0))))
    (setv v (.pop args 0))
    (.append new (if k (/ k v) v)))
  (construct.Struct #* new))

(defmacro adapt [old-cls #* body]
  (setv C (hy.gensym))
  `(do
    (defclass ~C [construct.Adapter]
      (defn _decode [self obj context path]
        ~@body))
    (~C ~old-cls)))

(defn mac-roman-str [n] (adapt (construct.Bytes n)
  (.replace (.decode obj "mac_roman") "\r" "\n")))
(defn iq-str [n] (with-construct (FocusedSeq "string"
  (/ "len" Byte)
  (/ "string" (mac-roman-str this.len))
  (Bytes (- n 2 this.len)))))

(setv big-bool (adapt construct.Int16ub
  ; A Boolean encoded as the last bit of a 2-byte sequence.
  (get {0 F  1 T} obj)))

(defn iq-coords [xy map-height]
  ; Convert from IQ coordinates (1-based indices, y = 1 on top, 0
  ; means missing) to SQ coordinates (0-based indices with y = 0 on
  ; bottom, None means missing).
  (if (and #* xy)
    #((- (get xy 0) 1) (- map-height (get xy 1)))
    None))

(defmacro coords [map-height] `(adapt
  (with-construct (Sequence Byte Byte))
  (iq-coords obj ~map-height)))


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
    'start (coords context.height)
    'n-tile-extras Int16ub
    'wrap-x big-bool
    'wrap-y big-bool
    'next-level Int16ub
    'light Int16ub
    'poison Int16ub
    'time Int16ub
    'exit-speed Int16ub
    (Const (bytes [0]))
    'wall-tile Byte
    'moving-exit-start (coords context.height)
    'map (adapt
      (Bytes (* this.width this.height))
      (lfor
        column (partition context.height obj)
        (lfor
          iq-ix (reversed column)
            ; Reversed so that y = 0 is the bottom row.
          (get tile-types-by-iq-ix iq-ix))))
    'tile-extras (Array this.n-tile-extras (sym-struct
      'pos (coords context._.height)
      'data (Bytes 2)))))
  Terminated)))

(defn read-quest [inp]
  (.parse
    quest-fmt
    (if (isinstance inp machfs.directory.File)
      inp.data
      (.read-bytes (Path inp)))))


(defn iq-file [[file-name None]]
  "Get a `machfs.File` object for the requested file in IQ."
  (setv x (machfs.Volume))
  (.read x (.read-bytes (Path (get os.environ "SIMALQ_IQ2_PATH"))))
  (get x (or file-name "Infinity Quest II 1.0.1")))
