(require
  hyrule [unless])


(defmacro defdataclass [class-name superclasses #* rest]
  (setv   rest (list rest)  docstring []  fields []  kwargs [])
  (when (and rest (isinstance (get rest 0) hy.models.String))
    (.append docstring (.pop rest 0)))
  (when rest
    (setv fields (.pop rest 0))
    (assert (isinstance fields hy.models.List)))
  (while (and rest (isinstance (get rest 0) hy.models.Keyword))
    (.append kwargs (.pop rest 0))
    (.append kwargs (.pop rest 0)))
  (unless fields
    (.extend kwargs '[:frozen True]))
  `(defclass
    [(hy.M.dataclasses.dataclass :slots True ~@kwargs)]
    ~class-name ~superclasses
    ~@docstring
    ~@(gfor
      field fields
      `(annotate ~field ...))
    ~@rest))


(defmacro slot-defaults [#* items]
  (import toolz [partition])
  (setv slots (dfor  [k v] (partition 2 items)  (hy.mangle k) v))
  `(setv
    __slots__ [~@(.keys slots)]
    slot-defaults ~slots))


(defmacro has [pos tile-type predicate-form]
  `(next
    (gfor
      it (hy.M.simalq/geometry.at ~pos)
      :if (and (isinstance it ~tile-type) ~predicate-form)
      it)
    None))


(defmacro defn-dd [fname params doc-form #* body]
  #[[Define a function with a "dynamic docstring": another function
  stored in an attribute `dynadoc` of the host function.]]
  `(do ~@(_defn-dd fname params doc-form body)))

(defmacro fn-dd [params doc-form #* body]
  "As `defn-dd`, for an anonymous function."
  (setv fname (hy.gensym))
  `(do
    ~@(_defn-dd fname params doc-form body)
    ~fname))

(defn _defn-dd [fname params doc-form body]
  (assert (and
    (isinstance doc-form hy.models.Expression)
    (= (get doc-form 0) 'doc)))
  [
    `(defn ~fname ~params ~@body)
    `(setv (. ~fname dynadoc) (fn [it] ~@(cut doc-form 1 None)))])


(defmacro pop-integer-part [x]
  "Subtract any integer part from `x` and return it."
  (setv n (hy.gensym))
  `(do
    (setv ~n (.__floor__ ~x))
    (-= ~x ~n)
    ~n))
