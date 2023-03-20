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
  (setv dataclass (hy.gensym))
  `(do
    (import dataclasses [dataclass :as ~dataclass])
    (defclass [(~dataclass :slots True ~@kwargs)] ~class-name ~superclasses
      ~@docstring
      ~@(gfor
        field fields
        `(annotate ~field ...))
      ~@rest)))


(defmacro has [pos predicate-form]
  `(do
    (import simalq.geometry)
    (next (gfor  it (simalq.geometry.at ~pos)  :if ~predicate-form  it) None)))
