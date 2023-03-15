(defmacro defdataclass [class-name superclasses [fields #()] #* kwargs]
  (import toolz [partition])
  (setv dataclass (hy.gensym))
  (when (not fields)
    (+= kwargs '#(:frozen True)))
  `(do
    (import dataclasses [dataclass :as ~dataclass])
    (defclass [(~dataclass :slots True ~@kwargs)] ~class-name ~superclasses
      ~@(gfor
        field fields
        `(annotate ~field ...)))))
