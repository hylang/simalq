(defmacro defdataclass [class-name superclasses [fields #()] #* kwargs]
  (when (not fields)
    (+= kwargs '#(:frozen True)))
  (setv dataclass (hy.gensym))
  `(do
    (import dataclasses [dataclass :as ~dataclass])
    (defclass [(~dataclass :slots True ~@kwargs)] ~class-name ~superclasses
      ~@(gfor
        field fields
        `(annotate ~field ...)))))
