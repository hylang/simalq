(require
  hyrule [unless defmacro-kwargs])


(defmacro defmeth [#* args]
  #[[Define a method. `self` can be elided from the parameter list,
  `self` itself can be written `@` in the body, and `self.foo`
  can be written `@foo`.

  Furthermore, the method can have a "dynamic docstring": another
  function stored in an attribute `dynadoc` of the host function. If
  provided, this should be an expression headed `doc` as the first
  element of the body.]]

  (if (isinstance (get args 0) hy.models.List)
    (setv [decorators fname params #* body] args)
    (setv decorators []  [fname params #* body] args))
  (_defmeth decorators fname params body))

(defn _defmeth [decorators fname params body]
  (defn f [x]
    (cond
      (isinstance x hy.models.Sequence)
        ((type x) (map f x))
      (and (isinstance x hy.models.Symbol) (.startswith x "@"))
        (if (= x '@)
          'self
          `(. self ~(hy.models.Symbol (cut x 1 None))))
      True
        x))
  (setv body (f (hy.as-model body)))

  (setv dynadoc #(None))
  (when (and
      body
      (isinstance (get body 0) hy.models.Expression)
      (get body 0)
      (= (get body 0 0) 'doc))
    (setv [dynadoc #* body] body)
    (setv dynadoc #(`(setv (. ~fname dynadoc)
      (fn [self] ~@(cut dynadoc 1 None))))))

  `(do
    (defn ~decorators ~fname [self ~@params]
      ~@body)
    ~@dynadoc))


(defmacro-kwargs defdataclass [class-name superclasses #* args #** kwargs]
  #[[Syntactic sugar for common uses of data classes. Code like

    (defdataclass C []
      "A docstring."

      :fields [foo1 foo2]
      :eq True

      (defn method [self]
        (+ self.foo1 self.foo2)))

  becomes

    (defclass [(dataclass :slots True :eq True)] C []
      "A docstring."

      (annotate foo1 ...)
      (annotate foo2 ...)

      (defn method [self]
        (+ self.foo1 self.foo2)))

  `slots` is always set to `True`. `:fields` is optional, and you may
  supply a dictionary `:field-defaults` instead.]]

  (setv   args (list args)  docstring [])
  (when (and args (isinstance (get args 0) hy.models.String))
    (.append docstring (.pop args 0)))
  (setv fields (.pop kwargs "fields" []))
  (setv field-defaults (.pop kwargs "field_defaults" {}))
  (assert (not (and fields field-defaults)))
    ; `field-defaults` defines `fields` implicitly.
  (unless (or fields field-defaults)
    (setv (get kwargs "frozen") 'True))
  `(defclass
    [(hy.I.dataclasses.dataclass :slots True #** ~kwargs)]
    ~class-name ~superclasses
    ~@docstring
    ~@(if field-defaults
      (gfor
        [field default] (hy.I.toolz.partition 2 field-defaults)
        `(setv (annotate ~field ...) ~(if (= default '[])
          '(hy.I.dataclasses.field :default-factory list)
          default)))
      (gfor
        field fields
        `(annotate ~field ...)))
    ~@args))


(defmacro field-defaults [#* items]
  (import toolz [partition])
  (setv fields (dfor
    [k v] (partition 2 items)
    :do (assert (isinstance k hy.models.Symbol))
    (hy.mangle k) v))
  `(setv
    fields [~@(.keys fields)]
    field-defaults ~fields))


(defmacro pop-integer-part [x]
  "Subtract any integer part from `x` and return it."
  (setv n (hy.gensym))
  `(do
    (setv ~n (.__floor__ ~x))
    (-= ~x ~n)
    ~n))
