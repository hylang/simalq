(require
  hyrule [unless])


(defmacro defdataclass [class-name superclasses #* rest]
  (setv   rest (list rest)  docstring [])
  (when (and rest (isinstance (get rest 0) hy.models.String))
    (.append docstring (.pop rest 0)))
  (setv  kwargs []  fields []  field-defaults [])
  (while (and rest (isinstance (get rest 0) hy.models.Keyword))
    (setv [k v #* rest] rest)
    (cond
      (= k ':fields)
        (setv fields v)
      (= k ':field-defaults)
        (setv field-defaults v)
      True
        (.extend kwargs [k v])))
  (assert (not (and fields field-defaults)))
    ; `field-defaults` defines `fields` implicitly.
  (unless (or fields field-defaults)
    (.extend kwargs '[:frozen True]))
  `(defclass
    [(hy.I.dataclasses.dataclass :slots True ~@kwargs)]
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
    ~@rest))


(defmacro field-defaults [#* items]
  (import toolz [partition])
  (setv fields (dfor
    [k v] (partition 2 items)
    :do (assert (isinstance k hy.models.Symbol))
    (hy.mangle k) v))
  `(setv
    fields [~@(.keys fields)]
    field-defaults ~fields))


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


(defmacro defmacro-kwargs [mname params #* body]
  (setv [ps p-rest p-kwargs] (parse-params params))
  (setv g (hy.gensym))
  `(defmacro ~mname [#* ~g]
    (setv ~g (hy.I.simalq/macros.match-params ~g '~params))
    ~@(gfor
      name [#* (.keys ps) #* (if p-rest [p-rest] []) #* (if p-kwargs [p-kwargs] [])]
      `(setv ~(hy.models.Symbol name) (get ~g ~name)))
    ~@body))

(defn match-params [args params]
  "Match a interable of arguments against a parameter list in the
  style of a `defn` lambda list. The parameter-list syntax here is
  somewhat restricted: annotations are forbiddden, `/` and `*` aren't
  recognized, and nothing is allowed after `#* args` other than `#**
  kwargs`.

  Return a dictionary of parameters and their values."

  (setv [ps p-rest p-kwargs] (parse-params params))

  ; Loop over `args`.
  (setv  args (list args)  collected-rest []  collected-kwargs {}  i-pos 0)
  (while args
    (setv x (.pop args 0))
    (cond

      (and
          (isinstance x hy.models.Expression)
          x
          (isinstance (get x 0) hy.models.Symbol)
          (in (hy.mangle (get x 0)) ["unpack_iterable" "unpack_mapping"]))
        ; Unpacking would require evaluating the elements of `args`, which we
        ; want to avoid.
        (raise (TypeError "unpacking is not allowed in `args`"))

      (isinstance x hy.models.Keyword) (do
        ; A keyword argument
        (setv x (hy.mangle x.name))
        (when (or
            (in x collected-kwargs)
            (and (in x ps) (is-not (get ps x "value") None)))
          (raise (TypeError (+ "keyword argument repeated: " x))))
        (setv v (.pop args 0))
        (cond
          (in x ps)
            (setv (get ps x "value") v)
          p-kwargs
            (setv (get collected-kwargs x) v)
          True
            (raise (TypeError f"unexpected keyword argument '{x}'"))))

      True (do
        ; A positional argument
        (cond
          (< i-pos (len ps)) (do
            (setv [k d] (get (list (.items ps)) i-pos))
            (if (is (get d "value") None)
              (setv (get d "value") x)
              (raise (TypeError f"got multiple values for argument '{k}'"))))
          p-rest
            (.append collected-rest x)
          True
            (raise (TypeError f"takes {(len ps)} positional arguments but more were given")))
        (+= i-pos 1))))

  ; Return the result.
  (dict
    #** (dfor
      [p d] (.items ps)
      p (cond
        (is-not (get d "value") None)
          (get d "value")
        (is-not (get d "default") None)
          (get d "default")
        True
          (raise (TypeError f"missing a required positional argument: '{p}'"))))
    #** (if p-rest {p-rest (tuple collected-rest)} {})
    #** (if p-kwargs {p-kwargs collected-kwargs} {})))

(defn parse-params [params]
  (import
    funcparserlib.parser [maybe many]
    hy.model-patterns [SYM FORM sym brackets pexpr])
  (setv msym (>> SYM hy.mangle))
  (defn pvalue [root wanted]
    (>> (pexpr (+ (sym root) wanted)) (fn [x] (get x 0))))
  (setv [ps p-rest p-kwargs] (.parse
    (+
      (many (| msym (brackets msym FORM)))
      (maybe (pvalue "unpack-iterable" msym))
      (maybe (pvalue "unpack-mapping" msym)))
    params))
  (setv ps (dfor
    p ps
    :setv [k dv] (if (isinstance p hy.models.List) p [p None])
    k (dict :value None :default dv)))
  [ps p-rest p-kwargs])


(defmacro pop-integer-part [x]
  "Subtract any integer part from `x` and return it."
  (setv n (hy.gensym))
  `(do
    (setv ~n (.__floor__ ~x))
    (-= ~x ~n)
    ~n))
