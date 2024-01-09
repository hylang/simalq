(require
  simalq.macros [pop-integer-part defmacro-kwargs])
(import
  fractions [Fraction :as f/]
  simalq.macros [match-params]
  simalq.util [mixed-number]
  pytest)



(defn test-match-params []

  (defn f [args]
    (match-params args '[a b [c "default-c"] #* rest #** kw]))
  (assert (=
    (f [1 2])
    (dict :a 1 :b 2 :c '"default-c" :rest #() :kw {})))
  (assert (=
    (f '[1 2])
    (dict :a '1 :b '2 :c '"default-c" :rest #() :kw {})))
  (assert (=
    (f '[1 2 3 4 (+ 4 1)])
    (dict :a '1 :b '2 :c '3 :rest #('4 '(+ 4 1)) :kw {})))
  (assert (=
    (f '[:a 1 :b 2 :c 3 :extra 4])
    (dict :a '1 :b '2 :c '3 :rest #() :kw {"extra" '4})))
  (assert (=
    (f '[:b 2 1])
    (dict :a '1 :b '2 :c '"default-c" :rest #() :kw {})))
  (assert (=
    (f '[:b 2 :extra "foo" :a 1])
    (dict :a '1 :b '2 :c '"default-c" :rest #() :kw {"extra" '"foo"})))
  (assert (=
    (f '[1 2 3 4 5 6 7 :x 10 :y 11])
    (dict :a '1 :b '2 :c '3 :rest #('4 '5 '6 '7) :kw {"x" '10 "y" '11})))

  ; Mangling
  (assert (=
    (match-params
      '[1 :⬢ ☤ :⚘ 3 :☘ 4]
      '[a-b ⬢ #** ✈])
    (dict
      :a_b '1
      :hyx_Xblack_hexagonX '☤
      :hyx_XairplaneX {"hyx_XflowerX" '3 "hyx_XshamrockX" '4})))

  ; Unpacking
  (with [(pytest.raises TypeError :match "^unpacking is not allowed in `args`$")]
    (f '[1 2 3 #* [1 2]]))
  (with [(pytest.raises TypeError :match "^unpacking is not allowed in `args`$")]
    (f '[1 2 3 #** {"qq" 1 "xx" 2}])))


(defn test-defmacro-kwargs []
   (defmacro-kwargs m [a b [c "default-c"] #* rest #** kw]
     [a b c rest kw])
   (assert (=
     (m 1 2)
     [1 2 "default-c" #() {}]))
   (assert (=
     (m :b "bb" :a "aa" :foo "hello")
     ["aa" "bb" "default-c" #() {"foo" "hello"}])))


(defn test-pop-integer-part []
  (setv x (f/ 1 3))
  (assert (= (pop-integer-part x) 0))
  (assert (= x (f/ 1 3)))

  (setv x (f/ 14 3))
  (assert (= (pop-integer-part x) 4))
  (assert (= x (f/ 2 3)))

  (setv x (f/ 0))
  (assert (= (pop-integer-part x) 0))
  (assert (= x (f/ 0)))

  ; I'm leaving the negative case unspecified now because I'm not
  ; sure how it should work.
  )


(defn test-mixed-number []
  (assert (=
    (lfor  n (range 7) (mixed-number (f/ n 3)))
    ["0" "1/3" "2/3" "1" "1 1/3" "1 2/3" "2"])))
