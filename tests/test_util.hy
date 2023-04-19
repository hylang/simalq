(require
  simalq.macros [pop-integer-part])
(import
  fractions [Fraction :as f/])


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
