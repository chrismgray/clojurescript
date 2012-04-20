(ns cljs.defmacro-test
  (:require [cljs.macro-test.macros-2 :as mac2]
            [cljs.macro-test.macros-3 :as mac3]))

(defmacro foo [a b]
  `(+ ~a ~b))

;; In Clojure, the &env and &form arguments must be passed in first to
;; the recur call.  They are not passed implicitly in CLJS (yet).  I
;; wonder if they should be.
(defmacro rec-macro-1
  "The same as above, but with recur instead of a recursive call."
  [a]
  (if (= 0 a)
    a
    (recur (dec a))))

(defmacro rec-macro-2
  "The same again, but putting the recursive call in the output."
  [a]
  (if (= 0 a)
    a
    `(cljs.defmacro-test/rec-macro-2 ~(dec a))))

(defn bar-test-1
  [y]
  (mac2/bar y))

(defn bar-test-2
  [y]
  (mac3/bar y))

(defmacro retain-meta-inf
  [x]
  (meta x))

(defn test-macros []
  (assert (= (foo 2 3) 5))
  (assert (zero? (rec-macro-1 5)))
  (assert (zero? (rec-macro-2 4)))
  (assert (= 1 (bar-test-1 true)))
  (assert (= 2 (bar-test-1 false)))
  (assert (= 2 (bar-test-2 true)))
  (assert (= 1 (bar-test-2 false)))
  (assert (= (:line (retain-meta-inf (+ 1 1))) (:line (meta '(+ 1 1)))))
  
  )