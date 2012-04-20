(ns cljs.macro-test.macros-3)

(defmacro bar
  [x]
  `(if ~x
     2
     1))