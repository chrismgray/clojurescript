(ns cljs.macro-test.macros-4)

(defmacro bar
  [x]
  `(if ~x
     3
     4))