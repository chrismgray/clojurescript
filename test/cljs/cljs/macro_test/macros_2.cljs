(ns cljs.macro-test.macros-2)

(defmacro bar [x]
  `(if ~x
     1
     2))