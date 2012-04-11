(ns cljs.macros
  (:refer-clojure :exclude [macroexpand-1 munge])
  (:require [cljs.repl :as repl]
            [cljs.repl.rhino :as rhino]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:use [cljs.compiler :as compiler :exclude [macroexpand-1]]))

(def repl-env (rhino/repl-env))
(binding [compiler/*cljs-ns* 'cljs.user]
 (repl/-setup repl-env))

(defn- eval-analyzed
  [analyzed-form]
  (let [js (compiler/emits analyzed-form)]
    (when repl/*cljs-verbose*
      (println js))
    (:value (repl/-evaluate repl-env nil nil js))))

;; Get the compiler's def method because we are about to override it.
(def compiler-parse-def (get-method compiler/parse 'def))
;; Override the compiler's def method so that it evaluates the new definition.
(defmethod compiler/parse 'def
  [op env form name]
  (let [res (compiler-parse-def op env form name)
        eval-res (eval-analyzed res)]
    res))

(def macros (atom {}))

(defmethod compiler/parse 'defmacro
  [op env [_ mac-name & mac-body :as form] name]
  (let [mac-fun-name (symbol (gensym mac-name))]
    ;; Will recursive macros work? Probably not yet.
    ;; TODO: Properly namespace macros
    (swap! macros assoc (symbol mac-name) mac-fun-name)
    (eval-analyzed (compiler/analyze env (concat (list 'defn mac-fun-name) mac-body)))
    {:env env :op :constant :form ()}))

(defn macroexpand-1 [env form]
  (let [op (first form)]
    (if (specials op)
      form
      (if-let [mac (and (symbol? op) (get @macros op))]
        (eval-analyzed (analyze env (list 'str (concat (list mac) (map #(list 'quote %) (rest form)))) nil))
       (if-let [mac (and (symbol? op) (get-expander op env))]
         (apply mac form env (rest form))
         (if (symbol? op)
           (let [opname (str op)]
             (cond
              (= (first opname) \.) (let [[target & args] (next form)]
                                      (list* '. target (symbol (subs opname 1)) args))
              (= (last opname) \.) (list* 'new (symbol (subs opname 0 (dec (count opname)))) (next form))
              :else form))
           form))))))

(defmethod compiler/analyze :seq
  [env form & [name]]
  (let [env (assoc env :line
                   (or (-> form meta :line)
                       (:line env)))]
    (let [op (first form)]
      (assert (not (nil? op)) "Can't call nil")
      (let [mform (macroexpand-1 env form)]
        (cond
         (string? mform)
         (analyze env (read-string mform) name)
         (identical? form mform)
          (if (specials op)
            (parse op env form name)
            (parse-invoke env form))
          :else
          (analyze env mform name))))))

(comment
  (binding [repl/*cljs-verbose* true
          compiler/*cljs-ns* 'cljs.user]
 (let [repl-env (rhino/repl-env)
       file "<cljs repl>"]
   (repl/-setup repl-env)
   (repl/evaluate-form repl-env envy file '(ns cljs.user))
   (repl/evaluate-form repl-env envy file '(defn foo [x] (inc x)))
   (repl/evaluate-form repl-env envx file '(foo 2))))

(def envx {:ns (@namespaces 'cljs.user) :context :expr :locals '{ethel {:name ethel__123 :init nil}}})
(def envy {:ns (@namespaces 'cljs.user) :context :statement :locals '{ethel {:name ethel__123 :init nil}}})
(binding [repl/*cljs-verbose* true]
  (eval-analyzed (compiler/analyze envy '(defmacro reverse-forms [x y] `(~y ~x)))))
(binding [repl/*cljs-verbose* true]
  (eval-analyzed (compiler/analyze envy '(defmacro unless [pred body] `(if (not ~pred) ~body nil)))))
(eval-analyzed (analyze envy '(defmacro when [pred body] 1)))
(eval-analyzed (compiler/analyze envy '(defn foo [x] (+ x 1))))
(eval-analyzed (compiler/analyze envy '(foo 3)))
(repl/evaluate-form repl-env envx nil '(+ 1 1))
(repl/evaluate-form repl-env envx nil '(defn foo [x] (+ x 1)))
(repl/evaluate-form repl-env envx nil '(foo 2))
(repl/evaluate-form repl-env envx nil '(foobar 2))
(repl/evaluate-form repl-env envx nil 'nil)
(binding [repl/*cljs-verbose* true]
  (eval-analyzed (analyze envx '(reverse-forms 1 +))))
(binding [repl/*cljs-verbose* true]
  (eval-analyzed (analyze envx '(unless true (/ 1 0)))))
(binding [repl/*cljs-verbose* true]
  (repl/evaluate-form repl-env envx nil '(reverse-forms 1 +)))
(eval-analyzed (analyze envx '(when true 2)))
(rhino/rhino-eval repl-env nil nil "cljs.core.str(cljs.core.list.call(null,1,2))")
)
