(ns cljs.macros
  (:refer-clojure :exclude [macroexpand-1 munge])
  (:require [cljs.repl :as repl]
            [cljs.repl.rhino :as rhino]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:use [cljs.compiler :as compiler :exclude [macroexpand-1]]))

(def repl-env (rhino/repl-env))
(binding [compiler/*cljs-ns* 'cljs.core]
  (repl/-setup repl-env))

(defn- eval-analyzed
  [analyzed-form]
  (let [js (compiler/emits analyzed-form)
        res (binding [*out* (java.io.StringWriter.)] (repl/-evaluate repl-env nil nil js))]
    (when repl/*cljs-verbose*
      (binding [*out* *err*]
       (println js)))
    (when (:stacktrace res)
      (binding [*out* *err*]
        (println (:value res))
        (println (:stacktrace res))))
    (:value res)))

(defmethod compiler/parse 'defmacro
  [op env [_ mac-name & mac-body :as form] name]
  (let [mac-fun-name (symbol (gensym mac-name))]
    (swap! namespaces assoc-in [(-> env :ns :name) :defs (symbol mac-name) :macro] mac-fun-name)
    (eval-analyzed (compiler/analyze env (concat (list 'defn mac-fun-name) mac-body)))
    {:env env :op :constant :form ()}))

(defn- resolve-macro
  "Return the function implementing the macro in the current
   environment if it exists, `nil` otherwise."
  [sym env]
  (when-not (-> env :locals sym)
    (if-let [nstr (namespace sym)]
      (let [full-ns (resolve-ns-alias env (symbol nstr))
            s (-> sym name symbol)
            macro-fun (get-in @namespaces [full-ns :defs s :macro])]
        (get-in @namespaces [full-ns :defs macro-fun :name]))
      (get-in @namespaces [(-> env :ns :name) :defs sym :macro]))))

(defn macroexpand-1 [env form]
  (let [op (first form)]
    (if (specials op)
      form
      (if-let [mac (and (symbol? op) (resolve-macro op env))]
        (eval-analyzed (analyze (assoc env :context :statement) (list 'str (concat (list mac) (map #(list 'quote %) (rest form)))) nil))
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
  [env form & [name top-level-form?]]
  (let [env (assoc env :line
                   (or (-> form meta :line)
                       (:line env)))]
    (let [op (first form)]
      (assert (not (nil? op)) "Can't call nil")
      (let [mform (macroexpand-1 env form)]
        (cond
         (string? mform)
         (analyze env (read-string mform) name top-level-form?)
         (identical? form mform)
         (if (specials op)
           (let [res (parse op env form name)]
             (when top-level-form?
               (eval-analyzed res))
             res)
            (parse-invoke env form))
          :else
          (analyze env mform name top-level-form?))))))


