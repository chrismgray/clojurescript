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
        res (repl/-evaluate repl-env nil nil js)]
    (when repl/*cljs-verbose*
      (binding [*out* *err*]
       (println js)))
    (when (:stacktrace res)
      (binding [*out* *err*]
        (println (:value res))
        (println (:stacktrace res))))
    (:value res)))

;; Get the compiler's def method because we are about to override it.
;; (defonce compiler-parse-def (get-method compiler/parse 'def))
;; ;; Override the compiler's def method so that it evaluates the new definition.
;; (defmethod compiler/parse 'def
;;   [op env form name]
;;   (let [res (compiler-parse-def op env form name)
;;         eval-res (eval-analyzed res)]
;;     res))

;; ;; Do the same thing for namespaces
;; (defonce compiler-parse-ns (get-method compiler/parse 'ns))
;; (defmethod compiler/parse 'ns
;;   [op env form name]
;;   (let [res (compiler-parse-ns op env form name)
;;         eval-res (eval-analyzed res)]
;;     res))

;; ;; Do the same thing for deftype
;; (defonce compiler-parse-deftype* (get-method compiler/parse 'deftype*))
;; (defmethod compiler/parse 'deftype*
;;   [op env form name]
;;   (let [res (compiler-parse-deftype* op env form name)
;;         eval-res (eval-analyzed res)]
;;     res))

;; ;; Do the same thing for defrecord
;; (defonce compiler-parse-defrecord* (get-method compiler/parse 'defrecord*))
;; (defmethod compiler/parse 'defrecord*
;;   [op env form name]
;;   (let [res (compiler-parse-defrecord* op env form name)
;;         eval-res (eval-analyzed res)]
;;     res))

;; ;; Do the same thing for js
;; (defonce compiler-parse-js* (get-method compiler/parse 'js*))
;; (defmethod compiler/parse 'js*
;;   [op env form name]
;;   (let [res (compiler-parse-js* op env form name)
;;         eval-res (eval-analyzed res)]
;;     res))

;; ;; Do the same thing for set!
;; (defonce compiler-parse-set! (get-method compiler/parse 'set!))
;; (defmethod compiler/parse 'set!
;;   [op env form name]
;;   (let [res (compiler-parse-set! op env form name)
;;         eval-res (eval-analyzed res)]
;;     res))

;; TODO: put the above in a macro :)

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

(comment
  (binding [repl/*cljs-verbose* true
          compiler/*cljs-ns* 'cljs.user]
 (let [repl-env (rhino/repl-env)
       file "<cljs repl>"]
   (repl/-setup repl-env)
   (repl/evaluate-form repl-env envy file '(ns cljs.user))
   (repl/evaluate-form repl-env envy file '(defn foo [x] (inc x)))
   (repl/evaluate-form repl-env envx file '(foo 2))))

  (def envx {:ns (@namespaces 'cljs.user) :context :expr :locals {}})
  (analyze envx '(defn foo [] (+ 1 1)))
  (swap! namespaces assoc 'cljs.foo {})
  (def envy {:ns (assoc (@namespaces 'cljs.foo) :requires {'user 'cljs.user}) :context :statement :locals '{ethel {:name ethel__123 :init nil}}})
  (resolve-ns-alias envy 'user)
  (resolve-macro 'user/unless envy)
  (resolve-ns-alias envy (symbol (namespace 'user/unless)))
(binding [repl/*cljs-verbose* true]
  (eval-analyzed (compiler/analyze envy '(defmacro reverse-forms [x y] `(~y ~x)))))
@namespaces
(binding [repl/*cljs-verbose* true]
  (eval-analyzed (compiler/analyze envx '(defmacro unless [pred & body] `(if (not ~pred) (do ~@body) nil)))))
(eval-analyzed (analyze envy '(defmacro when [pred body] 1)))
(eval-analyzed (compiler/analyze envy '(defn foo [x] (+ x 1))))
(eval-analyzed (compiler/analyze envy '(foo 3)))
(repl/evaluate-form repl-env envx nil '(+ 1 1))
(repl/evaluate-form repl-env envx nil '(defn foo [x] (+ x 1)))
(repl/evaluate-form repl-env envx nil '(foo 2))
(repl/evaluate-form repl-env envx nil '(foobar 2))
(repl/evaluate-form repl-env envx nil 'nil)
(binding [repl/*cljs-verbose* true]
  (eval-analyzed (analyze envx '(defmacro bar [x] (if (= 0 x) x `(cljs.user/bar ~(dec x)))))))
(binding [repl/*cljs-verbose* true]
  (eval-analyzed (analyze envx '(bar 3))))
(binding [repl/*cljs-verbose* true]
  (eval-analyzed (analyze envx '(reverse-forms 1 +))))
(binding [repl/*cljs-verbose* true]
  (eval-analyzed (analyze envy '(user/unless false (+ 1 1)))))
(binding [repl/*cljs-verbose* true]
  (repl/evaluate-form repl-env envx nil '(reverse-forms 1 +)))
(eval-analyzed (analyze envx '(when true 2)))
(rhino/rhino-eval repl-env nil nil "cljs.core.str(cljs.core.list.call(null,1,2))")
)
