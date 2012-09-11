(ns cloister.parser.scope)

(defn- error [message] (println message))
(defn- itself [map property] (assoc map property (fn [&] (itself map property))))

(def definition-proto
  (itself {:reserved false
           :left-denotation nil
           :statement-denotation nil
           :left-binding-power 0
           :scope nil
           :arity nil} :null-denotation))

(def scope-proto
  {:parent nil
   :definitions {}})

(defn scope-define [scope token]
  (let [name (:value token)
        defs (:definitions scope)]
    (if (defs name)
      (error (str name " already defined"))
      (let [definition (assoc (merge token definition-proto) :scope scope)]
        (assoc scope :definitions (assoc defs name definition))))))

(defn scope-find [scope name]
  (loop [s scope]
    (if s
      (if-let [definition ((:definitions s) name)]
        definition
        (recur (:parent s)))
      nil)))

(defn scope-reserve [scope definition]
  (let [name (:value definition)
        defs (:definitions scope)]
    (assoc scope :definitions (assoc defs name (assoc definition :reserved true)))))

(defn scope-create-child [scope]
  (assoc scope-proto :parent scope))