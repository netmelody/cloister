(ns cloister.parser.scope
  (:use [cloister.parser.util :only [report-error]]))

(def definition-proto
  {:reserved false
   :null-denotation (fn [world token] [world token]) 
   :left-denotation nil
   :statement-denotation nil
   :left-binding-power 0
   :scope nil
   :arity nil})

(def scope-proto
  {:parent nil
   :definitions {}})

(defn scope-define [scope token]
  (let [name (:value token)
        defs (:definitions scope)]
    (if (defs name)
      (report-error (defs name) (str name " already defined"))
      (let [definition (assoc (merge token definition-proto) :scope scope)]
        (assoc scope :definitions (assoc defs name definition))))))

(defn scope-find [{scope :scope symbol-table :symbol-table} name]
  (loop [s scope]
    (if s
      (if-let [definition ((:definitions s) name)]
        definition
        (recur (:parent s)))
      (if-let [symbol (symbol-table name)]
        symbol
        (:name symbol-table)))))

(defn scope-reserve [scope definition]
  (let [name (:value definition)
        defs (:definitions scope)]
    (assoc scope :definitions (assoc defs name (assoc definition :reserved true)))))

(defn scope-create-child [scope]
  (assoc scope-proto :parent scope))