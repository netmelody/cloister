(ns cloister.parser.util)

(defn report-error [token message] (throw (IllegalStateException. (str message " " token))))

(defn prettify [statements]
  (cond
    (vector? statements) (vec (map prettify statements))
    (map? statements) (into {} (for [[k v] (select-keys statements [:key :name :message :value :arity :first :second :third :fourth])] [k (prettify v)]))
    :else statements))

