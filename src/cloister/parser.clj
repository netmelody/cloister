(ns cloister.parser
  (:require [cloister.parser.traversal])
  (:require [cloister.parser.symbols])
  (:require [cloister.parser.scope]))

(defn- error [message] (println message))

(defn parse [tokens]
  (let [world {:scope scope-proto
               :symbol-table base-symbol-table
               :tokens tokens}
        token nil
        token-nr 0]
    
    (advance world token token-nr)
    
    (println "parsed")
    {}))
