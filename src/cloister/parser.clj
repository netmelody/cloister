(ns cloister.parser
  (:require [cloister.parser.traversal])
  (:require [cloister.parser.symbols])
  (:require [cloister.parser.scope]))

(defn- error [message] (println message))

(defn parse [tokens]
  (let [world (cloister.parser.traversal/advance {:scope cloister.parser.scope/scope-proto
                                                  :symbol-table cloister.parser.symbols/base-symbol-table
                                                  :tokens tokens})
        [new-world statements] (cloister.parser.traversal/extract-statements world)]
    statements))
