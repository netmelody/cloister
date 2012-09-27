(ns cloister.parser
  (:use [cloister.parser.util :only [prettify]])
  (:use [cloister.parser.scope :only [scope-proto]])
  (:use [cloister.parser.traversal :only [advance extract-statements]])
  (:use [cloister.parser.symbols :only [base-symbol-table]]))

(defn parse [tokens]
  (let [world (advance {:scope scope-proto :symbol-table base-symbol-table :tokens tokens})
        [new-world statements] (extract-statements world)]
    (prettify statements)))
