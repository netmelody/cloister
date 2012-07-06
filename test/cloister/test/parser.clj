(ns cloister.test.parser
  (:use [cloister.parser])
  (:use [clojure.test]))

(deftest parses-empty-vector-of-tokens
  (is (= {} (cloister.parser/parse []))))
