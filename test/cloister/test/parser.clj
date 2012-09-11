(ns cloister.test.parser
  (:use [cloister.parser])
  (:use [clojure.test]))

(deftest parses-empty-vector-of-tokens
  (is (= {} (cloister.parser/parse []))))

(deftest parses-simple-assignment
  (is (= {:value "="
          :arity "binary"
          :first  {:value "i" :arity "name"}
          :second {:value  1  :arity "literal"}}
         (cloister.parser/parse [{:from 0, :to 3, :type :name, :value "var"}
                                 {:from 4, :to 5, :type :name, :value "i"}
                                 {:from 5, :to 6, :type :operator, :value "="}
                                 {:from 6, :to 7, :type :number, :value 1}
                                 {:from 7, :to 8, :type :operator, :value ";"}]))))