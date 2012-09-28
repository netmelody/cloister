(ns cloister.test.parser
  (:use [cloister.parser])
  (:use [clojure.test]))

(deftest parses-empty-vector-of-tokens
  (is (= [] (cloister.parser/parse []))))

; var i = 1;
(deftest parses-simple-assignment
  (is (= [{:value "="
           :arity :binary
           :first  {:value "i" :arity :name}
           :second {:value  1  :arity :literal}}]
         (cloister.parser/parse [{:from 0 :to 3 :type :name     :value "var"}
                                 {:from 4 :to 5 :type :name     :value "i"}
                                 {:from 5 :to 6 :type :operator :value "="}
                                 {:from 6 :to 7 :type :number   :value 1}
                                 {:from 7 :to 8 :type :operator :value ";"}]))))

; var o = true ? 1.0 : 2.0;
(deftest parses-ternery-operator
  (is (= [{:value "="
           :arity :binary
           :first  {:value "o" :arity :name}
           :second {:value "?" :arity :ternary :first  {:value true :arity :literal}
                                               :second {:value 1.0  :arity :literal}
                                               :third  {:value 2.0  :arity :literal}}}]
         (cloister.parser/parse [{:to 3  :from 0  :type :name     :value "var"}
                                 {:to 5  :from 4  :type :name     :value "o"}
                                 {:to 7  :from 6  :type :operator :value "="}
                                 {:to 12 :from 8  :type :name     :value "true"}
                                 {:to 14 :from 13 :type :operator :value "?"}
                                 {:to 16 :from 15 :type :number   :value 1.0}
                                 {:to 18 :from 17 :type :operator :value ":"}
                                 {:to 20 :from 19 :type :number   :value 2.0}
                                 {:to 21 :from 20 :type :operator :value ";"}]))))

; var i = function() { return 1; };
(deftest parses-simple-function-declaration
  (is (= [{:value "="
           :arity :binary
           :first  {:value "i" :arity :name}
           :second {:value "function"
                    :arity :function
                    :first []
                    :second {:value "return" :arity :statement :first {:value 1.0 :arity :literal}}}}]
         (cloister.parser/parse [{:from 0  :to 3  :type :name     :value "var"}
                                 {:from 4  :to 5  :type :name     :value "i"}
                                 {:from 6  :to 7  :type :operator :value "="}
                                 {:from 8  :to 16 :type :name     :value "function"}
                                 {:from 16 :to 17 :type :operator :value "("}
                                 {:from 17 :to 18 :type :operator :value ")"}
                                 {:from 19 :to 20 :type :operator :value "{"}
                                 {:from 21 :to 27 :type :name     :value "return"}
                                 {:from 28 :to 29 :type :number   :value 1}
                                 {:from 29 :to 30 :type :operator :value ";"}
                                 {:from 31 :to 32 :type :operator :value "}"}
                                 {:from 32 :to 33 :type :operator :value ";"}]))))
