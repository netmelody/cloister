(ns cloister.test.tokeniser
  (:use [cloister.tokeniser])
  (:use [clojure.test]))

(deftest tokenises-nil-to-empty-vector
  (is (= [] (cloister.tokeniser/tokenise nil))))

(deftest tokenises-empty-string-to-empty-vector
  (is (= [] (cloister.tokeniser/tokenise ""))))

(deftest tokenises-blank-string-to-empty-vector
  (is (= [] (cloister.tokeniser/tokenise "     "))))

(deftest tokenises-word-to-name
  (let [tokens (cloister.tokeniser/tokenise " foo ")]
    (is (= 1 (count tokens)))
    (is (= :name (:type (first tokens))))
    (is (= "foo" (:value (first tokens))))))

(deftest tokenises-positive-whole-number-to-number
  (let [tokens (cloister.tokeniser/tokenise " 123 ")]
    (is (= 1 (count tokens)))
    (is (= :number (:type (first tokens))))
    (is (= 123.0 (:value (first tokens))))))

