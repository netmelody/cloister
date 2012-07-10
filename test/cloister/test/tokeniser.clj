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

(deftest tokenises-positive-fractional-number-to-number
  (let [tokens (cloister.tokeniser/tokenise " 123.125 ")]
    (is (= 1 (count tokens)))
    (is (= :number (:type (first tokens))))
    (is (= 123.125 (:value (first tokens))))))

(deftest tokenises-positive-fractional-number-with-exponent-to-number
  (let [tokens (cloister.tokeniser/tokenise " 123.125e15 ")]
    (is (= 1 (count tokens)))
    (is (= :number (:type (first tokens))))
    (is (= 123.125e15 (:value (first tokens))))))

(deftest tokenises-positive-fractional-number-with-negative-exponent-to-number
  (let [tokens (cloister.tokeniser/tokenise " 123.125E-15 ")]
    (is (= 1 (count tokens)))
    (is (= :number (:type (first tokens))))
    (is (= 123.125e-15 (:value (first tokens))))))

(deftest tokenises-single-quoted-string
  (let [tokens (cloister.tokeniser/tokenise " 'foobar' ")]
    (is (= 1 (count tokens)))
    (is (= :string (:type (first tokens))))
    (is (= "foobar" (:value (first tokens))))))