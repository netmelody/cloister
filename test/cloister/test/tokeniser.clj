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

(deftest tokenises-two-words-to-names
  (let [tokens (cloister.tokeniser/tokenise " foo   bar ")]
    (is (= 2 (count tokens)))
    (is (= :name (:type (first tokens))))
    (is (= "foo" (:value (first tokens))))
    (is (= :name (:type (first (rest tokens)))))
    (is (= "bar" (:value (first (rest tokens)))))))

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

(deftest tokenises-string-with-escaped-chars
  (let [tokens (cloister.tokeniser/tokenise " 'f\toobar' ")]
    (is (= 1 (count tokens)))
    (is (= :string (:type (first tokens))))
    (is (= "f\toobar" (:value (first tokens))))))

(deftest ignores-comments
  (let [tokens (cloister.tokeniser/tokenise (str " woo //wonderful \n foo "))]
    (is (= 2 (count tokens)))
    (is (= :name (:type (first tokens))))
    (is (= "woo" (:value (first tokens))))
    (is (= :name (:type (first (rest tokens)))))
    (is (= "foo" (:value (first (rest tokens)))))))

