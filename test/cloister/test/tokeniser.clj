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
  (let [tokens (cloister.tokeniser/tokenise " 'f\\toobar' ")]
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

(deftest tokenises-composite-operators
  (let [tokens (cloister.tokeniser/tokenise " <= ")]
    (is (= 1 (count tokens)))
    (is (= :operator (:type (first tokens))))
    (is (= "<=" (:value (first tokens))))))

(deftest tokenises-single-operators
  (let [tokens (cloister.tokeniser/tokenise " ( ")]
    (is (= 1 (count tokens)))
    (is (= :operator (:type (first tokens))))
    (is (= "(" (:value (first tokens))))))

;(deftest tokenises-demonstrative-syntax
;  (let [tokens (cloister.tokeniser/tokenise "(function() { cnt=-12.25e-14; alert(\"f\\too\" <= 'ba\\nr'); }()) //go")]
;    (is (= 21 (count tokens)))
;    (let [expected [{:type :operator :value "(" :from 0 :to 1}
;                    {:type :name :value "function" :from 1 :to 9}
;                    {:type :operator :value "(" :from 9 :to 10}
;                    {:type :operator :value ")" :from 10 :to 11}
;                    {:type :operator :value "{" :from 12 :to 13}
;                    {:type :name :value "cnt" :from 14 :to 17}
;                    {:type :operator :value "=" :from 17 :to 18}
;                    {:type :operator :value "-" :from 18 :to 19}
;                    {:type :number :value 1.225E-13 :from 19 :to 28}
;                    {:type :operator :value ";" :from 28 :to 29}
;                    {:type :name :value "alert" :from 30 :to 35}
;                    {:type :operator :value "(" :from 35 :to 36}
;                    {:type :string :value "f\too" :from 36 :to 43}
;                    {:type :operator :value "<=" :from 44 :to 46}
;                    {:type :string :value "ba\nr" :from 47 :to 54}
;                    {:type :operator :value ")" :from 54 :to 55}
;                    {:type :operator :value ";" :from 55 :to 56}
;                    {:type :operator :value "}" :from 57 :to 58}
;                    {:type :operator :value "(" :from 58 :to 59}
;                    {:type :operator :value ")" :from 59 :to 60}
;                    {:type :operator :value ")" :from 60 :to 61}]]
;      (is (= expected tokens)))))

