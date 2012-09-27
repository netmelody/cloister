(ns cloister.parser.traversal
  (:use [cloister.parser.util])
  (:require [cloister.parser.scope]))

(defn symbol-find [world symbol] ((:symbol-table world) symbol))

(defn next-token [world]
  (if-let [token (first (:tokens world))]
    (if-let [base (cond
                    (= :name     (:type token)) (assoc (cloister.parser.scope/scope-find world (:value token)) :arity (:type token))
                    (= :operator (:type token)) (assoc (symbol-find world (:value token)) :arity (:type token))
                    (= :string   (:type token)) (assoc (symbol-find world :literal) :arity :literal)
                    (= :number   (:type token)) (assoc (symbol-find world :literal) :arity :literal)
                    true nil)]
      (assoc base :from (:from token) :to (:to token) :value (:value token))
      (report-error token "Unexpected token"))
    (symbol-find world :end)))

(defn advance 
  ([world] (advance world nil))
  ([world expected-token-id]
    (if (and expected-token-id (not (= (:id (:token world)) expected-token-id)))
      (report-error (:token world) (str "Expected \"" expected-token-id "\"")) 
    ;TODO: checks
    (let [token (next-token world)]
      (assoc world :tokens (rest (:tokens world)) :token token)))))

(defn extract-expression [world right-binding-power]
  (loop [[w left] ((:null-denotation (:token world)) (advance world) (:token world))]
    (if (>= right-binding-power (:left-binding-power (:token w)))
      [w left]
      (recur ((:left-denotation (:token w)) (advance w) (:token w) left)))))

(defn extract-statement [world]
  (if-let [std (:statement-denotation (:token world))]
    (let [new-world (assoc (advance world) :scope (cloister.parser.scope/scope-reserve (:scope world) (:token world)))]
      (std new-world (:token world)))
    (let [[new-world expression] (extract-expression world 0)]
      (if (and (not (:assignment expression)) (not (= "(" (:id expression))))
        (report-error expression "Bad expression statement"))
      [(advance new-world ";") expression])))

(defn extract-statements [world]
  (loop [w world statements []]
    (if (or (= :end (:id (:token w))) (= "{" (:id (:token w))))
      [w statements]
      (let [[new-world statement] (extract-statement w)]
        (recur new-world (conj statements statement))))))

(defn extract-block [world]
  (let [std (:statement-denotation (:token world))]
    (std (advance world (:token world) "{"))))