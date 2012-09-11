(ns cloister.parser.traversal
  (:require [cloister.parser.scope]))

(defn- error [message] (println message))
(defn symbol-find [world symbol] ((:symbol-table world) symbol))

(defn next-token [world]
  (if-let [tokens (:tokens world)]
    (let [token (first tokens)]
      (if-let [base (cond
                      (= :name     (:type token)) (cloister.parser.scope/scope-find (:scope world) (:value token))
                      (= :operator (:type token)) (symbol-find world (:value token))
                      (= :string   (:type token)) (symbol-find world :literal)
                      (= :number   (:type token)) (symbol-find world :literal)
                      true nil)]
        (assoc base :from (:from token) :to (:to token) :value (:value token) :arity (:arity token))
        (error "Unexpected token")))
      (symbol-find world :end)))

(defn advance 
  ([world] (advance world nil))
  ([world expected-token-id]
    ;TODO: checks
    (let [token (next-token world)]
      (assoc world :tokens (rest (:tokens world)) :token token))))

(defn extract-expression [world right-binding-power]
  (loop [[w left] ((:null-denotation (:token world)) (advance world))]
    (if (>= right-binding-power (:left-binding-power (:token w)))
      [w left]
      (recur ((:left-denotation (:token w)) (advance w) left)))))

(defn extract-statement [world]
  (if-let [std (:statement-denotation (:token world))]
    (let [new-world (assoc (advance world) :scope (cloister.parser.scope/scope-reserve (:scope world) (:token world)))]
      (std new-world))
    (let [[new-world expression] (extract-expression world 0)]
      (if (and (not (:assignment expression)) (not (= "(" (:id expression))))
        (error "Bad expression statement")) 
      [(advance new-world ";") expression])))

(defn extract-statements [world]
  (loop [w world statements []]
    (if (or (= :end (:id (:token w))) (= "{" (:id (:token w))))
      [w statements]
      (let [[new-world statement] (extract-statement w)]
        (recur new-world (conj statements statement))))))

(defn extract-block [world]
  (let [std (:statement-denotation (:token world))]
    (std (advance world "{"))))