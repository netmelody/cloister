(ns cloister.parser.traversal
  (:require [cloister.parser.scope]))

(defn next-token [world]
  (if-let [tokens (:tokens world)]
    (let [token (first tokens)]
      (if-let [base (cond
                      (= :name     (:type token)) (scope-find (:scope world) (:value token))
                      (= :operator (:type token)) (symbol-find (:symbol-table world) (:value token))
                      (= :string   (:type token)) (symbol-find (:symbol-table world) :literal)
                      (= :number   (:type token)) (symbol-find (:symbol-table world) :literal)
                      true nil)]
        (assoc base :from (:from token) :to (:to token) :value (:value token) :arity (:arity token))
        (error "Unexpected token")))
      ((:symbol-table world) :end)))

(defn advance 
  ([world] (advance world nil))
  ([world expected-token-id]
    ;TODO: checks
    (let [token (next-token world)]
      (assoc world :tokens (rest tokens) :token token))))

(defn extract-expression [world right-binding-power]
  (loop [w (advance world) left ((:null-denotation (:token world)))] ;TODO: does invocation need world?
    (if (>= right-binding-power (:token w))
      [world left]
      (recur (advance w) ((:left-denotation (:token w)) left))))) ;TODO: does invocation need world?

(defn extract-statement [world]
  (if-let [std (:statement-denotation (:token world))]
    (let [new-world (assoc (advance world) :scope (scope-reserve (:scope new-world-1) (:token world)))]
      (std new-world)
    (let [[new-world expression] (extract-expression world 0)]
      ;TODO: checks
      [(advance new-world ";") expression]))))

(defn extract-statements [world]
  (loop [w world statements []]
    (if (or (= :end (:id (:token w))) (= "{" (:id (:token w))))
      [w statements]
      (let [[new-world statement] (extract-statement w)]
        (recur [new-world (conj statements statement)])))))

(defn extract-block [world]
  (let [std (:statement-denotation (:token world))]
    (std (advance world "{"))))