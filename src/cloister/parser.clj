(ns cloister.parser)

(defn- error [message] (println message))

(def symbol-proto
  {:id nil
   :value nil
   :null-denotation (fn [scope] (error "Undefined."))
   :left-denotation (fn [left] (error "Missing operator."))
   :statement-denotation (fn [] (error "Undefined."))
   :left-binding-power 0
   :right-binding-power 0})

(def definition-proto
  {:reserved false
   :null-denotation (fn [] (error "return this?"))
   :left-denotation nil
   :statement-denotation nil
   :left-binding-power 0
   :scope nil
   :arity nil})

(def scope-proto
  {:parent nil
   :definitions {}})

(defn make-symbol
  ([id] (make-symbol id 0))
  ([id binding-power] (assoc symbol-proto :id id :value id :left-binding-power binding-power)))

(defn make-constant [name value]
  (assoc (make-symbol name) :value value))

(defn register-symbol [table symbol]
  (let [id (:id symbol)]
    (if-let [existing-symbol (table id)]
      (assoc table id (assoc existing-symbol :left-binding-power (max (:left-binding-power symbol) (:left-binding-power existing-symbol))))
      (assoc table id symbol))))

(def base-symbol-table (-> {}
                         (register-symbol (make-symbol :end))
                         (register-symbol (make-symbol :name))
                         (register-symbol (make-symbol :literal))
                         (register-symbol (make-symbol ":"))
                         (register-symbol (make-symbol ";"))
                         (register-symbol (make-symbol ")"))
                         (register-symbol (make-symbol "]"))
                         (register-symbol (make-symbol "}"))
                         (register-symbol (make-symbol ","))
                         (register-symbol (make-symbol "else"))
                         (register-symbol (make-constant "true" true))
                         (register-symbol (make-constant "false" false))
                         (register-symbol (make-constant "null" nil))
                         (register-symbol (make-constant "pi" (double 3.141592653589793)))
                         (register-symbol (make-constant "Object" {}))
                         (register-symbol (make-constant "Array" []))))

(defn scope-define [scope token]
  (let [name (:value token)
        defs (:definitions scope)]
    (if (defs name)
      (error (str name " already defined"))
      (let [definition (assoc (merge token definition-proto) :scope scope)]
        (assoc scope :definitions (assoc defs name definition))))))

(defn scope-find [scope name]
  (loop [s scope]
    (if s
      (if-let [definition ((:definitions s) name)]
        definition
        (recur (:parent s)))
      nil)))

(defn scope-reserve [scope definition]
  (let [name (:value definition)
        defs (:definitions scope)]
    (assoc scope :definitions (assoc defs name (assoc definition :reserved true)))))

(defn scope-create-child [scope]
  (assoc scope-proto :parent scope))

(defn symbol-find [symbol-table symbol-id]
  (symbol-table symbol-id))

(defn next-token [tokens]
  (if tokens
    (let [token (first tokens)]
      (if-let [base (cond
                      (= :name     (:type token)) (scope-find (:scope world) (:value token))
                      (= :operator (:type token)) (symbol-find (:symbol-table world) (:value token))
                      (= :string   (:type token)) (symbol-find (:symbol-table world) :literal)
                      (= :number   (:type token)) (symbol-find (:symbol-table world) :literal)
                      true nil)]
        (assoc base :from (:from token) :to (:to token) :value (:value token) :arity (:arity token))
        (error "Unexpected token")))
      (symbol-find (:symbol-table world) :end)))

(defn advance 
  ([world] (advance world nil))
  ([world expected-token-id]
    (let [token (next-token (:tokens world))]
      (assoc world :tokens (rest tokens) :token token))))

(defn extract-statement [token])

(defn extract-statements [token]
  (loop [statements []]
    (if (or (= :end (:id token)) (= "{" (:id token)))
      statements
      (if-let [statement (extract-statement token)]
        (recur (conj statements statement))
        statements))))

(defn parse [tokens]
  (let [world {:scope scope-proto
               :symbol-table base-symbol-table
               :tokens tokens}
        token nil
        token-nr 0]
    
    (advance world token token-nr)
    
    (println "parsed")
    {}))
