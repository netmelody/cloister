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

(defn symbol-find [symbol-type])

(defn advance 
  ([symbol-table scope tokens token-nr]
    (if (> token-nr (count tokens))
      (:end symbol-table)
      (advance symbol-table scope tokens token-nr (tokens token-nr))))
  ([symbol-table scope tokens token-nr token]
    (let [a (:type token)]
      (cond
        (= :name a) (scope-find scope (:value token))
        (= :operator a) (symbol-find (:value token))
        (= :string a) (symbol-find :literal)
        (= :number a) (symbol-find :literal)
        true (error "Unexpected token"))))
  )

(defn parse [tokens]
  (let [scope scope-proto
        symbol-table base-symbol-table
        token nil
        token-nr 0]
    
    (advance symbol-table scope tokens token token-nr)
    
    (println "parsed")
    {}))
