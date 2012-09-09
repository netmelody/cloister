(ns cloister.symbols
  (:require [cloister.parser.traversal])
  (:require [cloister.parser.scope]))

(defn- error [message] (println message))

(def symbol-proto
  {:id nil
   :value nil
   :null-denotation (fn [world] (error "Undefined."))
   :left-denotation (fn [world left] (error "Missing operator."))
   :statement-denotation (fn [world] (error "Undefined."))
   :left-binding-power 0
   :right-binding-power 0})

(defn make-symbol
  ([id] (make-symbol id 0))
  ([id binding-power] (assoc symbol-proto :id id :value id :left-binding-power binding-power)))

(defn make-constant [id value]
  (assoc (make-symbol id)
         :value value
         :null-denotation (fn [world]
                            (let [constant (assoc ((:symbol-table world) id) :arity :literal)]
                              [(assoc world :scope (scope-reserve (:scope world) constant)) constant]))))

(defn make-offset-infix
  ([id binding-power offset
    (make-infix id binding-power offset (fn [world left]
                                          (let [[new-world expr] (extract-expression world (- binding-power offset))
                                                infix (assoc ((:symbol-table world) id) :first left :second expr :arity :binary)]
                                            [new-world infix])))])
  ([id binding-power offset left-denotation]
    (assoc (make-symbol id binding-power) :left-denotation left-denotation)))

(defn make-infix
  ([id binding-power] (make-offset-infix id binding-power 0))
  ([id binding-power left-denotation] (make-offset-infix id binding-power 0 left-denotation)))

(defn make-infixr
  ([id binding-power] (make-offset-infix id binding-power -1))
  ([id binding-power left-denotation] (make-offset-infix id binding-power -1 left-denotation)))

(defn make-assignment [id]
  (make-infixr id 10 (fn [world left] )))

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