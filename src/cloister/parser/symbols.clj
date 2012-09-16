(ns cloister.parser.symbols
  (:require [cloister.parser.traversal])
  (:require [cloister.parser.scope]))

(defn- error [message] (println message))
(defn- itself [map property] (assoc map property (fn [&] (itself map property))))
(defn- scope-reserve [world token] (assoc world :scope (cloister.parser.scope/scope-reserve (:scope world) token)))

(def symbol-proto
  {:id nil
   :value nil
   :null-denotation (fn [world] (error "Undefined.") [world nil])
   :left-denotation (fn [world left] (error "Missing operator.") [world nil])
   :statement-denotation (fn [world] (error "Undefined.") [world nil])
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
                              [(scope-reserve world constant) constant]))))

(defn make-offset-infix
  ([id binding-power offset]
    (make-offset-infix id binding-power offset (fn [world left]
                                          (let [[new-world expr] (cloister.parser.traversal/extract-expression world (- binding-power offset))
                                                infix (assoc ((:symbol-table world) id) :first left :second expr :arity :binary)]
                                            [new-world infix]))))
  ([id binding-power offset left-denotation]
    (assoc (make-symbol id binding-power) :left-denotation left-denotation)))

(defn make-infix
  ([id binding-power] (make-offset-infix id binding-power 0))
  ([id binding-power left-denotation] (make-offset-infix id binding-power 0 left-denotation)))

(defn make-infixr
  ([id binding-power] (make-offset-infix id binding-power -1))
  ([id binding-power left-denotation] (make-offset-infix id binding-power -1 left-denotation)))

(defn make-assignment [id]
  (assoc (make-symbol id 10) :left-denotation (fn [world left]
                                                (let [[new-world expr] (cloister.parser.traversal/extract-expression world 9)
                                                      infix (assoc ((:symbol-table world) id) :first left :second expr :arity :binary :assignment true)]
                                                  [new-world infix]))))

(defn make-prefix
  ([id] (make-prefix id (fn [world] (let [[new-world expr] (cloister.parser.traversal/extract-expression world 70)
                                          prefix (assoc ((:symbol-table world) id) :first expr :arity :unary)]
                                      [(scope-reserve new-world prefix) prefix]))))
  ([id nud] (assoc (make-symbol id) :null-denotation nud)))

(defn make-statement [id statement-denotation]
  (assoc (make-symbol id) :statement-denotation statement-denotation))

(defn register-symbol [table symbol]
  (let [id (:id symbol)]
    (if-let [existing-symbol (table id)]
      (assoc table id (assoc existing-symbol :left-binding-power (max (:left-binding-power symbol) (:left-binding-power existing-symbol))))
      (assoc table id symbol))))

(defn- extract-assignment [world name-token]
  (let [token (:token world)]
    (if (= "=" (:id token))
      (let [[new-world expr] (cloister.parser.traversal/extract-expression (cloister.parser.traversal/advance world "=") 0)
            assignment (assoc token :first name-token :second expr :arity :binary)]
        [new-world assignment]))
    [world nil]))

(defn- var-std [world]
  (loop [w world assignments []]
    (let [name-token (:token w)]
      (if (not (= :name (:arity name-token)))
        (error "expected a new variable name.")
        (let [[w2 assignment] (extract-assignment (scope-reserve (cloister.parser.traversal/advance w) name-token) name-token)
              a2 (if assignment (conj assignments assignment) assignments)]
          (if (= "," (:id (:token w2)))
            (recur (cloister.parser.traversal/advance w2 ",") a2)
            [(cloister.parser.traversal/advance w2 ";" a2)]))))))

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
                         (register-symbol (make-constant "Array" []))
                         (register-symbol (itself (make-symbol :literal) :null-denotation))
                         (register-symbol (assoc (make-symbol "this") :null-denotation (fn [world] 1)))
                         (register-symbol (make-assignment "="))
                         (register-symbol (make-assignment "+="))
                         (register-symbol (make-assignment "-="))
                         (register-symbol (make-statement "var" var-std))))

