(ns cloister.parser.symbols
  (:use [cloister.parser.util :only [report-error]])
  (:use [cloister.parser.scope :only [scope-find scope-reserve]])
  (:use [cloister.parser.traversal :only [advance extract-expression]]))

(defn- reserve [world token] (assoc world :scope (scope-reserve (:scope world) token)))

(def symbol-proto
  {:id nil
   :value nil
   :null-denotation (fn [world token] (report-error (:token world) "Undefined.") [world nil])
   :left-denotation (fn [world token left] (report-error (:token world) "Missing operator.") [world nil])
   ;:statement-denotation (fn [world token] (report-error (:token world) "Undefined.") [world nil])
   :left-binding-power 0
   :right-binding-power 0})

(defn make-symbol
  ([id] (make-symbol id 0))
  ([id binding-power] (assoc symbol-proto :id id :value id :left-binding-power binding-power)))

(defn make-constant [id value]
  (assoc (make-symbol id)
         :value value
         :null-denotation (fn [world token]
                            (let [constant (assoc ((:symbol-table world) id) :arity :literal)]
                              [(reserve world constant) constant]))))

(defn make-offset-infix [id binding-power offset]
  (assoc (make-symbol id binding-power)
         :left-denotation (fn [world token left]
                            (let [[new-world expr] (extract-expression world (- binding-power offset))
                                  infix (assoc ((:symbol-table world) id) :first left :second expr :arity :binary)]
                              [new-world infix]))))

(defn make-infix [id binding-power] (make-offset-infix id binding-power 0))
(defn make-infixr [id binding-power] (make-offset-infix id binding-power -1))

(defn make-assignment [id]
  (assoc (make-symbol id 10) :left-denotation (fn [world token left]
                                                (let [[new-world expr] (extract-expression world 9)
                                                      infix (assoc ((:symbol-table world) id) :first left :second expr :arity :binary :assignment true)]
                                                  [new-world infix]))))

(defn make-prefix
  ([id] (make-prefix id (fn [world token] (let [[new-world expr] (extract-expression world 70)
                                                prefix (assoc ((:symbol-table world) id) :first expr :arity :unary)]
                                            [(reserve new-world prefix) prefix]))))
  ([id nud] (assoc (make-symbol id) :null-denotation nud)))

(defn register-symbol [table symbol]
  (let [id (:id symbol)]
    (if-let [existing-symbol (table id)]
      (assoc table id (assoc existing-symbol :left-binding-power (max (:left-binding-power symbol) (:left-binding-power existing-symbol))))
      (assoc table id symbol))))

(defn- extract-assignment [world name-token]
  (let [token (:token world)]
    (if (= "=" (:id token))
      (let [[new-world expr] (extract-expression (advance world "=") 0)
            assignment (assoc token :first name-token :second expr :arity :binary)]
        [new-world assignment])
      [world nil])))

(defn- one-or-many [arr]
  (let [len (count arr)]
    (cond (= 0 len) nil (= 1 len) (arr 0) (> 1 len) arr)))


; Symbol Definitions

(def ^{:private true} _literal
  (assoc (make-symbol :literal)
         :null-denotation (fn [world token] [world token])))

(def ^{:private true} _this
  (assoc (make-symbol "this")
         :null-denotation (fn [world token]
                            (let [this (assoc token :arity :this)]
                              [(reserve world this) this]))))

(def ^{:private true} _?
  (assoc (make-infix "?" 20)
         :left-denotation (fn [world token left]
                            (let [[w1 expr1] (extract-expression world 0)
                                  [w2 expr2] (extract-expression (advance w1 ":") 0)]
                              [w2 (assoc token :first left :second expr1 :third expr2 :arity :ternary)]))))

(def ^{:private true} _var
  (assoc (make-symbol "var")
         :statement-denotation (fn [world token]
                                 (loop [w world assignments []]
                                   (let [name-token (:token w)]
                                     (if (not (= :name (:arity name-token)))
                                       (report-error name-token "expected a new variable name.")
                                       (let [[w2 assignment] (extract-assignment (reserve (advance w) name-token) name-token)
                                             a2 (if assignment (conj assignments assignment) assignments)]
                                         (if (= "," (:id (:token w2)))
                                           (recur (advance w2 ",") a2)
                                           [(advance w2 ";") (one-or-many a2)]))))))))

(def base-symbol-table (-> {}
                         (register-symbol (make-symbol :end))
                         (register-symbol (make-symbol :name))
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
                         (register-symbol _literal)
                         (register-symbol _this)
                         (register-symbol (make-assignment "="))
                         (register-symbol (make-assignment "+="))
                         (register-symbol (make-assignment "-="))
                         (register-symbol _?)
                         (register-symbol _var)))
