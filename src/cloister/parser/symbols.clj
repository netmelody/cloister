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

(defn make-offset-infix
  ([id binding-power offset]
    (make-offset-infix id binding-power offset (fn [world token left]
                                          (let [[new-world expr] (extract-expression world (- binding-power offset))
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
  (assoc (make-symbol id 10) :left-denotation (fn [world token left]
                                                (let [[new-world expr] (extract-expression world 9)
                                                      infix (assoc ((:symbol-table world) id) :first left :second expr :arity :binary :assignment true)]
                                                  [new-world infix]))))

(defn make-prefix
  ([id] (make-prefix id (fn [world token] (let [[new-world expr] (extract-expression world 70)
                                                prefix (assoc ((:symbol-table world) id) :first expr :arity :unary)]
                                            [(reserve new-world prefix) prefix]))))
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
      (let [[new-world expr] (extract-expression (advance world "=") 0)
            assignment (assoc token :first name-token :second expr :arity :binary)]
        [new-world assignment])
      [world nil])))

(defn- one-or-many [arr]
  (let [len (count arr)]
    (cond (= 0 len) nil (= 1 len) (arr 0) (> 1 len) arr)))

(defn- var-std [world token]
  (loop [w world assignments []]
    (let [name-token (:token w)]
      (if (not (= :name (:arity name-token)))
        (report-error name-token "expected a new variable name.")
        (let [[w2 assignment] (extract-assignment (reserve (advance w) name-token) name-token)
              a2 (if assignment (conj assignments assignment) assignments)]
          (if (= "," (:id (:token w2)))
            (recur (advance w2 ",") a2)
            [(advance w2 ";") (one-or-many a2)]))))))

(defn- this-nud [world token]
  (let [this (assoc token :arity :this)]
    [(reserve world this) this]))

(defn- ?-led [world token left]
  (let [[w1 expr1] (extract-expression world 0)
        [w2 expr2] (extract-expression (advance w1 ":") 0)]
    [w2 (assoc token :first left :second expr1 :third expr2 :arity :ternary)]))

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
                         (register-symbol (assoc (make-symbol :literal) :null-denotation (fn [world token] [world token])))
                         (register-symbol (assoc (make-symbol "this") :null-denotation this-nud))
                         (register-symbol (make-assignment "="))
                         (register-symbol (make-assignment "+="))
                         (register-symbol (make-assignment "-="))
                         (register-symbol (make-infix "?" 20 ?-led))
                         (register-symbol (make-statement "var" var-std))))
