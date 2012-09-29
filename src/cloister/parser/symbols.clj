(ns cloister.parser.symbols
  (:use [cloister.parser.util :only [report-error]])
  (:use [cloister.parser.scope :only [scope-find scope-reserve scope-create-child]])
  (:use [cloister.parser.traversal :only [advance extract-expression extract-statement extract-statements extract-assignment extract-block]]))

(defn- reserve [world token] (assoc world :scope (scope-reserve (:scope world) token)))

(def symbol-proto
  {:id nil
   :value nil
   :null-denotation (fn [world identity-token] (report-error identity-token "Undefined.") [world nil])
   :left-denotation (fn [world identity-token left] (report-error identity-token "Missing operator.") [world nil])
   :statement-denotation nil
   :left-binding-power 0
   :right-binding-power 0})

(defn make-symbol
  ([id] (make-symbol id 0))
  ([id binding-power] (assoc symbol-proto :id id :value id :left-binding-power binding-power)))

(defn make-constant [id value]
  (assoc (make-symbol id)
         :value value
         :null-denotation (fn [world identity-token]
                            (let [constant (assoc ((:symbol-table world) id) :arity :literal)]
                              [(reserve world constant) constant]))))

(defn make-offset-infix [id binding-power offset]
  (assoc (make-symbol id binding-power)
         :left-denotation (fn [world identity-token left]
                            (let [[new-world expr] (extract-expression world (- binding-power offset))
                                  infix (assoc ((:symbol-table world) id) :first left :second expr :arity :binary)]
                              [new-world infix]))))

(defn make-infix [id binding-power] (make-offset-infix id binding-power 0))
(defn make-infixr [id binding-power] (make-offset-infix id binding-power -1))

(defn make-assignment [id]
  (assoc (make-symbol id 10) :left-denotation (fn [world identity-token left]
                                                (let [[new-world expr] (extract-expression world 9)
                                                      infix (assoc ((:symbol-table world) id) :first left :second expr :arity :binary :assignment true)]
                                                  [new-world infix]))))

(defn make-prefix [id]
  (assoc (make-symbol id) :null-denotation (fn [world identity-token]
                                             (let [[new-world expr] (extract-expression world 70)
                                                   prefix (assoc ((:symbol-table world) id) :first expr :arity :unary)]
                                               [(reserve new-world prefix) prefix]))))

(defn register-symbol [table symbol]
  (let [id (:id symbol)]
    (if-let [existing-symbol (table id)]
      (assoc table id (assoc existing-symbol :left-binding-power (max (:left-binding-power symbol) (:left-binding-power existing-symbol))))
      (assoc table id symbol))))

; kill me?
(defn- one-or-many [arr]
  (let [len (count arr)]
    (cond (= 0 len) nil (= 1 len) (arr 0) (> 1 len) arr)))


; Symbol Definitions

(def ^{:private true} _literal
  (assoc (make-symbol :literal)
         :null-denotation (fn [world identity-token] [world identity-token])))

(def ^{:private true} _this
  (assoc (make-symbol "this")
         :null-denotation (fn [world identity-token]
                            (let [this (assoc identity-token :arity :this)]
                              [(reserve world this) this]))))

(def ^{:private true} _dot
  (assoc (make-symbol "." 80)
         :left-denotation (fn [world identity-token left]
                            (let [token (:token world)]
                              (if (not (= :name (:arity token)))
                                (report-error token "Expected a property name."))
                              [(advance world) (assoc identity-token :first left :second (assoc token :arity :literal) :arity :binary)]))))

(def ^{:private true} _open-parens
  (assoc (make-symbol "(" 80)
         :left-denotation (fn [world identity-token left]
                            (report-error identity-token "led not yet implemented!"))
         :null-denotation (fn [world identity-token]
                            (let [[w1 expr] (extract-expression world 0)]
                              [(advance w1 ")") expr]))))

(def ^{:private true} _open-square
  (assoc (make-symbol "[" 80)
         :left-denotation (fn [world identity-token left]
                            (let [[w1 expr] (extract-expression world 0)]
                              [(advance w1 "]") (assoc identity-token :first left :second expr :arity :binary)]))
         :null-denotation (fn [world identity-token]
                            (let [[w expressions] (if (= "]" (:id (:token world)))
                                                    [world []]
                                                    (loop [w1 world expressions []]
                                                      (let [[w2 expr] (extract-expression w1 0)
                                                            exprs (conj expressions expr)]
                                                        (if (= "," (:id (:token w2)))
                                                          (recur (advance w2 ",") exprs)
                                                          [w2 exprs]))))]
                              [(advance w "]") (assoc identity-token :first expressions :arity :unary)]))))

(def ^{:private true} _open-curly
  (assoc (make-symbol "{")
         :null-denotation (fn [world identity-token]
                            (let [[w expressions] (if (= "}" (:id (:token world)))
                                                    [world []]
                                                    (loop [w1 world expressions []]
                                                      (let [name-token (:token w1)]
                                                        (if (and (not (= :name (:arity name-token))) (not (= :literal (:arity name-token))))
                                                          (report-error name-token "Bad property name"))
                                                        (let [[w2 expr] (extract-expression (advance (advance w1) ":") 0)
                                                              exprs (conj expressions (assoc expr :key (:value name-token)))]
                                                          (if (= "," (:id (:token w2)))
                                                            (recur (advance w2 ",") exprs)
                                                            [w2 exprs])))))]
                              [(advance w "}") (assoc identity-token :first expressions :arity :unary)]))
         :statement-denotation (fn [world identity-token]
                                 (report-error identity-token "std not yet implemented!"))))

(def ^{:private true} _?
  (assoc (make-symbol "?" 20)
         :left-denotation (fn [world identity-token left]
                            (let [[w1 expr1] (extract-expression world 0)
                                  [w2 expr2] (extract-expression (advance w1 ":") 0)]
                              [w2 (assoc identity-token :first left :second expr1 :third expr2 :arity :ternary)]))))

(def ^{:private true} _var
  (assoc (make-symbol "var")
         :statement-denotation (fn [world identity-token]
                                 (loop [w world assignments []]
                                   (let [name-token (:token w)]
                                     (if (not (= :name (:arity name-token)))
                                       (report-error name-token "expected a new variable name.")
                                       (let [[w2 assignment] (extract-assignment (reserve (advance w) name-token) name-token)
                                             a2 (if assignment (conj assignments assignment) assignments)]
                                         (if (= "," (:id (:token w2)))
                                           (recur (advance w2 ",") a2)
                                           [(advance w2 ";") (one-or-many a2)]))))))))

(def ^{:private true} _if
  (assoc (make-symbol "if")
         :statement-denotation (fn [world identity-token]
                                 (let [[w1 first] (extract-expression (advance world "(") 0)
                                       [w2 second] (extract-block (advance w1 ")"))
                                       [w3 third] (if (= ("else" (:id (:token w2))))
                                                    (let [w3a (advance (reserve w2 (:token w2)) "else")]
                                                      (if (= "if" (:id (:token w3a))) (extract-statement w3a) (extract-block w3a)))
                                                    [w2 nil])]
                                   [w3 (assoc identity-token :first first :second second :third third :arity :statement)]))))


(def ^{:private true} _return
  (assoc (make-symbol "return")
         :statement-denotation (fn [world identity-token]
                                 (let [[w1 first] (if (not (= ";" (:id (:token world)))) (extract-expression world 0) [world nil])
                                       this (if (nil? first) identity-token (assoc identity-token :first first))
                                       w2 (advance w1 ";")]
                                   (if (not (= "}" (:id (:token w2))))
                                     (report-error (:token w2) "Unreachable statement"))
                                   [w2 (assoc this :arity :statement)]))))

(def ^{:private true} _function
  (assoc (make-symbol "function")
         :null-denotation (fn [world identity-token]
                            (let [token (:token world)
                                  w1 (assoc world :scope (scope-create-child (:scope world)))
                                  [this w2] (if (= :name (:arity token)) [(assoc identity-token :name (:value token)) (advance (reserve w1 token))] [identity-token w1])
                                  w3 (advance w2 "(")
                                  [w4 params] (if (= ")" (:id (:token w3)))
                                              [w3 []]
                                              (loop [w3a w3 parameters []]
                                                (let [name-token (:token w3a)]
                                                  (if (not (= :name (:arity name-token)))
                                                    (report-error name-token "Expected a parameter name"))
                                                  (let [w3b (advance (reserve w3a name-token)) 
                                                        params (conj parameters name-token)]
                                                    (if (= "," (:id (:token w3b)))
                                                      (recur (advance w3b ",") params)
                                                      [w3b params])))))]
                              (let [[w5 statements] (extract-statements (advance (advance w4 ")") "{"))
                                    w6 (advance w5 "}")]
                                [(assoc w6 :scope (:parent (:scope w6))) (assoc this :first [] :arity :function :second statements)])))))

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
                         (register-symbol (make-infixr "&&"  30))
                         (register-symbol (make-infixr "||"  30))
                         (register-symbol (make-infixr "===" 40))
                         (register-symbol (make-infixr "!==" 40))
                         (register-symbol (make-infixr "<"   40))
                         (register-symbol (make-infixr "<="  40))
                         (register-symbol (make-infixr ">"   40))
                         (register-symbol (make-infixr ">="  40))
                         (register-symbol (make-infixr "+"   50))
                         (register-symbol (make-infixr "-"   50))
                         (register-symbol (make-infixr "*"   60))
                         (register-symbol (make-infixr "/"   60))
                         (register-symbol _dot)
                         (register-symbol _open-square)
                         (register-symbol _open-parens)
                         (register-symbol _open-curly)
                         (register-symbol (make-prefix "!")) 
                         (register-symbol (make-prefix "-")) 
                         (register-symbol (make-prefix "typeof"))
                         (register-symbol _function)
                         (register-symbol _var)
                         (register-symbol _if)
                         (register-symbol _return)))
