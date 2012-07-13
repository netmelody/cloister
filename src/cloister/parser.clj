(ns cloister.parser)

(defn- error [message] (println message))

(def symbol-table {})

(def symbol-proto
  {:id nil
   :value nil
   :null-denotation (fn [] (error "Undefined."))
   :left-denotation (fn [left] (error "Missing operator."))
   :statement-denotation (fn [] (error "Undefined."))
   :left-binding-power 0
   :right-binding-power 0
   })

(defn make-symbol
  ([id] (make-symbol id 0))
  ([id binding-power]
    (if-let [existing-symbol (symbol-table id)]
      (assoc existing-symbol :left-binding-power (max binding-power (:left-binding-power existing-symbol)))
      (assoc symbol-proto :id id :value id :left-binding-power binding-power)))
  )

(defn parse [tokens]
  (println "parsed")
  {})


