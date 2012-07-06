(ns cloister.tokeniser)

(defn- make-token [type value from to]
  {:type type :value value :from from :to to})

(defn- next-token-from [text]
  )

(defn tokenise [text]
  (loop [tokens [] content text]
    (let [[token remainder] (next-token-from content)]
      (if (not token)
        tokens
        (recur (conj tokens token) remainder)))))
