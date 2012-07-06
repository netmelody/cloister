(ns cloister.tokeniser)

(defn- next-token-from [text]
  )

(defn tokenise [text]
  (loop [tokens [] content text]
    (let [[token remainder] (next-token-from content)]
      (recur (conj tokens token) remainder))))
