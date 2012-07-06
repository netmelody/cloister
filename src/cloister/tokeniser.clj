(ns cloister.tokeniser)

(defn- make-token [type value from to]
  {:type type :value value :from from :to to})

(defn- next-token-from [text]
  )

(defn tokenise
  ([text] (tokenise text #{"<" ">" "+" "-" "&"} #{"=" ">" "&" ":"}))
  ([text prefixes suffixes]
    (loop [tokens [] content text]
				    (let [[token remainder] (next-token-from content)]
				      (if (not token)
				        tokens
				        (recur (conj tokens token) remainder))))))
