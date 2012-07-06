(ns cloister.tokeniser)

(defn- make-token [type value from to]
  {:type type :value value :from from :to to})

(defn- next-name-from [text]
  [nil (rest text)])

(defn- next-token-from [text]
  (loop [chars text token-string ""]
    (let [char (first chars) remainder (rest chars)]
      (cond
        (not char) [nil []]
        (= \space char) (recur remainder token-string)
        (and (>= \a char) (<= \z char)) (next-name-from chars)
        (and (>= \A char) (<= \Z char)) (next-name-from chars)
        ))))

(defn tokenise
  ([text] (tokenise text #{"<" ">" "+" "-" "&"} #{"=" ">" "&" ":"}))
  ([text prefixes suffixes]
    (loop [tokens [] content text]
	    (let [[token remainder] (next-token-from content)]
	      (if (not token)
	        tokens
	        (recur (conj tokens token) remainder))))))
