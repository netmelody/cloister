(ns cloister.tokeniser)

(def alpha? (set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def num? (set "0123456789"))
(defn- alpha-num? [char] (or (alpha? char) (num? char) (= \_ char)))

(defn- make-token [type value from to]
  {:type type :value value :from from :to to})

(defn- chomp-while [text f?]
  (loop [chars text value ""]
    (let [char (first chars) remainder (rest chars)]
      (if (f? char)
        (recur remainder (str value char))
        [value chars]))))

(defn- next-name-from [text]
  (let [[name remainder] (chomp-while text alpha-num?)]
    [(make-token :name name 0 0) remainder]))

(defn- next-int-from [text]
  (let [[number-str remainder] (chomp-while text num?)]
    [(make-token :number (Double/parseDouble number-str) 0 0) remainder]))

(defn- next-num-from [text]
  (let [[first-num-str remainder] (chomp-while text num?)]
    (if (= \. (first remainder))
      (let [[second-num-str remainder] (chomp-while (rest remainder) num?)]
        [(make-token :number (Double/parseDouble (str first-num-str "." second-num-str)) 0 0) remainder])
      [(make-token :number (Double/parseDouble first-num-str) 0 0) remainder])))

(defn- next-token-from [text]
  (loop [chars text token-string ""]
    (let [char (first chars) remainder (rest chars)]
      (cond
        (not char) [nil []]
        (= \space char) (recur remainder token-string)
        (alpha? char) (next-name-from chars)
        (num? char) (next-num-from chars)
      ))))

(defn tokenise
  ([text] (tokenise text #{"<" ">" "+" "-" "&"} #{"=" ">" "&" ":"}))
  ([text prefixes suffixes]
    (loop [tokens [] content text]
	    (let [[token remainder] (next-token-from content)]
	      (if (not token)
	        tokens
	        (recur (conj tokens token) remainder))))))
