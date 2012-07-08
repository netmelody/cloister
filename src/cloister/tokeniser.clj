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
  (let [[first-num-str r1] (chomp-while text num?)]
    (if (= \. (first r1))
      (let [[second-num-str r2] (chomp-while (rest r1) num?)]
        (if (or (= \e (first r2)) (= \E (first r2)))
          (let [[exp-num-str r3] (chomp-while (rest r2) num?)]
            [(make-token :number (Double/parseDouble (str first-num-str "." second-num-str "E" exp-num-str)) 0 0) r3])
          [(make-token :number (Double/parseDouble (str first-num-str "." second-num-str)) 0 0) r2]))
      [(make-token :number (Double/parseDouble first-num-str) 0 0) r1])))

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
