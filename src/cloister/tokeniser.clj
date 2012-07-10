(ns cloister.tokeniser)

(defn- error [token message] (println message token))

(def alpha? (set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def num? (set "0123456789"))
(def sign? #{\+ \-})
(def quote? #{\" \'})
(defn- alpha-num? [char] (or (alpha? char) (num? char) (= \_ char)))

(defn- make-token [type value from to]
  {:type type :value value :from from :to to})

(defn- chomp-while 
  ([text f?] (chomp-while text "" f?))
  ([text prefix f?]
    (loop [chars text, value prefix]
      (let [char (first chars) remainder (rest chars)]
        (if (f? char)
          (recur remainder (str value char))
          [value chars])))))

(defn- chomp-pattern [text pattern]
  (loop [operations pattern, chars text, value ""]
    (if-let [operation (first operations)]
      (let [[prefix operand] ((:when operation) chars)
            [result remainder] (if operand (chomp-while operand (str value prefix) (:while operation)) [value chars])]
        (recur (rest operations) remainder result))
      [value chars])))

(defn- next-name-from [text]
  (let [[name remainder] (chomp-while text alpha-num?)]
    [(make-token :name name 0 0) remainder]))

(defn- next-num-from [text]
  (let [[num-str remainder] (chomp-pattern text [{:when #(identity ["" %]) :while num?}
                                                 {:when #(if (= \. (first %)) [\. (rest %)]) :while num?}
                                                 {:when #(if (or (= \e (first %)) (= \E (first %))) (chomp-while (rest %) "E" sign?)) :while num?}])
        number (try (Double/parseDouble num-str) (catch Exception e))
        token (make-token :number (or number num-str) 0 0)]
    (if (or (not number) (alpha? (first remainder)))
      (error token "Bad number"))
    [token remainder]))

(defn- next-string-from [text]
  (let [quote-char (first text)
        [string remainder] (chomp-while (rest text) #(not (= % quote-char)))]
    [(make-token :string string 0 0) (rest remainder)]))

(defn- next-token-from [text]
  (loop [chars text token-string ""]
    (let [char (first chars) remainder (rest chars)]
      (cond
        (not char) [nil []]
        (= \space char) (recur remainder token-string)
        (alpha? char) (next-name-from chars)
        (num? char) (next-num-from chars)
        (quote? char) (next-string-from chars)
      ))))

(defn tokenise
  ([text] (tokenise text #{"<" ">" "+" "-" "&"} #{"=" ">" "&" ":"}))
  ([text prefixes suffixes]
    (loop [tokens [] content text]
	    (let [[token remainder] (next-token-from content)]
	      (if (not token)
	        tokens
	        (recur (conj tokens token) remainder))))))
