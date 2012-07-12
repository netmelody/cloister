(ns cloister.tokeniser
  (:require clojure.set))

(defn- error [token message] (println message token))

(def alpha? (set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def num? (set "0123456789"))
(def sign? #{\+ \-})
(def quote? #{\" \'})
(def line-break? #{\newline \return})
(def alpha-num? (clojure.set/union alpha? num? #{\_}))

(def escape-char-mapping {\b \backspace, \f \formfeed, \n \newline, \r \return, \t \tab})

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
        string-char? #(not (#{quote-char \\ \newline \return} %))]
    (loop [chars (rest text) value ""]
      (let [[string remainder] (chomp-while chars value string-char?)
            char (first remainder) xs (rest remainder)]
        (cond
          (= quote-char char) [(make-token :string string 0 0) (rest remainder)]
          (or (nil? char) (#{\n \r nil} char)) (error (make-token :string string 0 0) "Unterminated string")
          (= \\ char) (let [escaped-char (first xs)
                            [escape-char r] (if (= \u escaped-char) [(str (take 4 xs)) (drop 4 xs)] [(escape-char-mapping escaped-char) (rest xs)])]
                        (recur r (str string escape-char))))))))

(defn- next-token-from [text]
  (let [char (first text) remainder (rest text)]
    (cond
      (not char) [nil nil]
      (= \space char) [nil remainder]
      (alpha? char) (next-name-from text)
      (num? char) (next-num-from text)
      (quote? char) (next-string-from text)
      (and (= \/ char) (= \/ (first remainder))) [nil (rest (drop-while #(not (line-break? %)) remainder))])))

(defn tokenise
  ([text] (tokenise text #{"<" ">" "+" "-" "&"} #{"=" ">" "&" ":"}))
  ([text prefixes suffixes]
    (loop [tokens [] content text]
      (let [[token remainder] (next-token-from content)
            new-tokens (if token (conj tokens token) tokens)]
        (if remainder
          (recur new-tokens remainder)
          new-tokens)))))