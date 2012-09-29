(ns cloister.test.parser
  (:use [cloister.parser])
  (:use [clojure.test]))

(deftest parses-empty-vector-of-tokens
  (is (= [] (cloister.parser/parse []))))

; var i = 1;
(deftest parses-simple-assignment
  (is (= [{:value "="
           :arity :binary
           :first  {:value "i" :arity :name}
           :second {:value  1  :arity :literal}}]
         (cloister.parser/parse [{:from 0 :to 3 :type :name     :value "var"}
                                 {:from 4 :to 5 :type :name     :value "i"}
                                 {:from 5 :to 6 :type :operator :value "="}
                                 {:from 6 :to 7 :type :number   :value 1}
                                 {:from 7 :to 8 :type :operator :value ";"}]))))

; var a = [1, 2, 3];
(deftest parses-simple-array-assignment
  (is (= [{:value "="
           :arity :binary
           :first {:value "a" :arity :name}
           :second {:value "[" :arity :unary :first [{:value 1 :arity :literal} {:value 2 :arity :literal} {:value 3 :arity :literal}]}}]
         (cloister.parser/parse [{:from 0  :to 3  :type :name     :value "var"}
                                 {:from 4  :to 5  :type :name     :value "a"}
                                 {:from 6  :to 7  :type :operator :value "="}
                                 {:from 8  :to 9  :type :operator :value "["}
                                 {:from 9  :to 10 :type :number   :value 1}
                                 {:from 10 :to 11 :type :operator :value ","}
                                 {:from 12 :to 13 :type :number   :value 2}
                                 {:from 13 :to 14 :type :operator :value ","}
                                 {:from 15 :to 16 :type :number   :value 3}
                                 {:from 16 :to 17 :type :operator :value "]"}
                                 {:from 17 :to 18 :type :operator :value ";"}]))))

; var o = true ? 1.0 : 2.0;
(deftest parses-ternery-operator
  (is (= [{:value "="
           :arity :binary
           :first  {:value "o" :arity :name}
           :second {:value "?" :arity :ternary :first  {:value true :arity :literal}
                                               :second {:value 1.0  :arity :literal}
                                               :third  {:value 2.0  :arity :literal}}}]
         (cloister.parser/parse [{:to 3  :from 0  :type :name     :value "var"}
                                 {:to 5  :from 4  :type :name     :value "o"}
                                 {:to 7  :from 6  :type :operator :value "="}
                                 {:to 12 :from 8  :type :name     :value "true"}
                                 {:to 14 :from 13 :type :operator :value "?"}
                                 {:to 16 :from 15 :type :number   :value 1.0}
                                 {:to 18 :from 17 :type :operator :value ":"}
                                 {:to 20 :from 19 :type :number   :value 2.0}
                                 {:to 21 :from 20 :type :operator :value ";"}]))))

; var i = function() { return 1; };
(deftest parses-simple-function-declaration
  (is (= [{:value "="
           :arity :binary
           :first  {:value "i" :arity :name}
           :second {:value "function"
                    :arity :function
                    :first []
                    :second [{:value "return" :arity :statement :first {:value 1 :arity :literal}}]}}]
         (cloister.parser/parse [{:from 0  :to 3  :type :name     :value "var"}
                                 {:from 4  :to 5  :type :name     :value "i"}
                                 {:from 6  :to 7  :type :operator :value "="}
                                 {:from 8  :to 16 :type :name     :value "function"}
                                 {:from 16 :to 17 :type :operator :value "("}
                                 {:from 17 :to 18 :type :operator :value ")"}
                                 {:from 19 :to 20 :type :operator :value "{"}
                                 {:from 21 :to 27 :type :name     :value "return"}
                                 {:from 28 :to 29 :type :number   :value 1}
                                 {:from 29 :to 30 :type :operator :value ";"}
                                 {:from 31 :to 32 :type :operator :value "}"}
                                 {:from 32 :to 33 :type :operator :value ";"}]))))

; var j = {"foo": 1}.foo;
(deftest parses-object-access
  (is (= [{:value "="
           :arity :binary
           :first  {:value "j" :arity :name}
           :second {:value "."
                    :arity :binary
                    :first {:value "{"
                            :arity :unary
                            :first [{:key "foo" :value 1 :arity :literal}]}
                    :second {:value "foo" :arity :literal}}}]
         (cloister.parser/parse [{:from 0  :to 3  :type :name     :value "var"}
                                 {:from 4  :to 5  :type :name     :value "j"  }
                                 {:from 6  :to 7  :type :operator :value "="  }
                                 {:from 8  :to 9  :type :operator :value "{"  }
                                 {:from 9  :to 14 :type :string   :value "foo"}
                                 {:from 14 :to 15 :type :operator :value ":"  }
                                 {:from 16 :to 17 :type :number   :value 1    }
                                 {:from 17 :to 18 :type :operator :value "}"  }
                                 {:from 18 :to 19 :type :operator :value "."  }
                                 {:from 19 :to 22 :type :name     :value "foo"}
                                 {:from 22 :to 23 :type :operator :value ";"  }]))))

; var k = function(a) { if (a) {return 1;} else {return 2;} };
(deftest parses-func-with-args-and-if-statement
  (is (= [{:value "="
           :arity :binary
           :first {:value "k" :arity :name}
           :second {:value "function"
                    :arity :function
                    :first [{:value "a" :arity :name}]
                    :second {:value "if"
                             :arity :statement
                             :first {:value "a" :arity :name}
                             :second {:value "return"
                                      :arity :statement
                                      :first {:value 1 :arity :literal}}
                             :third {:value "return"
                                     :arity :statement
                                     :first {:value 2 :arity :literal}}}}}]
         (cloister.parser/parse [{:from 0  :to 3  :type :name     :value "var"      }
                                 {:from 4  :to 5  :type :name     :value "k"        }
                                 {:from 6  :to 7  :type :operator :value "="        }
                                 {:from 8  :to 16 :type :name     :value "function" }
                                 {:from 16 :to 17 :type :operator :value "("        }
                                 {:from 17 :to 18 :type :name     :value "a"        }
                                 {:from 18 :to 19 :type :operator :value ")"        }
                                 {:from 20 :to 21 :type :operator :value "{"        }
                                 {:from 22 :to 24 :type :name     :value "if"       }
                                 {:from 25 :to 26 :type :operator :value "("        }
                                 {:from 26 :to 27 :type :name     :value "a"        }
                                 {:from 27 :to 28 :type :operator :value ")"        }
                                 {:from 29 :to 30 :type :operator :value "{"        }
                                 {:from 30 :to 36 :type :name     :value "return"   }
                                 {:from 37 :to 38 :type :number   :value 1          }
                                 {:from 38 :to 39 :type :operator :value ";"        }
                                 {:from 39 :to 40 :type :operator :value "}"        }
                                 {:from 41 :to 45 :type :name     :value "else"     }
                                 {:from 46 :to 47 :type :operator :value "{"        }
                                 {:from 47 :to 53 :type :name     :value "return"   }
                                 {:from 54 :to 55 :type :number   :value 2          }
                                 {:from 55 :to 56 :type :operator :value ";"        }
                                 {:from 56 :to 57 :type :operator :value "}"        }
                                 {:from 58 :to 59 :type :operator :value "}"        }
                                 {:from 59 :to 60 :type :operator :value ";"        }]))))
