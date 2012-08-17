(ns cloister
  "javascript interpreter in clojure"
  (:require [cloister.tokeniser])
  (:require [cloister.parser])
  )

(cloister.parser/parse
  (cloister.tokeniser/tokenise "(function() { cnt=-12.25e-14; alert(\"f\\too\" <= 'ba\\nr'); }()) //go"))

