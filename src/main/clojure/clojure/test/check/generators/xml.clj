(ns clojure.test.check.generators.xml
  (:require [clojure.test.check.generators :as gen]))

(tag schema {:hello gen/string :number gen/int})
(defn tag [nm attrs]
  (gen/bind (apply gen/vector (vals attrs))
            (fn [values]
              {:tag (keyword nm) :attrs (zipmap (keys attrs) values)})))
