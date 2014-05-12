(ns clojure.test.check.generators.regex
  (:require [clojure.test.check.generators :as gen]))

(def metacharacters [\. \\ \? \* \+ \| \( \) \[ \]])

(def any
  (gen/such-that (complement #{\newline \return})
                 gen/char-ascii))

(def whitespaces #{\space \tab \newline \return})

(def whitespace
  (gen/elements (vec whitespaces)))

(def except-whitespace
  (gen/such-that (complement whitespaces) gen/char-ascii))

(gen/sample except-whitespace 100)

(def name-starts-chars [[58 58]
                        [95 95]
                        [65 90]
                        [97 122]
                        [0x00C0 0x00D6]
                        [0x00D8 0x00F6]
                        [0x00F8 0x02FF]
                        [0x0370 0x037D]
                        [0x037F 0x1FFF]
                        [0x200C 0x200D]
                        [0x2070 0x218F]
                        [0x2C00 0x2FEF]
                        [0x3001 0xD7FF]
                        [0xF900 0xFDCF]
                        [0xFDF0 0xFFFD]])

(def name-starts
  (gen/fmap char
            (gen/one-of (vec (map (partial apply gen/choose)
                                  name-starts-chars)))))


(def except-name-starts 
  (let [illegal (mapcat (fn [[l h]] (range l (inc h)))
                        name-starts-chars)
        illegal-chars (map char illegal)]
    (gen/such-that (complement (set illegal-chars)) gen/char)))

(def nmtokens (vec (concat name-starts-chars
                           [[45 46]
                            [48 57]
                            [0x00B7 0x00B7]
                            [0x0300 0x036F]
                            [0x203F 0x2040]])))

(def nmtoken
  (gen/fmap char
            (gen/one-of (vec (map (partial apply gen/choose)
                                  nmtokens)))))

(def except-nmtoken
  (let [illegal (mapcat (fn [[l h]] (range l (inc h)))
                        nmtokens)
        illegal-chars (map char illegal)]
    (gen/such-that (complement (set illegal-chars)) gen/char)))

;;(gen/sample except-nmtoken)

(def decimal
  (gen/fmap char (gen/choose 48 57)))

;;(gen/sample decimal)

(def except-decimal
  (gen/such-that (complement #(Character/isDigit %))
                 gen/char-ascii))

;;(gen/sample except-decimal)

(def multi-escape {\. any
                   \s whitespace
                   \S except-whitespace
                   \i name-starts
                   \I except-name-starts
                   \c nmtoken
                   \C except-nmtoken
                   \d decimal
                   \D except-decimal
                   \w nil
                   \W nil})


(def unicode-categories-by-cat {:L {:Lu [Character/UPPERCASE_LETTER]
                                    :Ll [Character/LOWERCASE_LETTER]
                                    :Lt [Character/TITLECASE_LETTER]
                                    :Lm [Character/MODIFIER_LETTER]
                                    :Lo [Character/OTHER_LETTER]}

                                :M {:Mn [Character/NON_SPACING_MARK]
                                    :Mc [Character/COMBINING_SPACING_MARK]
                                    :Me [Character/ENCLOSING_MARK]}

                                :N {:Nd [Character/DECIMAL_DIGIT_NUMBER]
                                    :Nl [Character/LETTER_NUMBER]
                                    :No [Character/OTHER_NUMBER]}

                                :P {:Pc [Character/CONNECTOR_PUNCTUATION]
                                    :Pd [Character/DASH_PUNCTUATION]
                                    :Ps [Character/START_PUNCTUATION]
                                    :Pe [Character/END_PUNCTUATION]
                                    :Pi [Character/INITIAL_QUOTE_PUNCTUATION]
                                    :Pf [Character/FINAL_QUOTE_PUNCTUATION]
                                    :Po [Character/OTHER_PUNCTUATION]}

                                :S {:Sm [Character/MATH_SYMBOL]
                                    :Sc [Character/CURRENCY_SYMBOL]
                                    :Sk [Character/MODIFIER_LETTER]
                                    :So [Character/OTHER_SYMBOL]}

                                :Z {:Zs [Character/SPACE_SEPARATOR]
                                    :Zl [Character/LINE_SEPARATOR]
                                    :Zp [Character/PARAGRAPH_SEPARATOR]}

                                :C {:Cc [Character/CONTROL]
                                    :Cf [Character/FORMAT]
                                    :Cs [Character/SURROGATE]
                                    :Co [Character/PRIVATE_USE]}})

(def unicode-categories
  (reduce (fn [acc [cat subcats]]
            (-> acc
                (assoc cat (apply concat (vals subcats)))
                (into subcats)))
          {}
          unicode-categories-by-cat))

(defn match-category [cat]
  (let [types (unicode-categories cat)
        pred (fn [ch]
               (some #(= % (Character/getType ch)) types))]
    (gen/such-that pred gen/char)))

(gen/sample (match-category :Nd))

unicode-categories

(def v
  "0000;<control>;Cc;0;BN;;;;;N;NULL;;;;
0001;<control>;Cc;0;BN;;;;;N;START OF HEADING;;;;
0002;<control>;Cc;0;BN;;;;;N;START OF TEXT;;;;
0003;<control>;Cc;0;BN;;;;;N;END OF TEXT;;;;
0004;<control>;Cc;0;BN;;;;;N;END OF TRANSMISSION;;;;")

(require '[clojure.string :as str] )
(let [lines (->> v
                 str/split-lines
                 (map #(str/split % #";")))]
  (for [line lines]
    [(nth line 0) (nth line 2)]))
