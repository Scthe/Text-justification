(ns ^:figwheel-always text-justification.model
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.string :as gstring]
              [text-justification.utils :as utils]))

(enable-console-print!)

(defonce invisible-char (gstring/unescapeEntities "&nbsp;")) ;; \u00A0 ?


(defn- prepare-word
  "remove whitespaces, split word if it is too long"
  [max-len word-raw]
  (let [word (gstring/trim word-raw) len (count word)]
    (cond
      (<= max-len 0) word
      (= len 0) invisible-char
      (<= len max-len) word
      :else [(.substring word 0 max-len) (str " -" (prepare-word max-len (.substring word max-len)))]
    )))
; (println "norm:" (prepare-word 3 "abc"))
; (println "trim:" (prepare-word 3 "  abc  "))
; (println "long:" (prepare-word 2 "abc"))
; (println "test:" (prepare-word 11 "aaabbbccc111"))

(defn- badness
  "given line and page width and returns measurement how `pretty` the line is"
  [line page_width]
  (let [total_length (utils/line-length line)]
    (if (> total_length page_width)
      js/Infinity
      (utils/exp (- page_width total_length) 3))))

(defn- text_justification
  "given words to fit and page width returns vector of `pretty` measurement and vector of justified lines"
  [words page_width]
  (if (empty? words)
    [0.0 []]
    (reduce ( fn
      [acc word_id]
      (let [line (subvec words 0 word_id) rest_words (subvec words word_id)
            sub_problem_solution (text_justification rest_words page_width)
            ugliness (+ (badness line page_width) (first sub_problem_solution))
            solution_proposal (cons line (second sub_problem_solution))]
        ; (print "sub" line "|words in line" (count words))
        (if (< ugliness (first acc)) [ugliness solution_proposal] acc)))
      [js/Infinity []]
      (map inc (range (count words))) )))

(defn execute
  ""
  [words-raw page-width]
  (let [words (vec (flatten (map #(prepare-word page-width %) words-raw)))
        ; justified_lines words] ;; debug
        [_ justified_lines] (text_justification words page-width)]
        justified_lines))
