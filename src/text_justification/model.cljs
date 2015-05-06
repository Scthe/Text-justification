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
  [line page-width]
  (let [total-length (utils/line-length line)]
    (if (> total-length page-width)
      js/Infinity
      (utils/exp (- page-width total-length) 3))))

(defn- text-justification-inner
  "given words to fit and page width returns vector of `pretty` measurement and vector of justified lines"
  [words page-width]
  (if (empty? words)
    [0.0 []]
    (reduce
      (fn [acc word-id]
         (let [line (subvec words 0 word-id) rest-words (subvec words word-id)
               sub-problem-solution (text-justification-inner rest-words page-width)
               ugliness (+ (badness line page-width) (first sub-problem-solution))
               solution-proposal (cons line (second sub-problem-solution))]
          ; (print "sub" line "|words in line" (count words))
          (if (< ugliness (first acc)) [ugliness solution-proposal] acc)))
      [js/Infinity []]
      (map inc (range (count words))) )))

(defn text-justification
  "justify text provided as a collection of words to given page width"
  [words-raw page-width]
  {:pre [(> page-width 0)]}
  (second
    (text-justification-inner
     (vec (flatten (map #(prepare-word page-width %) words-raw)))
     page-width)
    ))
