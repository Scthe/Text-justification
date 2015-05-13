(ns ^:figwheel-always text-justification.model
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.string :as gstring]))

(enable-console-print!)


;;;; TODO precalc badness to hash ?


;;;;
;;;; consts
;;;;

(defonce invisible-char (gstring/unescapeEntities "&nbsp;")) ;; \u00A0 ?


;;;;
;;;; state
;;;;

(defonce state (atom []))

(defn- set-state    [new-state] (swap! state (fn [] new-state))) ;; TODO just use reset!
(defn- clear-state  [] (set-state []))
(defn- update-state [new-lines] (set-state (concat @state new-lines)))

;;;;
;;;; implementation
;;;;

(defn- badness
  "given line and page width returns measurement how `pretty` the line is"
  [words-lengths page-width]
  (let [total-length (+ (reduce + words-lengths) (count words-lengths) -1)] ;; count whitespaces too
    (if (> total-length page-width)
      js/Infinity
      (let [a (- page-width total-length)] (* a a a)) )))


;; cache results (memo)
;; f.e. result for 'justify text after X word is: [lines]'
(def tj-memo (atom {}))

(defn- clear-memo [] (reset! tj-memo {}))
(defn- add-memo   [arg-val return-val] (swap! tj-memo #(assoc % arg-val return-val)))
(defn- has-memo?  [arg-val] (contains? @tj-memo arg-val)) ;; TODO use let-if instead, to search for value once
(defn- get-memo   [arg-val] (get @tj-memo arg-val))

(defn- tj-inner
  "fits words into lines so that whole text looks pretty
  @return #words in each line"
  ([page-width words]
    (clear-memo)
    (second (tj-inner page-width words 0)))
  ([page-width words word-idx]
    (cond
      (empty? words) [0.0 []]
      (has-memo? word-idx) (get-memo word-idx)
      :else
        (reduce
          (fn [acc newline-placement-idx]
             (let [line (subvec words 0 newline-placement-idx)
                   rest-of-text (subvec words newline-placement-idx)
                   newline-word-idx (+ word-idx newline-placement-idx)
                   [sub-prob-ugliness sub-prob-lines :as sub-prob-solution]
                            (tj-inner page-width rest-of-text newline-word-idx)
                   ugliness (+ (badness line page-width) sub-prob-ugliness)
                   solution-proposal (cons newline-placement-idx sub-prob-lines)]
              ; (print "sub" line "|words in line" (count words))
              (add-memo newline-word-idx sub-prob-solution)
              (if (< ugliness (first acc)) [ugliness solution-proposal] acc)))
          [js/Infinity []]
          (map inc (range (count words))) )))) ;; TODO (dec (count words)) ?


(defn- split-word
  "Split words that have length > max-len"
  [max-len word]
  (let [len (count word)]
    (if (<= len max-len)
      [word]
      (concat
        [(.substring word 0 max-len)]
        (split-word max-len (str " -" (.substring word max-len))) ))))

(defn- prepare-paragraph
  [paragraph max-chars-in-line]
  (-> paragraph
         gstring/trim
         (.split #" +")
         (->> (mapv #(split-word max-chars-in-line %))
              flatten
              vec)))

(defn- prepare-text
  "Prepare whole text for justification.
   1) split by paragraphs / treat text as single text flow
   2) trim words
   @return vector of lines, where each line is vector of words"
  [text separate-paragraphs max-chars-in-line]
  (let [paragraphs (if separate-paragraphs
                        (seq (.split text "\n"))
                        [(clojure.string/replace text #"\n" " ")] )]
      (mapv #(prepare-paragraph % max-chars-in-line) paragraphs) ))


;;;;
;;;; public interface
;;;;

(defn text-justification
  "justify text provided as a collection of words to given page width"
  [text separate-paragraphs page-width]
  {:pre [(> page-width 0)]}
  
  (clear-state)
  (.profile js.console "text-justification")
  (doseq [words-in-paragraph (prepare-text text separate-paragraphs page-width)]
    ; (println "PARAGRAPH:" words-in-paragraph)
    (let [words-lengths (mapv count words-in-paragraph)
          justified-text (tj-inner page-width words-lengths)]
       ; (.log js/console "words" (count words) ":" words)
       ; (.log js/console "state" (count justified-text) ":" justified-text)
       (update-state
        (second (reduce
          (fn [[start-word-idx lines-atm] line-words-count]
            (let [next-word-start-idx (+ start-word-idx line-words-count)
                  words-in-this-line (subvec words-in-paragraph start-word-idx next-word-start-idx)]
              ; (println words-in-this-line)
              [next-word-start-idx (conj lines-atm words-in-this-line)] ))
          [0 []]
          justified-text))) ))
  (.profileEnd js.console)
  )

